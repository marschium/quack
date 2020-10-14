use std::collections::HashMap;

// TODO might have to change to a more byte like representation to allow compilation
#[derive(PartialEq, PartialOrd, Debug, Copy, Clone)]
pub enum Instruction {
    Push{val: QuackPrimitive},
    SetLocal,
    ReadLocal,
    CreateStruct,
    ReadField,
    WriteField,
    CallBuiltin,
    JumpEq,
    JumpNotEq,
    JumpAbs,
    Call,
    Return,
    Exit
    // TODO delete locals and structs
}

#[derive(PartialEq, PartialOrd, Debug, Copy, Clone)]
pub enum QuackException{
    EmptyStack,
    StructNotFound,
    InvalidOperation,
    NoMoreInstructions,
    General(&'static str)
}

pub type QuackHandle = u32;

// The base representations of all things
#[derive(PartialEq, PartialOrd, Debug, Clone, Copy)]
pub enum QuackPrimitive {
    Bool(bool),
    Num(f64),
    StructHandle(QuackHandle),
    StructField(QuackHandle),
    StructNumFields(u32),
    VariableId(QuackHandle),
    FunctionId(QuackHandle),
    BuiltinNumArgs(u32),
    InstructionOffset(i32),
    InstructionAddress(u32),
}

#[derive(new, Debug, PartialEq)]
struct QuackStackFrame {
    return_address: usize,
    #[new(value = "HashMap::new()")]
    locals: HashMap::<QuackHandle, QuackPrimitive>,
}

impl QuackStackFrame {

    pub fn get_local(&mut self, id: u32) -> Option<&QuackPrimitive> {
        self.locals.get(&id)
    }

    pub fn set_local(&mut self, id: u32, val: QuackPrimitive) {
        self.locals.insert(id, val);
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct QuackStructDef {
    name: String,
    num_fields: u32
}

pub type Callback = dyn Fn(Vec::<QuackPrimitive>) -> Result<Option<QuackPrimitive>, QuackException>;

#[derive(new)]
pub struct QuackVM<'a> {
    #[new(value = "0")]
    pc: usize,
    #[new(value = "vec![QuackStackFrame::new(0)]")]
    frames: Vec::<QuackStackFrame>,
    #[new(value = "0")]
    current_stack_frame: usize,
    #[new(value = "Vec::new()")]
    stack: Vec::<QuackPrimitive>,
    #[new(value = "HashMap::new()")]
    structs: HashMap::<QuackHandle, QuackStruct>,
    #[new(value = "HashMap::new()")]
    functions: HashMap::<QuackHandle, Box<dyn FnMut(Vec::<QuackPrimitive>) -> Result<Option<QuackPrimitive>, QuackException> + 'a>>,
}

#[derive(new, Debug, PartialEq)]
struct QuackStruct {
    #[new(value = "HashMap::new()")]
    items: HashMap::<QuackHandle, QuackPrimitive>
}

impl<'a> QuackVM<'a> {     
    
    fn push(&mut self, i: QuackPrimitive) {
        self.stack.push(i);
    }

    fn pop(&mut self) -> Result<QuackPrimitive,QuackException> {
        match self.stack.pop() {
            Some(i) => Ok(i),
            none => Err(QuackException::EmptyStack)
        }
    }

    fn current_frame(&mut self) -> &mut QuackStackFrame {
        &mut self.frames[self.current_stack_frame]
    }   

    fn push_frame(&mut self, addr: usize) {
        self.frames.push(QuackStackFrame::new(addr));
        self.current_stack_frame += 1;
    }

    fn pop_frame(&mut self) {
        self.frames.pop();
        self.current_stack_frame -= 1;
    }

    pub fn register(&mut self, f:  Box<dyn FnMut(Vec::<QuackPrimitive>) -> Result<Option<QuackPrimitive>, QuackException> + 'a>) -> QuackHandle {
        let i = self.functions.len() as u32;
        self.functions.insert(i, f);
        i
    }

    pub fn run(&mut self, start_addr: usize, instructions: Vec::<Instruction>) -> Result<(), QuackException> {
        self.pc = start_addr;
        while let new_pc = self.step(*instructions.get(self.pc).ok_or(QuackException::NoMoreInstructions)?)? {
            self.pc = new_pc
        }
        Ok(())
    }

    fn step(&mut self, ins: Instruction) -> Result<(usize), QuackException> {
        let mut pc =  self.pc + 1;
        match ins {
            Instruction::Push{val} => {
                self.push(val);
                Ok(pc)
            },
            Instruction::SetLocal => {
                if let QuackPrimitive::VariableId(val_id) = self.pop()? {
                    let val = self.pop()?;
                    self.current_frame().set_local(val_id, val);
                    Ok(pc)
                }
                else{
                    Err(QuackException::InvalidOperation)
                }
            },
            Instruction::ReadLocal => {
                if let QuackPrimitive::VariableId(val_id) = self.pop()? {
                    let val = self.current_frame().get_local(val_id).ok_or(QuackException::InvalidOperation)?.clone();
                    self.push(val);
                    Ok(pc)
                }
                else{
                    Err(QuackException::InvalidOperation)
                }
            },
            Instruction::ReadField => {
                if let QuackPrimitive::StructHandle(handle) = self.pop()? {
                    if let QuackPrimitive::StructField(field) = self.pop()? {
                        let v = self.structs.get(&handle).and_then(|s| s.items.get(&field)).ok_or(QuackException::InvalidOperation)?.clone();
                        self.push(v);
                        Ok(pc)
                    }
                    else {
                        Err(QuackException::InvalidOperation)
                    }
                }
                else {
                    Err(QuackException::InvalidOperation)
                }
            },
            Instruction::CreateStruct => {
                if let QuackPrimitive::StructNumFields(n) = self.pop()? {
                    let mut s = QuackStruct::new();
                    for h in 0..n {
                        let v = self.pop()?;
                        s.items.insert(h, v);
                    }
                    let id = self.structs.len() as u32;
                    self.structs.insert(id, s);
                    self.push(QuackPrimitive::StructHandle(id));
                }
                Ok(pc)
            }
            Instruction::WriteField => {
                if let QuackPrimitive::StructHandle(handle) = self.pop()? {
                    if let QuackPrimitive::StructField(field) = self.pop()? {
                        let v = self.pop()?;
                        match self.structs.get_mut(&handle){
                            Some(s) => {
                                s.items.insert(field, v);
                                Ok(pc)
                            },
                            _ => {
                                Err(QuackException::InvalidOperation)
                            }
                        }
                    }
                    else {
                        Err(QuackException::InvalidOperation)
                    }
                }
                else {
                    Err(QuackException::InvalidOperation)
                }
            },
            Instruction::CallBuiltin => {
                if let QuackPrimitive::FunctionId(i) = self.pop()? {
                    if let QuackPrimitive::BuiltinNumArgs(na) = self.pop()? {
                        let f = match self.functions.get_mut(&i) { 
                            Some(f) =>  Ok(f),
                            None => Err(QuackException::InvalidOperation)
                        }?;
                        let mut params = vec!();
                        for i in 0..na {
                            params.push(self.stack.pop().ok_or(QuackException::EmptyStack)?);
                        }
                        if let Some(r) = f(params)? {
                            self.stack.push(r)
                        }
                        Ok(pc) 
                    }
                    else {
                        Err(QuackException::InvalidOperation)
                    }
                    
                }
                else {
                    Err(QuackException::InvalidOperation)
                }
            },
            Instruction::JumpEq => {
                if let QuackPrimitive::InstructionOffset(offset) = self.pop()? {
                    let a = self.pop()?; // TODO throw if not bool
                    let b = self.pop()?; // TODO throw if not bool
                    if a == b {
                        pc += offset as usize;
                    }
                    Ok(pc)
                }
                else {
                    Err(QuackException::General("Expected an offset"))
                }
            },
            Instruction::JumpNotEq => {
                if let QuackPrimitive::InstructionOffset(offset) = self.pop()? {
                    let a = self.pop()?;// TODO throw if not bool
                    let b = self.pop()?; // TODO throw if not bool
                    if a != b {
                        pc += offset as usize;
                    }
                    Ok(pc)
                }
                else {
                    Err(QuackException::General("Expected an offset"))
                }
            },
            Instruction::JumpAbs => {
                if let QuackPrimitive::InstructionAddress(address) = self.pop()? {
                    pc = address as usize;
                    Ok(pc)
                }
                else {
                    Err(QuackException::General("Expected an address"))
                }
            },
            Instruction::Call => {
                if let QuackPrimitive::InstructionAddress(func_addr) = self.pop()? {
                    self.push_frame(pc);
                    Ok(func_addr as usize)
                }         
                else {
                    Err(QuackException::InvalidOperation)
                }    
            },
            Instruction::Return => {
                let r = self.current_frame().return_address;
                self.pop_frame();
                Ok(r)
            },
            Instruction::Exit => {
                Err(QuackException::NoMoreInstructions)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_push_pop() {
        let mut vm = QuackVM::new();
        vm.push(QuackPrimitive::Num(666.0));
        assert_eq!(vm.pop(), Ok(QuackPrimitive::Num(666.0)))
    }

    #[test]
    fn test_local_var() {
        let mut vm = QuackVM::new();
        let mut ins = vec!
        [Instruction::Push{val: QuackPrimitive::Num(3.0)},
         Instruction::Push{val: QuackPrimitive::VariableId(0)},
         Instruction::SetLocal];
        for i in ins {
            vm.step(i);
        }
        let mut expected = HashMap::new();
        expected.insert(0, QuackPrimitive::Num(3.0));

        assert_eq!(vm.current_frame().locals, expected);

        let mut ins = vec!
        [ Instruction::Push{val: QuackPrimitive::VariableId(0)},
         Instruction::ReadLocal];
        for i in ins {
            vm.step(i);
        }

        assert_eq!(vm.stack, vec![QuackPrimitive::Num(3.0)]);
    }

    #[test]
    fn test_create_struct() {
        let mut vm = QuackVM::new();
        let sd = QuackStructDef {
            name: "Test".to_owned(),
            num_fields: 1
        };
        for i in vec!
        [Instruction::Push{val: QuackPrimitive::Num(9.9)},
         Instruction::Push{val: QuackPrimitive::StructNumFields(1)},
         Instruction::CreateStruct] {
             vm.step(i);
         }
        
        let mut expected_struct = QuackStruct::new();
        expected_struct.items.insert(0, QuackPrimitive::Num(9.9));
        let mut expected = HashMap::new();
        expected.insert(0, expected_struct);
        assert_eq!(vm.stack, vec!(QuackPrimitive::StructHandle(0)));
        assert_eq!(vm.structs, expected);
    }

    #[test]
    fn test_call_builtin() {

        use std::sync::mpsc::channel;
        let (send, recv) = channel();
        let closure = move |_: _| { send.send(true).unwrap(); Ok(None) } ;

        let mut vm = QuackVM::new();
        let id =  vm.register(Box::new(closure));

        vm.step(Instruction::Push{val: QuackPrimitive::FunctionId(id)});
        vm.step(Instruction::CallBuiltin);

        assert_eq!(recv.recv().unwrap(), true);
    }

    #[test]
    fn test_jump_match() {
        let mut vm = QuackVM::new();

        let ins = vec!(
            Instruction::Push{val: QuackPrimitive::Bool(true)},
            Instruction::Push{val: QuackPrimitive::Bool(true)},
            Instruction::Push{val: QuackPrimitive::InstructionOffset(1)},
            Instruction::JumpEq,
            Instruction::Push{val: QuackPrimitive::Num(4.4)},
            Instruction::Push{val: QuackPrimitive::Num(6.66)},
        );

        vm.run(0, ins);

        assert_eq!(vm.stack, vec!(QuackPrimitive::Num(6.66)));
    }

    #[test]
    fn test_jump_no_match() {
        let mut vm = QuackVM::new();

        let ins = vec!(
            Instruction::Push{val: QuackPrimitive::Bool(true)},
            Instruction::Push{val: QuackPrimitive::Bool(false)},
            Instruction::Push{val: QuackPrimitive::InstructionOffset(1)},
            Instruction::JumpEq,
            Instruction::Push{val: QuackPrimitive::Num(4.4)},
            Instruction::Push{val: QuackPrimitive::Num(6.66)},
        );

        vm.run(0, ins);

        assert_eq!(vm.stack, vec!(QuackPrimitive::Num(4.4), QuackPrimitive::Num(6.66)));
    }
}