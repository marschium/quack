#[macro_use] extern crate derive_new;
extern crate nom;
#[macro_use] extern crate log;

use std::marker::PhantomData;

pub mod compile;
pub mod parse;
pub mod vm;
pub mod builtin;

use vm::{QuackException, Callback};

#[derive(Debug, Copy, Clone)]
pub enum ReturnValue {
    None,
    Bool(bool),
    Num(f64)
}

impl From<bool> for ReturnValue {
    fn from(val: bool) -> Self {
        ReturnValue::Bool(val)
    }
}

impl From<f64> for ReturnValue {
    fn from(val: f64) -> Self {
        ReturnValue::Num(val)
    }
}

impl From<()> for ReturnValue {
    fn from(val: ()) -> Self {
        ReturnValue::None
    }
}

pub enum ParamValue {
    None,
    Bool(bool),
    Num(f64)
}

impl From<ParamValue> for () {
    fn from(val: ParamValue) -> Self {
        ()
    }
}

impl From<ParamValue> for bool {
    fn from(val: ParamValue) -> Self {
        match val {
            ParamValue::Bool(true) => {
                true
            },
            _ => {
                false
            }
        } 
    }
}

impl From<ParamValue> for f64 {
    fn from(val: ParamValue) -> Self {
        match val {
            ParamValue::Num(x) => {
                x
            }
            _ => {
                0.0 // TODO this seems wrong/landmine/dangerous? 
            }
        }
    }
}

pub trait IntoTypeInfo {
    fn as_type_info() -> Option<compile::TypeInfo>;
}

impl IntoTypeInfo for f64 {
    fn as_type_info() -> Option<compile::TypeInfo> {
        Some(compile::TypeInfo::Num)
    }
}

impl IntoTypeInfo for bool {
    fn as_type_info() -> Option<compile::TypeInfo> {
        Some(compile::TypeInfo::Bool)
    }
}

impl IntoTypeInfo for () {
    fn as_type_info() -> Option<compile::TypeInfo> {
        None
    }
}

fn into_type_info<T: IntoTypeInfo>() -> Option<compile::TypeInfo> {
    T::as_type_info()
}

pub struct Quack<'a> {
    compiler: compile::Compiler,
    vm: vm::QuackVM<'a>,
    start_address: Option<usize>,
    instructions: Option<Vec::<vm::Instruction>>
}

impl<'a> Quack<'a> {
    pub fn new() -> Quack<'a> {
        let mut vm = vm::QuackVM::new();
        let mut compiler_context = compile::CompilerContext::new();

        for (f, name, ret_type) in builtin::get_builtins() {
            let id = vm.register(Box::new(f));
            compiler_context.add_builtin(name.into(), compile::BuiltinFunctionInfo::new(id, ret_type));
        }

        let compiler = compile::Compiler::new(compiler_context);
        Quack {
            compiler,
            vm,
            start_address: None,
            instructions: None
        }
    }

    // TODO could probably do some macro magic to auto generate wrappers for any number of args
    pub fn register<T: 'a+ Into<ReturnValue> + IntoTypeInfo>(&mut self, name: String, mut f: Box::<dyn FnMut() -> T + 'a>) {
        let closure = move |_| {
            let r = f();
            match r.into() {
                ReturnValue::Bool(b) => Ok(Some(vm::QuackPrimitive::Bool(b))),
                ReturnValue::Num(f) => Ok(Some(vm::QuackPrimitive::Num(f))),
                ReturnValue::None => Ok(None)
            }
        };
        let id = self.vm.register(Box::new(closure));
        self.compiler.context().add_builtin(name.into(), compile::BuiltinFunctionInfo::new(id, T::as_type_info()));
    }

    pub fn register1<T: 'a+ Into<ReturnValue>+IntoTypeInfo, K: 'a + From<ParamValue>>(&mut self, name: String, mut f: Box::<dyn FnMut(K) -> T + 'a>) {
        let closure = move |mut params: Vec::<vm::QuackPrimitive>| {
            let p = match params.pop() {
                Some(vm::QuackPrimitive::Num(x)) => {
                    ParamValue::Num(x)
                },
                _ => ParamValue::None
            };
            let r = f(p.into());
            match r.into() {
                ReturnValue::Bool(b) => Ok(Some(vm::QuackPrimitive::Bool(b))),
                ReturnValue::Num(f) => Ok(Some(vm::QuackPrimitive::Num(f))),
                ReturnValue::None => Ok(None)
            }
        };
        let id = self.vm.register(Box::new(closure));
        self.compiler.context().add_builtin(name.into(), compile::BuiltinFunctionInfo::new(id, T::as_type_info()));
    }

    // TODO Rewrite as a macro?
    pub fn register2<T: 'a+ Into<ReturnValue> + IntoTypeInfo, K: 'a + From<ParamValue>>(&mut self, name: String, mut f: Box::<dyn FnMut(K, K) -> T + 'a>) {
        let closure = move |mut params: Vec::<vm::QuackPrimitive>| {
            let a = match params.get(0) {
                Some(vm::QuackPrimitive::Num(x)) => {
                    ParamValue::Num(*x)
                },
                _ => ParamValue::None
            };
            let b = match params.get(1) {
                Some(vm::QuackPrimitive::Num(x)) => {
                    ParamValue::Num(*x)
                },
                _ => ParamValue::None
            };
            let r = f(a.into(), b.into());
            match r.into() {
                ReturnValue::Bool(b) => Ok(Some(vm::QuackPrimitive::Bool(b))),
                ReturnValue::Num(f) => Ok(Some(vm::QuackPrimitive::Num(f))),
                ReturnValue::None => Ok(None)
            }
        };
        let id = self.vm.register(Box::new(closure));
        self.compiler.context().add_builtin(name.into(), compile::BuiltinFunctionInfo::new(id, T::as_type_info()));
    }

    pub fn load(&mut self, program: &str) {
        let mut exprs = parse::parse(program).unwrap();
        let (start_addr, instructions) = self.compiler.compile(&mut exprs.iter()).unwrap();
        self.start_address = Some(start_addr);
        self.instructions = Some(instructions);
    }

    pub fn run(&mut self) {
        if let (Some(a), Some(i)) = (self.start_address, self.instructions.clone()) {
            match self.vm.run(a, i) {
                Err(QuackException::NoMoreInstructions) => {
    
                },
                _=> {
                    println!("something went wrong");
                }
            }
        }
        // TODO something useful if not set        
    }
}