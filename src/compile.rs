use super::parse::QuackExpression;
use super::vm::{Instruction, QuackHandle, QuackPrimitive};
use std::collections::HashMap;

#[derive(Debug, PartialEq, PartialOrd)]
pub enum CompilerError {
    General(&'static str),
    StructNotDefined(&'static str),
}

#[derive(new, Clone)]
struct FieldInfo {
    name: String,
    field_type: TypeInfo,
}

struct StructDef {
    name: String,
    fields: Vec<FieldInfo>,
}

#[derive(Debug, Clone)]
pub enum TypeInfo {
    Bool,
    Num,
    Struct(String),
}

#[derive(Debug, Clone, new)]
struct LocalVarInfo {
    id: u32,
    #[new(value = "None")]
    underlying_type: Option<TypeInfo>,
}

#[derive(Debug, Clone, new)]
pub struct BuiltinFunctionInfo {
    id: QuackHandle,
    return_type: Option<TypeInfo>,
}

#[derive(Debug, Clone, new)]
pub struct FunctionInfo {
    num_params: usize,
    address: usize
}

// TODO have scoped contexts. have a stack of them and search down the stack to find definitions e.t.c
#[derive(new)]
pub struct CompilerContext {
    #[new(value = "0")]
    local_id: u32,
    #[new(value = "HashMap::new()")]
    locals: HashMap<String, LocalVarInfo>,
    #[new(value = "HashMap::new()")]
    struct_defs: HashMap<String, StructDef>,
    #[new(value = "HashMap::new()")]
    builtin_funcs: HashMap<String, BuiltinFunctionInfo>,
    #[new(value = "HashMap::new()")]
    functions: HashMap<String, FunctionInfo>
}

impl CompilerContext {
    fn get_local(&self, n: &String) -> Option<&LocalVarInfo> {
        self.locals.get(n)
    }

    fn get_or_add_local(&mut self, n: &String) -> Option<LocalVarInfo> {
        match self.locals.get_mut(n) {
            Some(x) => Some(x.clone()),
            None => {
                let inf = LocalVarInfo::new(self.local_id);
                self.locals.insert(n.clone(), inf.clone());
                self.local_id += 1;
                Some(inf)
            }
        }
    }

    fn set_local(&mut self, n: String, v: LocalVarInfo) {
        self.locals.insert(n, v);
    }

    pub fn add_builtin(&mut self, name: String, inf: BuiltinFunctionInfo) {
        self.builtin_funcs.insert(name, inf);
    }

    pub fn add_function(&mut self, name: String, inf: FunctionInfo) {
        self.functions.insert(name, inf);
    }
}

#[derive(new)]
pub struct Compiler {
    context: CompilerContext,
}

impl Compiler {

    pub fn context(&mut self) -> &mut CompilerContext {
        &mut self.context
    }

    pub fn compile(&mut self, exprs: &mut dyn Iterator<Item=&QuackExpression>) -> Result<(usize, Vec<Instruction>), CompilerError> {
        let mut functions = Vec::new();
        for expr in exprs {
            match expr {
                // TODO only struct defs and function defs are allowed on the top level of file
                QuackExpression::FunctionDefinition {name, parameters, inner} => {
                    let function_def = &mut self.function_def(name, parameters, inner)?;
                    let address = functions.len();
                    let num_params = parameters.len();
                    self.context.add_function(name.clone(), FunctionInfo::new(num_params, address));
                    functions.append(function_def);
                },
                QuackExpression::StructDef { name, fields } => {
                    self.struct_def(name, fields)?;
                },
                _ => {}
            }
        }
        let entry = self.context.functions.get("main").ok_or(CompilerError::General("No main function!"))?.address;
        Ok((entry, functions))
    }

    fn compile_inner(&mut self,  exprs: &mut dyn Iterator<Item=&QuackExpression>) -> Result<Vec<Instruction>, CompilerError> {
        // TODO return the start address (could either be first non-def or the address of a 'main' function)
        let mut output = vec![];
        for expr in exprs {
            // TODO some expressions aren't valid as top level expressions
            match expr {
                QuackExpression::NumLiteral(v) => {
                    output.append(&mut self.num_literal(*v)?);
                }
                QuackExpression::Name(n) => {
                    output.append(&mut self.local_var(n)?);
                }
                QuackExpression::AccessStructField { var_name, field } => {
                    output.append(&mut self.struct_field(var_name, field)?);
                }
                QuackExpression::ConditionBranch {condition, inner, expected} => {
                    output.append(&mut self.conditional(condition, inner, *expected)?);
                }
                QuackExpression::Assign { left, right } => {
                    output.append(&mut self.assign(left, right)?);
                }
                // TODO disable struct defs inside functions e.t.c
                QuackExpression::StructDef { name, fields } => {
                    self.struct_def(name, fields)?;
                },
                QuackExpression::CreateStruct {struct_name, fields} => {
                    output.append(&mut self.create_struct(struct_name, fields)?);
                }
                QuackExpression::FunctionCall { name, arguments } => {
                    output.append(&mut self.function_call(name, arguments)?);
                }
                // TODO disable nested functions?
                QuackExpression::FunctionDefinition {name, parameters, inner} => {
                    let function_def = &mut self.function_def(name, parameters, inner)?;
                    let address = output.len();
                    let num_params = parameters.len();
                    self.context.add_function(name.clone(), FunctionInfo::new(num_params, address));
                    output.append(function_def);
                }
            }
        }
        Ok(output)
    }

    fn num_literal(&self, i: f64) -> Result<Vec<Instruction>, CompilerError> {
        Ok(vec![Instruction::Push {
            val: QuackPrimitive::Num(i),
        }])
    }

    fn function_call(&mut self, func_name: &String, params: &[QuackExpression]) -> Result<Vec<Instruction>, CompilerError> {
        let mut ins = vec![];
        if let Some(_) = self.context.builtin_funcs.get(func_name) {
            for p in params.iter().rev() {
                ins.append(&mut self.compile_inner(&mut vec!(p).iter().map(|x| *x))?);
            }
            let func_info = self
                .context
                .builtin_funcs
                .get(func_name)
                .ok_or(CompilerError::General("function does not exist"))?;
            ins.push(Instruction::Push {
                val: QuackPrimitive::BuiltinNumArgs(params.len() as u32)
            });
            ins.push(Instruction::Push {
                val: QuackPrimitive::FunctionId(func_info.id),
            });
            ins.push(Instruction::CallBuiltin);
            Ok(ins)
        }
        else if let Some(_) = self.context.functions.get(func_name) {
            for p in params.iter().rev() {
                ins.append(&mut self.compile_inner(&mut vec!(p).iter().map(|x| *x))?);
            }
            let func_info = self
                .context
                .functions
                .get(func_name)
                .ok_or(CompilerError::General("function does not exist"))?;
            ins.push(Instruction::Push {
                val: QuackPrimitive::InstructionAddress(func_info.address as u32)
            });
            ins.push(Instruction::Call);
            Ok(ins)
        }
        else {
            Err(CompilerError::General("No function with name found"))
        }
    }

    fn local_var(&self, n: &String) -> Result<Vec<Instruction>, CompilerError> {
        let mut ins = vec![];
        match self.context.locals.get(n) {
            Some(vi) => {
                ins.push(Instruction::Push {
                    val: QuackPrimitive::VariableId(vi.id),
                });
                ins.push(Instruction::ReadLocal);
                Ok(ins)
            }
            None => Err(CompilerError::General("No local var with name")),
        }
    }

    fn struct_field(&self, vn: &String, f: &String) -> Result<Vec<Instruction>, CompilerError> {
        let mut ins = vec![];
        match self.context.get_local(vn) {
            Some(inf) => match &inf.underlying_type {
                Some(TypeInfo::Struct(struct_name)) => {
                    let struct_def = self
                        .context
                        .struct_defs
                        .get(struct_name)
                        .ok_or(CompilerError::StructNotDefined("struct not found"))?;
                    let field_id = struct_def
                        .fields
                        .iter()
                        .position(|x| &x.name == f)
                        .ok_or(CompilerError::StructNotDefined("field not in struct"))?;
                    ins.push(Instruction::Push {
                        val: QuackPrimitive::StructField(field_id as u32),
                    });
                    ins.append(&mut self.local_var(vn)?);
                    ins.push(Instruction::ReadField);
                    Ok(ins)
                }
                _ => Err(CompilerError::General("Struct expected!")),
            },
            None => Err(CompilerError::General("Variable does not exist")),
        }
    }

    fn check_or_set_type(
        &mut self,
        var_name: &String,
        var_info: &mut LocalVarInfo,
        expected_type: &TypeInfo,
    ) -> Result<(), CompilerError> {
        let _ = match var_info.underlying_type.as_ref() {
            Some(expected_type) => Ok(()),
            Some(..) => Err(CompilerError::General(
                "Variable is already assigned with different type",
            )),
            None => {
                var_info.underlying_type = Some(expected_type.clone());
                self.context.set_local(var_name.clone(), var_info.clone());
                Ok(())
            }
        }?;
        Ok(())
    }

    fn assign(&mut self, left: &QuackExpression, right: &QuackExpression) -> Result<Vec<Instruction>, CompilerError> {
        let mut ins = vec![];
        if let QuackExpression::Name(l) = left {
            let mut local_var = self
                .context
                .get_or_add_local(&l)
                .ok_or(CompilerError::General("_"))?;
            match right {
                QuackExpression::NumLiteral(r) => {
                    self.check_or_set_type(l, &mut local_var, &TypeInfo::Num)?;
                    ins.append(&mut self.num_literal(*r)?);
                    Ok(())
                }
                QuackExpression::CreateStruct {
                    struct_name,
                    fields,
                } => {
                    self.check_or_set_type(
                        l,
                        &mut local_var,
                        &TypeInfo::Struct(struct_name.clone()),
                    )?;
                    ins.append(&mut self.create_struct(struct_name, fields)?);
                    Ok(())
                }
                QuackExpression::AccessStructField { var_name, field } => {
                    // TODO one day to support nested access
                    let field =
                        match self.context.get_local(var_name) {
                            Some(LocalVarInfo {
                                underlying_type: Some(TypeInfo::Struct(struct_name)),
                                id,
                            }) => {
                                let struct_type = self.context.struct_defs.get(struct_name).ok_or(
                                    CompilerError::StructNotDefined("no mathcing struct!"),
                                )?;
                                Ok(struct_type.fields.iter().find(|x| &x.name == field).ok_or(
                                    CompilerError::General("Field does not exist in struct"),
                                )?)
                            }
                            _ => Err(CompilerError::General(
                                "Variable is not a struct or does not exist",
                            )),
                        }?
                        .clone();
                    self.check_or_set_type(l, &mut local_var, &field.field_type)?;
                    ins.append(&mut self.struct_field(var_name, &field.name)?);
                    Ok(())
                }
                // TODO function call (will need information about the return type of the function)
                QuackExpression::FunctionCall { name, arguments } => {
                    let function_info = self
                        .context
                        .builtin_funcs
                        .get(name)
                        .ok_or(CompilerError::General("No function with name"))?;
                    let func_return_type = function_info
                        .return_type
                        .as_ref()
                        .ok_or(CompilerError::General("Function does not return anything!"))?;
                    self.check_or_set_type(l, &mut local_var, &func_return_type.clone())?;
                    ins.append(&mut self.function_call(name, arguments)?);
                    Ok(())
                }
                _ => Err(CompilerError::General("Not implemented!")),
            }?;
            ins.push(Instruction::Push {
                val: QuackPrimitive::VariableId(local_var.id),
            });
            ins.push(Instruction::SetLocal);
            Ok(ins)
        } else {
            Err(CompilerError::General(
                "left side of operation must be variable",
            ))
        }
    }

    fn struct_def(&mut self, name: &String, fields: &Vec<(String, String)>) -> Result<(), CompilerError> {
        fn type_name_to_info(n: &String) -> TypeInfo {
            match n.as_ref() {
                "num" => TypeInfo::Num,
                _ => TypeInfo::Struct(n.clone()),
            }
        }

        let def = StructDef {
            name: name.clone(),
            fields: fields
                .iter()
                .map(|(n, t)| FieldInfo::new(n.clone(), type_name_to_info(t)))
                .collect(),
        };
        self.context.struct_defs.insert(name.clone(), def);
        Ok(())
    }

    fn create_struct(&mut self, name: &String, fields: &[QuackExpression]) -> Result<Vec<Instruction>, CompilerError> {
        let mut ins = vec![];
        match self.context.struct_defs.get(name) {
            Some(def) => {
                if fields.len() != def.fields.len() {
                    Err(CompilerError::General("Incorrect number of arguments"))
                } else {
                    for f in fields.iter().rev() {
                        ins.append(&mut self.compile_inner(&mut vec!(f).iter().map(|x| *x))?);
                    }
                    ins.push(Instruction::Push {
                        val: QuackPrimitive::StructNumFields(fields.len() as u32),
                    });
                    ins.push(Instruction::CreateStruct);
                    Ok(ins)
                }
            }
            _ => Err(CompilerError::StructNotDefined("Struct not found")),
        }
    }

    fn conditional(&mut self, condition: &QuackExpression, inner: &Vec<QuackExpression>, expected: bool) -> Result<Vec<Instruction>, CompilerError> {
        let mut ins = match condition {
            QuackExpression::FunctionCall { name, arguments } => {
                Ok(self.function_call(name, arguments)?)
            }
            _ => Err(CompilerError::General(
                "Conditions only supported for functions!",
            )),
        }?;

        // TODO one day the inner section should have its own scope
        let mut inner_instructions = vec!();
        for i in inner {
            inner_instructions.append(&mut self.compile_inner(&mut vec!(i).iter().map(|x| *x))?);
        }
        ins.push(Instruction::Push {
            val: QuackPrimitive::Bool(expected),
        });
        ins.push(Instruction::Push {
            val: QuackPrimitive::InstructionOffset(inner_instructions.len() as i32),
        });
        ins.push(Instruction::JumpNotEq);
        ins.append(&mut inner_instructions);
        Ok(ins)
    }

    fn function_def(&mut self, name: &String, parameters: &Vec<(String, String)>, inner: &Vec<QuackExpression>) -> Result<Vec<Instruction>, CompilerError> {
        // TODO if main replace stack pop with exit
        let mut ins = vec![];

        for (param_name, param_type) in parameters {
            let mut local_var = self
                .context
                .get_or_add_local(&param_name)
                .ok_or(CompilerError::General("_"))?;
            self.check_or_set_type(param_name, &mut local_var, &TypeInfo::Num)?; // TODO convert param_type to TypeInfo
            ins.push(Instruction::Push {
                val: QuackPrimitive::VariableId(local_var.id),
            });
            ins.push(Instruction::SetLocal);
        }

        let mut inner_instructions = vec!();
        for i in inner {
            inner_instructions.append(&mut self.compile_inner(&mut vec!(i).iter().map(|x| *x))?);
        }
        ins.append(&mut inner_instructions);
        if name == "main" {
            ins.push(Instruction::Exit)
        }
        else {
            ins.push(Instruction::Return);
        }
        Ok(ins)
    }
}

mod test {
    use super::*;

    #[test]
    fn test_compile_single_op() {
        let e = QuackExpression::Assign {
            left: Box::new(QuackExpression::Name("a".to_owned())),
            right: Box::new(QuackExpression::NumLiteral(6.6)),
        };

        let mut compiler = Compiler::new(CompilerContext::new());
        let res = compiler.compile_inner(&mut vec![e].iter()).unwrap();
        let expected = vec![
            Instruction::Push {
                val: QuackPrimitive::Num(6.6),
            },
            Instruction::Push {
                val: QuackPrimitive::VariableId(0),
            },
            Instruction::SetLocal,
        ];
        assert_eq!(res, expected);
    }

    #[test]
    fn test_compile_multi_set_local() {
        let e1 = QuackExpression::Assign {
            left: Box::new(QuackExpression::Name("a".to_owned())),
            right: Box::new(QuackExpression::NumLiteral(6.6)),
        };
        let e2 = QuackExpression::Assign {
            left: Box::new(QuackExpression::Name("b".to_owned())),
            right: Box::new(QuackExpression::NumLiteral(7.6)),
        };

        let mut compiler = Compiler::new(CompilerContext::new());
        let res = compiler.compile_inner(&mut vec![e1, e2].iter()).unwrap();
        let expected = vec![
            Instruction::Push {
                val: QuackPrimitive::Num(6.6),
            },
            Instruction::Push {
                val: QuackPrimitive::VariableId(0),
            },
            Instruction::SetLocal,
            Instruction::Push {
                val: QuackPrimitive::Num(7.6),
            },
            Instruction::Push {
                val: QuackPrimitive::VariableId(1),
            },
            Instruction::SetLocal,
        ];
        assert_eq!(res, expected);
    }

    #[test]
    fn test_compile_new_struct() {
        let create_struct = QuackExpression::Assign {
            left: Box::new(QuackExpression::Name("a".to_owned())),
            right: Box::new(QuackExpression::CreateStruct {
                struct_name: "test".to_owned(),
                fields: vec![
                    QuackExpression::NumLiteral(1.0),
                    QuackExpression::NumLiteral(2.0),
                    QuackExpression::NumLiteral(3.0),
                ],
            }),
        };

        let struct_def = QuackExpression::StructDef {
            name: "test".to_owned(),
            fields: vec![
                ("a".to_owned(), "num".to_owned()),
                ("b".to_owned(), "num".to_owned()),
                ("c".to_owned(), "num".to_owned()),
            ],
        };

        let mut compiler = Compiler::new(CompilerContext::new());
        let res = compiler.compile_inner(&mut vec![struct_def, create_struct].iter()).unwrap();

        let expected = vec![
            // push each value to stack (front value should be popped first)
            Instruction::Push {
                val: QuackPrimitive::Num(3.0),
            },
            Instruction::Push {
                val: QuackPrimitive::Num(2.0),
            },
            Instruction::Push {
                val: QuackPrimitive::Num(1.0),
            },
            // push num of field to stack
            Instruction::Push {
                val: QuackPrimitive::StructNumFields(3),
            },
            // call create struct
            Instruction::CreateStruct,
            // assign variable
            Instruction::Push {
                val: QuackPrimitive::VariableId(0),
            },
            Instruction::SetLocal,
        ];
        assert_eq!(res, expected);
    }

    #[test]
    fn test_compile_assign_from_struct_field() {
        let assign_expr = QuackExpression::Assign {
            left: Box::new(QuackExpression::Name("b".to_owned())),
            right: Box::new(QuackExpression::AccessStructField {
                var_name: "a".to_owned(),
                field: "c".to_owned(),
            }),
        };

        let create_struct = QuackExpression::Assign {
            left: Box::new(QuackExpression::Name("a".to_owned())),
            right: Box::new(QuackExpression::CreateStruct {
                struct_name: "test".to_owned(),
                fields: vec![
                    QuackExpression::NumLiteral(1.0),
                    QuackExpression::NumLiteral(2.0),
                    QuackExpression::NumLiteral(3.0),
                ],
            }),
        };

        let struct_def = QuackExpression::StructDef {
            name: "test".to_owned(),
            fields: vec![
                ("a".to_owned(), "num".to_owned()),
                ("b".to_owned(), "num".to_owned()),
                ("c".to_owned(), "num".to_owned()),
            ],
        };

        let mut compiler = Compiler::new(CompilerContext::new());
        let _ = compiler.compile_inner(&mut vec![struct_def, create_struct].iter()); // TODO return context or something
        let res = compiler.compile_inner(&mut vec![assign_expr].iter()).unwrap();
        let expected = vec![
            Instruction::Push {
                val: QuackPrimitive::StructField(2),
            },
            Instruction::Push {
                val: QuackPrimitive::VariableId(0),
            }, // struct handle should be top of the stack
            Instruction::ReadLocal,
            Instruction::ReadField,
            Instruction::Push {
                val: QuackPrimitive::VariableId(1),
            },
            Instruction::SetLocal,
        ];
        assert_eq!(res, expected);
    }

    #[test]
    fn test_compile_builtin_function_call() {
        // before passing control to func, stack should be
        // <id of function at top>
        // <1st arg>
        // <2nd arg>

        let func_expr = QuackExpression::FunctionCall {
            name: "test".to_owned(),
            arguments: vec![
                QuackExpression::NumLiteral(3.0),
                QuackExpression::NumLiteral(1.0),
                QuackExpression::Name("a".to_owned()),
            ],
        };

        let mut context = CompilerContext::new();
        context
            .builtin_funcs
            .insert("test".to_owned(), BuiltinFunctionInfo::new(0, None));
        context.set_local("a".to_owned(), LocalVarInfo::new(0));
        let mut compiler = Compiler::new(context);

        let res = compiler.compile_inner(&mut vec![func_expr].iter()).unwrap();

        let expected = vec![
            Instruction::Push {
                val: QuackPrimitive::VariableId(0),
            },
            Instruction::ReadLocal,
            Instruction::Push { val: QuackPrimitive::Num(1.0) }, // second arg
            Instruction::Push { val: QuackPrimitive::Num(3.0) }, // first arg
            Instruction::Push {
                val: QuackPrimitive::BuiltinNumArgs(3)
            },
            Instruction::Push {
                val: QuackPrimitive::FunctionId(0),
            }, // function id
            Instruction::CallBuiltin,
        ];

        assert_eq!(res, expected);
    }

    #[test]
    fn test_assign_from_function() {
        let assign_expr = QuackExpression::Assign {
            left: Box::new(QuackExpression::Name("a".to_owned())),
            right: Box::new(QuackExpression::FunctionCall {
                name: "test".to_owned(),
                arguments: vec![],
            }),
        };

        let mut context = CompilerContext::new();
        context
            .builtin_funcs
            .insert("test".to_owned(), BuiltinFunctionInfo::new(0, Some(TypeInfo::Num)));
        let mut compiler = Compiler::new(context);

        let res = compiler.compile_inner(&mut vec![assign_expr].iter()).unwrap();

        let expected = vec![
            Instruction::Push {
                val: QuackPrimitive::FunctionId(0),
            }, // function id
            Instruction::CallBuiltin,
            Instruction::Push {
                val: QuackPrimitive::VariableId(0),
            },
            Instruction::SetLocal,
        ];

        assert_eq!(res, expected);
    }

    #[test]
    fn test_conditional() {
        let condition_expr = QuackExpression::ConditionBranch {
            condition: Box::new(QuackExpression::FunctionCall {
                name: "equal".to_owned(),
                arguments: vec![
                    QuackExpression::Name("a".to_owned()),
                    QuackExpression::NumLiteral(1.0),
                ],
            }),
            inner: vec![QuackExpression::Assign {
                left: Box::new(QuackExpression::Name("a".to_owned())),
                right: Box::new(QuackExpression::NumLiteral(2.2)),
            }],
            expected: true,
        };

        let mut context = CompilerContext::new();
        context.builtin_funcs.insert(
            "equal".to_owned(),
            BuiltinFunctionInfo::new(0, Some(TypeInfo::Bool)),
        );
        let mut a_var_info = LocalVarInfo::new(0);
        a_var_info.underlying_type = Some(TypeInfo::Bool);
        context.set_local("a".to_owned(), a_var_info);
        let mut compiler = Compiler::new(context);
        let res = compiler.compile_inner(&mut vec![condition_expr].iter()).unwrap();

        let expected = vec![
            Instruction::Push {
                val: QuackPrimitive::Num(1.0),
            },
            Instruction::Push {
                val: QuackPrimitive::VariableId(0),
            },
            Instruction::ReadLocal,
            Instruction::Push{
                val: QuackPrimitive::BuiltinNumArgs(2)
            },
            Instruction::Push {
                val: QuackPrimitive::FunctionId(0),
            },
            Instruction::CallBuiltin,
            Instruction::Push {
                val: QuackPrimitive::Bool(true),
            },
            Instruction::Push {
                val: QuackPrimitive::InstructionOffset(3),
            },
            Instruction::JumpNotEq,
            Instruction::Push {
                val: QuackPrimitive::Num(2.2),
            },
            Instruction::Push {
                val: QuackPrimitive::VariableId(0),
            },
            Instruction::SetLocal,
        ];

        assert_eq!(res, expected);
    }

    #[test]
    fn test_function_def() {
        // read arguments from stack into local storage
        // instructions for function
        // write result to the stack
        // write the return address to stack (address should be stored on frame)
        // pop stack frame
        // jump to return address

        let expr = QuackExpression::FunctionDefinition {
            name: "main".to_string(),
            parameters: vec![("a".to_string(), "num".to_string())],
            inner: vec![QuackExpression::Assign {
                left: Box::new(QuackExpression::Name("b".to_owned())),
                right: Box::new(QuackExpression::NumLiteral(6.6)),
            }],
        };

        let expected = vec![
            Instruction::Push {
                val: QuackPrimitive::VariableId(0),
            },
            Instruction::SetLocal,
            Instruction::Push {
                val: QuackPrimitive::Num(6.6),
            },
            Instruction::Push {
                val: QuackPrimitive::VariableId(1),
            },
            Instruction::SetLocal,
            Instruction::Return,
        ];

        let mut compiler = Compiler::new(CompilerContext::new());
        let (_, res) = compiler.compile(&mut vec![expr].iter()).unwrap();

        assert_eq!(res, expected);
    }

    #[test]
    fn test_function_call() {
        // for now just have all functions defined in order <- addresses can then be know up front

        // caller creates a new stack frame
        // push arguments onto the stack
        // create new locals from arguments on the stack
        // jump to the function
        //  function writes return values onto the stack

        let expr = vec!(
            QuackExpression::FunctionDefinition {
                name: "ignore".to_string(),
                parameters: vec![("a".to_string(), "num".to_string())],
                inner: vec![QuackExpression::Assign {
                    left: Box::new(QuackExpression::Name("a".to_owned())),
                    right: Box::new(QuackExpression::NumLiteral(6.6)),
                }],
            },
            QuackExpression::FunctionDefinition {
                name: "something".to_string(),
                parameters: vec![("b".to_string(), "num".to_string())],
                inner: vec![QuackExpression::Assign {
                    left: Box::new(QuackExpression::Name("b".to_owned())),
                    right: Box::new(QuackExpression::NumLiteral(6.6)),
                }],
            },
            QuackExpression::FunctionCall {
                name: "something".to_owned(),
                arguments: vec![
                    QuackExpression::NumLiteral(1.0)
                ],
            },
        );

        let expected = vec!(    
            // function ignore
            Instruction::Push { val: QuackPrimitive::VariableId(0) },
            Instruction::SetLocal,
            Instruction::Push { val: QuackPrimitive::Num(6.6) },
            Instruction::Push { val: QuackPrimitive::VariableId(0) },
            Instruction::SetLocal,
            Instruction::Return,
            // function something
            Instruction::Push {val: QuackPrimitive::VariableId(1),},
            Instruction::SetLocal,
            Instruction::Push { val: QuackPrimitive::Num(6.6) },
            Instruction::Push { val: QuackPrimitive::VariableId(1) },
            Instruction::SetLocal,
            Instruction::Return,
            // end of functions
            Instruction::Push {val: QuackPrimitive::Num(1.0)},
            Instruction::Push {val: QuackPrimitive::InstructionAddress(6)},
            Instruction::Call,
        );

        let mut compiler = Compiler::new(CompilerContext::new());
        let res = compiler.compile_inner(&mut expr.iter()).unwrap();

        assert_eq!(res, expected);
    }
}
