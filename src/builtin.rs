use super::vm::{QuackVM, QuackPrimitive, QuackException, Callback};
use super::compile::TypeInfo;
use std::collections::HashMap;

fn add(mut params: Vec::<QuackPrimitive>) -> Result<Option<QuackPrimitive>, QuackException> {
    let first = params.drain(0..1).next().ok_or(QuackException::EmptyStack)?;
    let second = params.drain(0..1).next().ok_or(QuackException::EmptyStack)?;

    if let (QuackPrimitive::Num(a), QuackPrimitive::Num(b)) = (first, second) { 
        Ok(Some(QuackPrimitive::Num(a + b)))
    } 
    else {
        Err(QuackException::General("Expected two numbers!"))
    } 
}

fn sub(mut params: Vec::<QuackPrimitive>) -> Result<Option<QuackPrimitive>, QuackException> {
    let first = params.drain(0..1).next().ok_or(QuackException::EmptyStack)?;
    let second = params.drain(0..1).next().ok_or(QuackException::EmptyStack)?;

    if let (QuackPrimitive::Num(a), QuackPrimitive::Num(b)) = (first, second) { 
        Ok(Some(QuackPrimitive::Num(a - b)))
    } 
    else {
        Err(QuackException::General("Expected two numbers!"))
    } 
}

fn gt(mut params: Vec::<QuackPrimitive>) -> Result<Option<QuackPrimitive>, QuackException> {
    let first = params.drain(0..1).next().ok_or(QuackException::EmptyStack)?;
    let second = params.drain(0..1).next().ok_or(QuackException::EmptyStack)?;

    if let (QuackPrimitive::Num(a), QuackPrimitive::Num(b)) = (first, second) { 
        Ok(Some(QuackPrimitive::Bool(a > b)))
    } 
    else {
        Err(QuackException::General("Expected two numbers!"))
    } 
}

fn lt(mut params: Vec::<QuackPrimitive>) -> Result<Option<QuackPrimitive>, QuackException>  {
    let first = params.drain(0..1).next().ok_or(QuackException::EmptyStack)?;
    let second = params.drain(0..1).next().ok_or(QuackException::EmptyStack)?;
    if let (QuackPrimitive::Num(a), QuackPrimitive::Num(b)) = (first, second) {        
        Ok(Some(QuackPrimitive::Bool(a < b)))
    } 
    else {
        Err(QuackException::General("Expected two numbers!"))
    } 
}

fn dump(mut params: Vec::<QuackPrimitive>) -> Result<Option<QuackPrimitive>, QuackException> {
    println!("{:?}", params.drain(0..1).next());
    Ok(None)
}

pub fn get_builtins() -> Vec::<(Box::<Callback>, &'static str,  Option<TypeInfo>)> {
    vec!(
        (Box::new(gt), "gt", Some(TypeInfo::Bool)),
        (Box::new(lt), "lt", Some(TypeInfo::Bool)),
        (Box::new(add), "add", Some(TypeInfo::Num)),
        (Box::new(sub), "sub", Some(TypeInfo::Num)),
        (Box::new(dump), "dump", None),
    )
}