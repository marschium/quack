use nom::character::complete::{one_of, alpha1, multispace0, digit1, line_ending, space0};
use nom::bytes::complete::tag;
use nom::combinator::{map, map_res, opt};
use nom::branch::alt;
use nom::multi::{separated_list};
use nom::sequence::{tuple, preceded, terminated, separated_pair};
use nom::error::VerboseError;

use super::{QuackExpression, ParseException, spaced, parse_atom};

pub fn parse_struct_field_access<'a>(i: &'a str) -> nom::IResult<&'a str, QuackExpression, VerboseError<&'a str>>{
    map(separated_pair(alpha1, tag("."), alpha1), |(v, f): (&str, &str)| QuackExpression::AccessStructField{var_name: v.to_owned(), field: f.to_owned()})(i)
}

pub fn parse_struct_fields<'a>(i: &'a str) -> nom::IResult<&'a str, Vec::<(String, String)>, VerboseError<&'a str>> {
    preceded(
        spaced(tag("{")),
        terminated(
            separated_list(spaced(tag(",")), map(tuple((alpha1, spaced(tag(":")), alpha1)), |(x, _, y)| (x.to_owned(), y.to_owned()))),
            spaced(tag("}"))
        )
    )(i)
}

pub fn parse_struct_def<'a>(i: &'a str) -> nom::IResult<&'a str, QuackExpression, VerboseError<&'a str>> {
    map(tuple(
        (spaced(tag("struct")),
        map(alpha1, |x: &str| x.to_owned()),
        parse_struct_fields)), 
        |(_, name, fields)| { 
            QuackExpression::StructDef{name, fields}
        })(i)
}

pub fn parse_struct_create<'a>(i: &'a str) -> nom::IResult<&'a str, QuackExpression, VerboseError<&'a str>> {
    let fields = preceded(
        spaced(tag("{")),
        terminated(
            separated_list(spaced(tag(",")), parse_atom),
            spaced(tag("}"))
        )
    );

    map(
        preceded(
            spaced(tag("new")), 
            tuple((spaced(alpha1), spaced(fields)))
        ),
        |(n, f)| {          
            QuackExpression::CreateStruct{struct_name: n.to_owned(), fields: f}
        }
    )(i)
}

#[cfg(test)]
mod test {

    use super::*;
    use super::super::assign_operation;

    #[test]
    fn test_struct_field_access() {
        let s = "something.other";
        let expected = QuackExpression::AccessStructField {
            var_name: "something".to_owned(),
            field: "other".to_owned()
        };
        let (_, res) = parse_struct_field_access(&s).unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_struct_def(){
        let s = "struct test { a: num, b: testb }";
        let expected = QuackExpression::StructDef {
            name: "test".to_owned(),
            fields: vec!(
                ("a".to_owned(), "num".to_owned()),
                ("b".to_owned(), "testb".to_owned()),
            )
        };
        let (_, res) = parse_struct_def(&s).unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_struct_create(){
        let s = "new test { 1, 2, 3 }";
        let expected = QuackExpression::CreateStruct {
            struct_name: "test".to_owned(),
            fields: vec!(
                QuackExpression::NumLiteral(1.0),
                QuackExpression::NumLiteral(2.0),
                QuackExpression::NumLiteral(3.0),
            )
        };
        let (_, res) = parse_struct_create(&s).unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_parse_assign_struct_literals(){
        let s = "a = new test { 1, 2, 3 }";
        let expected = QuackExpression::Assign {
            left: Box::new(QuackExpression::Name("a".to_owned())),
            right: Box::new( QuackExpression::CreateStruct {
                struct_name: "test".to_owned(),
                fields: vec!(
                    QuackExpression::NumLiteral(1.0),
                    QuackExpression::NumLiteral(2.0),
                    QuackExpression::NumLiteral(3.0),
                )
            })};
        let (_, res) = assign_operation(&s).unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_parse_assign_struct_variables(){
        let s = "a = new test { a, b, c }";
        let expected = QuackExpression::Assign {
            left: Box::new(QuackExpression::Name("a".to_owned())),
            right: Box::new( QuackExpression::CreateStruct {
                struct_name: "test".to_owned(),
                fields: vec!(
                    QuackExpression::Name("a".to_owned()),
                    QuackExpression::Name("b".to_owned()),
                    QuackExpression::Name("c".to_owned()),
                )
            })};
        let (_, res) = assign_operation(&s).unwrap();
        assert_eq!(res, expected);
    }
}