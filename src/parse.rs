use nom::character::complete::{one_of, alpha1, multispace0, digit1, line_ending, space0};
use nom::bytes::complete::tag;
use nom::combinator::{map, map_res, opt};
use nom::branch::alt;
use nom::multi::{separated_list, fold_many1, many1};
use nom::sequence::{tuple, preceded, terminated, separated_pair};
use nom::error::VerboseError;

mod quack_struct;
use quack_struct::*;

type Value = i64;

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum QuackExpression {
    Name(String),
    NumLiteral(f64),
    Assign{left: Box<QuackExpression>, right: Box<QuackExpression>}, // todo left can just be a string
    StructDef{name: String, fields: Vec::<(String, String)>},
    CreateStruct{struct_name: String, fields: Vec<QuackExpression>},
    AccessStructField{var_name: String, field: String},
    FunctionCall{name: String, arguments: Vec::<QuackExpression>},
    // TODO expression that produces a boolean value?
    ConditionBranch{condition: Box<QuackExpression>, inner: Vec::<QuackExpression>, expected: bool}, // TODO split this away from other expressions? doesn't make sense you could have an condition within an operation
    FunctionDefinition{name: String, parameters: Vec::<(String, String)>, inner: Vec::<QuackExpression>}
}

#[derive(Debug)]
pub enum ParseException {
    Unknown
}
  

fn parse_var<'a>(i: &'a str) -> nom::IResult<&'a str, QuackExpression, VerboseError<&'a str>> {
    map(alpha1, |v: &str| QuackExpression::Name(v.to_owned()))(i)
}

fn parse_num_literal<'a>(i: &'a str) -> nom::IResult<&'a str, QuackExpression, VerboseError<&'a str>> {
    map_res(
        fold_many1(
            alt((tag("-"), digit1)), 
            String::new(), |mut a, 
            item| 
            {
                a.push_str(item);
                a
            }
        ),
        |s: String| s.parse::<f64>().map(QuackExpression::NumLiteral)
    )(i)
}

fn parse_atom<'a>(i: &'a str) -> nom::IResult<&'a str, QuackExpression, VerboseError<&'a str>> {
    alt((
        parse_var,
        parse_num_literal
    ))(i)
}

fn function_name<'a>(i: &'a str) -> nom::IResult<&'a str, String, VerboseError<&'a str>> {
    nom::multi::fold_many1(alt((alpha1, tag("_"))), "".to_owned(),
        |mut acc: String, item: &str| {
            acc + item
        }
    )(i)
}

fn in_braces<'a, O1, F>(open: &'static str, inner: F, close: &'static str) -> impl Fn(&'a str) -> nom::IResult<&'a str, O1, VerboseError<&'a str>>
where
  F: Fn(&'a str) ->  nom::IResult<&'a str, O1, VerboseError<&'a str>>
{
    preceded(
        terminated(
            preceded(opt(spaced(line_ending)), spaced(tag(open))),
            opt(spaced(line_ending))),
        terminated(
            spaced(inner),
            preceded(opt(spaced(line_ending)), spaced(tag(close))))
    )
}

fn spaced<'a, O1, F>(inner: F) -> impl Fn(&'a str) -> nom::IResult<&'a str, O1, VerboseError<&'a str>>
where
  F: Fn(&'a str) ->  nom::IResult<&'a str, O1, VerboseError<&'a str>>
{
    preceded(space0, terminated(inner, space0))
}


fn assign_operation<'a>(i: &'a str) -> nom::IResult<&'a str, QuackExpression, VerboseError<&'a str>>
{
    map(
        tuple((spaced(sub_expression), spaced(tag("=")), spaced(sub_expression))),
        |(left, _, right)| QuackExpression::Assign{left: Box::new(left), right: Box::new(right)})(i)
}

fn parse_function_call<'a>(i: &'a str) -> nom::IResult<&'a str, QuackExpression, VerboseError<&'a str>> {
    let args = preceded(
        spaced(tag("(")),
        terminated(
            separated_list(spaced(tag(",")), sub_expression),
            spaced(tag(")"))
        )
    );

    map(
        tuple((spaced(function_name), spaced(args))), 
        |(n, f)| {          
            QuackExpression::FunctionCall{name: n.to_owned(), arguments: f}
        }
    )(i)
}

fn parse_conditional<'a>(i: &'a str) -> nom::IResult<&'a str, QuackExpression, VerboseError<&'a str>> {
    map(
        tuple((
            spaced(tag("if")),
            spaced(opt(tag("not"))),
            spaced(sub_expression),
            in_braces("{", parse_inner, "}")
        )),
        |(_, n, condition, inner)| {
            QuackExpression::ConditionBranch{condition: Box::new(condition), inner, expected: n.is_none()}
        })
    (i)
}

fn parse_function_def<'a>(i: &'a str) -> nom::IResult<&'a str, QuackExpression, VerboseError<&'a str>> {
    map(
        preceded(
            spaced(tag("fn")),
            tuple((
                function_name,
                in_braces(
                    "(", 
                    opt(separated_list(spaced(tag(",")), separated_pair(alpha1, spaced(tag(":")), alpha1))),
                    ")"
                ),
                in_braces("{", parse_inner, "}")
            ))
        ),
        |(name, params, inner)| {
            let parameters = params.unwrap_or(vec!()).iter().map(|(x, y)| (x.to_string(), y.to_string())).collect();
            QuackExpression::FunctionDefinition{name: name.to_string(), parameters, inner}
        }
    )(i)
}

fn sub_expression<'a>(i: &'a str) -> nom::IResult<&'a str, QuackExpression, VerboseError<&'a str>> {
    alt((
        parse_function_call,
        parse_struct_field_access,
        parse_struct_create,
        parse_atom,
    ))(i)
}

fn parse_inner<'a>(i: &'a str) -> nom::IResult<&'a str, Vec::<QuackExpression>, VerboseError<&'a str>> {
    separated_list(spaced(many1(line_ending)), spaced(alt((parse_struct_def, parse_function_def, assign_operation, parse_conditional, parse_function_call))))(i)
}

// todo some expressions can appear at the top level, some can't
pub fn parse(i: &str) -> Result<Vec::<QuackExpression>, ParseException> {
    let res = preceded(
        spaced(line_ending),
        parse_inner
    )(i);
    res.map_err(|_| ParseException::Unknown).and_then(|(remain, x)| {
        Ok(x) // expand for debugging
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_assign_local(){
        let s = "a = 1";
        let expected = QuackExpression::Assign {
            left: Box::new(QuackExpression::Name("a".to_owned())),
            right: Box::new(QuackExpression::NumLiteral(1.0))};
        let (_, res) = assign_operation(&s).unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_parse_assign_local_newlines(){
        let s = "a = 1\nb = 2";
        let expected = vec!(
            QuackExpression::Assign {
                left: Box::new(QuackExpression::Name("a".to_owned())),
                right: Box::new(QuackExpression::NumLiteral(1.0))},
            QuackExpression::Assign {
                left: Box::new(QuackExpression::Name("b".to_owned())),
                right: Box::new(QuackExpression::NumLiteral(2.0))}
        );
        let res = parse(&s).unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_function_call_single_arg() {
        let s = "test(something)";
        let expected = QuackExpression::FunctionCall {
            name: "test".to_owned(),
            arguments: vec!(
                QuackExpression::Name("something".to_owned()),
            )
        };
        let (_, res) = parse_function_call(&s).unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_function_call_multiline(){
        let s = r"something = 1
        test(something)";
        let expected = vec!(
            QuackExpression::Assign {
                left: Box::new(QuackExpression::Name("something".to_owned())),
                right: Box::new(QuackExpression::NumLiteral(1.0))},
            QuackExpression::FunctionCall {
                name: "test".to_owned(),
                arguments: vec!(
                    QuackExpression::Name("something".to_owned()),
            )
        });
        let res = parse(&s).unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_function_call(){
        let s = "test(1, 2, 3)";
        let expected = QuackExpression::FunctionCall {
            name: "test".to_owned(),
            arguments: vec!(
                QuackExpression::NumLiteral(1.0),
                QuackExpression::NumLiteral(2.0),
                QuackExpression::NumLiteral(3.0),
            )
        };
        let (_, res) = parse_function_call(&s).unwrap();
        assert_eq!(res, expected);
    }

    // TODO test function single arg

    #[test]
    fn test_function_call_complex_params(){
        let s = "test(new other {1}, a, 3)";
        let expected = QuackExpression::FunctionCall {
            name: "test".to_owned(),
            arguments: vec!(
                QuackExpression::CreateStruct {
                struct_name: "other".to_owned(),
                fields: vec!(
                    QuackExpression::NumLiteral(1.0),
                    )
                },
                QuackExpression::Name("a".to_owned()),
                QuackExpression::NumLiteral(3.0),
            )
        };
        let (_, res) = parse_function_call(&s).unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_conditional_branch()
    {
        let s = "if equal(a, 1) { a = 2 }";
        let expected = QuackExpression::ConditionBranch {
            condition: Box::new(
                QuackExpression::FunctionCall{
                    name: "equal".to_owned(), 
                    arguments: vec!(QuackExpression::Name("a".to_owned()), QuackExpression::NumLiteral(1.0))
                }
            ),
            inner: vec!(
                QuackExpression::Assign {
                    left: Box::new(QuackExpression::Name("a".to_owned())),
                    right: Box::new(QuackExpression::NumLiteral(2.0))
                }
            ),
            expected: true
        };

        let (_, res) = parse_conditional(&s).unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_conditional_branch_multiline()
    {
        let s = r"if equal(a, 1) 
        { 
            a = 2
            a = 3
        }
        a = 4";
        let expected = vec!(QuackExpression::ConditionBranch {
            condition: Box::new(
                QuackExpression::FunctionCall{
                    name: "equal".to_owned(), 
                    arguments: vec!(QuackExpression::Name("a".to_owned()), QuackExpression::NumLiteral(1.0))
                }
            ),
            inner: vec!(
                QuackExpression::Assign {
                    left: Box::new(QuackExpression::Name("a".to_owned())),
                    right: Box::new(QuackExpression::NumLiteral(2.0))
                },
                QuackExpression::Assign {
                    left: Box::new(QuackExpression::Name("a".to_owned())),
                    right: Box::new(QuackExpression::NumLiteral(3.0))
                }
            ),
            expected: true
        },
        QuackExpression::Assign {
            left: Box::new(QuackExpression::Name("a".to_owned())),
            right: Box::new(QuackExpression::NumLiteral(4.0))
        });

        let res = parse(&s).unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_define_function(){
        let s = r"fn test() { 
            a = 1
        }";

        let expected = QuackExpression::FunctionDefinition {
            name: "test".to_owned(),
            parameters: vec!(),
            inner: vec!(
                QuackExpression::Assign {
                    left: Box::new(QuackExpression::Name("a".to_owned())),
                    right: Box::new(QuackExpression::NumLiteral(1.0))}
            )
        };

        let res =  parse(&s).unwrap();
        assert_eq!(res, vec!(expected));
    }

    #[test]
    fn test_define_function_single_arg_with_call(){
        let s = r"fn test(b: num) { 
            a = 1
        }
        test(1)";

        let expected = vec!(
            QuackExpression::FunctionDefinition {
                name: "test".to_owned(),
                parameters: vec!(
                    ("b".to_owned(), "num".to_owned())
                ),
                inner: vec!(
                    QuackExpression::Assign {
                        left: Box::new(QuackExpression::Name("a".to_owned())),
                        right: Box::new(QuackExpression::NumLiteral(1.0))}
                )
            },
            QuackExpression::FunctionCall{
                name: "test".to_owned(),
                arguments: vec!(
                    QuackExpression::NumLiteral(1.0)
                )
            }
        );

        let res =  parse(&s).unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn test_define_function_with_args(){
        let s = r"fn test(a: num, b: num) {
            a = 1
        }";

        let expected = QuackExpression::FunctionDefinition {
            name: "test".to_owned(),
            parameters: vec!(
                ("a".to_string(), "num".to_string()),
                ("b".to_string(), "num".to_string())
            ),
            inner: vec!(
                QuackExpression::Assign {
                    left: Box::new(QuackExpression::Name("a".to_owned())),
                    right: Box::new(QuackExpression::NumLiteral(1.0))}
            )
        };

        let res =  parse(&s).unwrap();
        assert_eq!(res, vec!(expected));
    }

    #[test]
    fn test_simple_program(){
        let s = r"
        fn test() {
            a = 1
        }


        fn main() {
            test()
        }";

        let expected = vec!(
            QuackExpression::FunctionDefinition {
                name: "test".to_owned(),
                parameters: vec!(),
                inner: vec!(
                    QuackExpression::Assign {
                        left: Box::new(QuackExpression::Name("a".to_owned())),
                        right: Box::new(QuackExpression::NumLiteral(1.0))}
                )
            },
            QuackExpression::FunctionDefinition {
                name: "main".to_owned(),
                parameters: vec!(),
                inner: vec!(
                    QuackExpression::FunctionCall {
                        name: "test".to_owned(),
                        arguments: vec!()
                    }
                ),
            }
        );

        let res =  parse(&s).unwrap();
        assert_eq!(res, expected);
    }
}