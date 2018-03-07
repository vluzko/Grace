use grace_error::*;
use std::str;
use std::io::BufReader;
use std::io::prelude::*;
use std::fs::File;
use std::str::from_utf8;
use std::env;

extern crate nom;
use self::nom::*;
use expression;
//use nom::Offset;

pub fn parse_grace(input: &str) -> Result<&[u8], GraceError> {
    parse_grace_from_slice(input.as_bytes())
}

// This is the important function
pub fn parse_grace_from_slice(input: &[u8]) -> Result<&[u8], GraceError> {
    parse_assignment(input) // for now this is all it can do
}

fn parse_assignment(input:&[u8]) -> Result<&[u8], GraceError> {
    match assignment_ast(input) {
        nom::IResult::Done(i,o) => println!("{}", (*o).to_string()),
        _ => panic!()
    }

    Err(GraceError::GenericError)
}


named!(identifier_rule<&[u8],(&[u8])>,
    recognize!(
        pair!(
            alt!(alpha | tag!("_")),
            many0!(alt!(alpha | tag!("_") | digit))
            )   
    )
);

// TODO: Should handle all non-newline characters (well, just ASCII for now).
// TODO: Decide: are single quotes and double quotes equivalent
// a la python, or are single quotes for single characters
// a la C++? Or some other thing?
named!(string_literal_rule<&[u8],(&[u8])>,
    recognize!(
        tuple!(
            tag!("\""),
            opt!(alpha),
            tag!("\"")
            )
    )
);

named!(boolean_rule<&[u8],(&[u8])>,
    alt!(tag!("true") | tag!("false"))
);

named!(assignment_rule<&[u8],(expression::Identifier, &[u8], Box<expression::Expression>)>,
    tuple!(
        identifier_ast,
        ws!(tag!("=")),
        bool_expr_ast
    )
);

fn assignment_ast(input: &[u8]) -> nom::IResult<&[u8], Box<expression::ASTNode>> {
    let parse_result = assignment_rule(input);
    let node= match parse_result {
        nom::IResult::Done(i,o) => {
            let val = Box::new(expression::Assignment{identifier: o.0, expression: o.2}) as Box<expression::ASTNode>;
            nom::IResult::Done(i, val)
        },
        _ => panic!()
//        Ok(val) => nom::IResult::Done(input, Box::new(expression::Assignment{identifier: val.0, expression: val.2})),
//        Err(_) => nom::IResult::Error(nom::ErrorKind::Alpha)
    };

    return node;
}

named!(and_expr<&[u8], &[u8]>,
    alt!(identifier_rule | string_literal_rule | digit | boolean_rule)
);

fn identifier_ast(input: &[u8]) -> nom::IResult<&[u8], expression::Identifier> {
    let parse_result= identifier_rule(input);
    let node: nom::IResult<&[u8], expression::Identifier, u32>= match parse_result {
        nom::IResult::Done(i,o) => {
            let val = match from_utf8(o) {
                Ok(v) => v,
                x => panic!()
            };
            let ident: expression::Identifier = expression::Identifier{name: val.to_string()};
            nom::IResult::Done(i,ident)
        },
        x => panic!()
    };
    return node;
}

fn bool_expr_ast(input: &[u8]) -> nom::IResult<&[u8], Box<expression::Expression>> {
    let parse_result= boolean_rule(input);

    let node: nom::IResult<&[u8], Box<expression::Expression>, u32>= match parse_result {
        nom::IResult::Done(i,o) => {
            match from_utf8(o) {
            Ok("true") => nom::IResult::Done(i, Box::new(expression::Boolean::True) as Box<expression::Expression>),
            Ok("false") => nom::IResult::Done(i, Box::new(expression::Boolean::False) as Box<expression::Expression>),
            _ => panic!(),
            }
        },
        x => panic!()
    };
    println!("got to end of bool_expr_ast");
    return node;
}

#[test]
pub fn basic_file_test() {
    // Read file
    let filename= "./test_data/simple_grace.gr";
    let mut f = File::open(filename).expect("File not found");
    let file = BufReader::new(&f);
    let mut contents = String::new();
    for (num, line) in file.lines().enumerate() {
        let l = line.unwrap();
        contents = l.chars().collect();
        println!("File contents: {}", contents);

        // parse first line
        let results = parse_grace(&contents);
    }
}

