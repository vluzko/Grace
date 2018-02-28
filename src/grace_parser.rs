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
    match assignment_rule(input).to_result() {
        Ok(T) => println!("Parsed to: {:?}", T.2.to_string()),
        Err(_) => println!("Error")
    }

    Err(GraceError::GenericError)
}

named!(identifier<&[u8],(&[u8])>,
    recognize!(
        pair!(
            alt!(alpha | tag!("_")),
            many0!(alt!(alpha | tag!("_") | digit))
            )   
    )
);

fn parse_identifier(input: &[u8]) -> nom::IResult<&[u8], i64>{
//    return identifier(input);
    print("identifier");
    let val = nom::IResult::Done(input, 8);
    return val;
}

// TODO: Should handle all non-newline characters (well, just ASCII for now).
// TODO: Decide: are single quotes and double quotes equivalent
// a la python, or are single quotes for single characters
// a la C++? Or some other thing? 
named!(string_literal<&[u8],(&[u8])>,
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

named!(assignment_rule<&[u8],(&[u8], &[u8], Box<expression::Expression>)>,
    tuple!(
        identifier,
        ws!(tag!("=")),
        bool_ast
    )
);

//fn assignment(input: &[u8]) -> nom::IResult<&[u8], Box<expression::ASTNode>> {
//    let parse_result = assignment_rule(input);
//    let node= match parse_result.to_result() {
//        Ok(val) => nom::IResult::Done(input, Box::new(expression::Assignment{identifier: val.0, expression: val.2})),
//        Err(_) => nom::IResult::Error(nom::ErrorKind::Alpha)
//    };
//
//    return node;
//}

named!(and_expr<&[u8], &[u8]>,
    alt!(identifier | string_literal | digit | boolean_rule)
);

fn bool_ast(input: &[u8]) -> nom::IResult<&[u8], Box<expression::Expression>> {
    let parse_result= boolean_rule(input);
    let node= match parse_result.to_result() {
        Ok(val) => match from_utf8(val) {
            Ok("true") => nom::IResult::Done(input, Box::new(expression::Boolean::True) as Box<expression::Expression>),
            Ok("false") => nom::IResult::Done(input,Box::new(expression::Boolean::False) as Box<expression::Expression>),
            _ => nom::IResult::Error(nom::ErrorKind::Alpha),
        },
        Err(_) => nom::IResult::Error(nom::ErrorKind::Alpha)
    };

    return node;
}


//named!(or_expr<[u8], ([u8], [u8], [[u8]])>,
//    tuple!(and_expr, tag!(" or "), or_expr)
//);




//pub fn parse_file(file_name: &str) {
//    let mut f = File::open(file_name).expect("File not found");
//    let file = BufReader::new(&f);
//    let mut contents = String::new();
//    for (num, line) in file.lines().enumerate() {
//        let l = line.unwrap();
//        contents = l.chars().collect();
//        //f.read_to_string(&mut contents).expect("Problem reading file.");
//        println!("File contents: {}", contents);
//
//        // parse first line
//        let results = parse_grace(&contents);
//        // Print parsing result
//        println!("Result: {:?}", results);
//    }
//}

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
        //f.read_to_string(&mut contents).expect("Problem reading file.");
        println!("File contents: {}", contents);

        // parse first line
        let results = parse_grace(&contents);
        // Print parsing result
//        println!("Result: {:?}", results);
    }
}

