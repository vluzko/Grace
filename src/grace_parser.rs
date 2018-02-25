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
    match assignment(input).to_result() {
        Ok(T) => println!("Parsed to: {}", T.2.to_string()),
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

named!(bool<&[u8],(&[u8])>,
    alt!(tag!("true") | tag!("false"))
);

fn bool_ast(input: &[u8]) -> nom::IResult<&[u8], Box<expression::ASTNode>> {
    let f: &[u8] = &[102, 97, 108, 115, 101];
    let t: &[u8] = &[116, 114, 117, 101];
    let parse_result = bool(input);
//    match parse_result.to_result() {
//        Ok(val) => match from_utf8(val) {
//            Ok("true") => println!("True"),
//            Ok("false") => println!("False"),
//            Ok(&_) => println!("bool somehow matched non true/false string."),
//            Err(_) => println!("wat")
//        },
//        _ => println!("Error in boolean")
//    }
    let n: IResult<&[u8], Box<expression::ASTNode>, nom::ErrorKind> = match parse_result.to_result() {
        Ok(val) => match from_utf8(val) {
            Ok("true") => nom::IResult::Done(input, Box::new(expression::Boolean::True) as Box<expression::ASTNode>),
            Ok("false") => nom::IResult::Done(input,Box::new(expression::Boolean::False) as Box<expression::ASTNode>),
            Ok(&_) => nom::IResult::Error(nom::ErrorKind::Alpha),
            Err(_) => nom::IResult::Error(nom::ErrorKind::Alpha)
        },
        Err(x) => nom::IResult::Error(nom::ErrorKind::Alpha)
    };
    let node = Box::new(expression::Boolean::True) as Box<expression::ASTNode>;
    return nom::IResult::Done(input, node);
}

named!(assignment<&[u8],(&[u8], &[u8], Box<expression::ASTNode>)>,
    tuple!(
        identifier,
        ws!(tag!("=")),
        bool_ast
    )
);

//named!(or_expr<[u8], ([u8], [u8], [[u8]])>,
//    tuple!(and_expr, tag!(" or "), or_expr)
//);


named!(and_expr<&[u8], &[u8]>,
    alt!(identifier | string_literal | digit | bool)
);

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

