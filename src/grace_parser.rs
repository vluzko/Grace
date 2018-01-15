use grace_error::*;
use std::str;
use std::io::BufReader;
use std::io::prelude::*;
use std::fs::File;
use std::str::from_utf8;
use std::env;

extern crate nom;
use self::nom::*;
//use nom::Offset;

pub fn parse_grace(input: &str) -> Result<&[u8], GraceError> {
    parse_grace_from_slice(input.as_bytes())
}

// This is the important function
pub fn parse_grace_from_slice(input: &[u8]) -> Result<&[u8], GraceError> {
    parse_assignment(input) // for now this is all it can do
}

fn parse_assignment(input:&[u8]) -> Result<&[u8], GraceError> {
    println!("result {:?}", assignment(input).to_result());
    Err(GraceError::GenericError)
    // whitespace (use existing fn)
    // identifier
    // whitespace (use existing fn)
    // check that the equals sign exists
    // whitespace (use existing fn)
    // value
}

named!(identifier<&[u8],(&[u8])>,
    recognize!(
        pair!(
            alt!(alpha | tag!("_")),
            many0!(alt!(alpha | tag!("_") | digit))
            )   
    )
);

//TODO: Decide: are single quotes and double quotes equivalent
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

named!(assignment<&[u8],(&[u8],&[u8],&[u8])>,
    tuple!(
    identifier, 
    ws!(tag!("=")), 
    or_expr)
);

named!(or_expr<&[u8], &[u8]>,
    recognize!(
    tuple!(
    and_expr, many0!(pair!(tag!(" or "), and_expr))
    )
    )
);

named!(and_expr<&[u8], &[u8]>,
    alt!(identifier | string_literal | digit | bool)
);

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
        println!("Result: {:?}", results);
    }
}

