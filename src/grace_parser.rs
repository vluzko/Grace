use grace_error::*;
use std::str;
use std::io::BufReader;
use std::io::prelude::*;
use std::fs::File;
use std::str::from_utf8;
use std::env;

extern crate nom;
use self::nom::*;
use self::nom::IResult::Done;
use expression::*;
//use nom::Offset;

pub fn parse_grace(input: &str) -> Result<&[u8], GraceError> {
    parse_grace_from_slice(input.as_bytes())
}

// This is the important function
pub fn parse_grace_from_slice(input: &[u8]) -> Result<&[u8], GraceError> {
    let output = statement_ast(input); // for now this is all it can do
    match output {
        Done(i, o) => println!("{}", (*o).to_string()),
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

named!(assignment_rule<&[u8],(Identifier, &[u8], Box<Expression>)>,
    tuple!(
        identifier_ast,
        ws!(tag!("=")),
        and_expr_ast
    )
);

named!(statement_rule<&[u8],(Box<Statement>, char)>,
    tuple!(alt!(assignment_ast), newline)
);

named!(block_rule<&[u8],(Vec<Box<Statement>>)>,
    many0!(statement_ast)
);

fn block_ast(input: &[u8]) -> IResult<&[u8], Box<Block>> {
    let parse_result = block_rule(input);
    let node= match parse_result {
        Done(i,o) => Done(i,Box::new(Block{statements: o})),
        IResult::Incomplete(n) => {
            println!("inc {:?}", n);
            panic!();
        },
        IResult::Error(e) => {
            println!("err: {}", e);
            panic!()
        }
    };

    return node;
}

fn statement_ast(input: &[u8]) -> IResult<&[u8], Box<Statement>> {
    let parse_result = statement_rule(input);
    let node= match parse_result {
        Done(i,o) => Done(i, o.0),
        IResult::Incomplete(n) => {
            println!("inc {:?}", n);
            panic!();
        },
        IResult::Error(e) => {
            println!("err: {}", e);
            panic!()
        }
    };

    return node;
}

fn assignment_ast(input: &[u8]) -> IResult<&[u8], Box<Assignment>> {
    let parse_result = assignment_rule(input);
    let node= match parse_result {
        Done(i,o) => {
            let val = Box::new(Assignment{identifier: o.0, expression: o.2});
            Done(i, val)
        },
        IResult::Incomplete(n) => {
            println!("inc {:?}", n);
            panic!();
        },
        IResult::Error(e) => {
            println!("err: {}", e);
            panic!()
        }
    };

    return node;
}

fn identifier_ast(input: &[u8]) -> IResult<&[u8], Identifier> {
    let parse_result= identifier_rule(input);
    let node = match parse_result {
        Done(i,o) => {
            let val = match from_utf8(o) {
                Ok(v) => v,
                x => panic!()
            };
            let ident: Identifier = Identifier{name: val.to_string()};
            Done(i,ident)
        },
        x => panic!()
    };
    return node;
}

named!(boolean_rule<&[u8],(&[u8])>,
    alt!(tag!("true") | tag!("false"))
);

named!(and_rule<&[u8], (Box<Expression>, &[u8], Box<Expression>)>,
    tuple!(
        alt!(bool_expr_ast),
        ws!(tag!("and")),
        alt!(and_expr_ast | bool_expr_ast)
    )
);


fn and_expr_ast(input: &[u8]) -> IResult<&[u8], Box<Expression>> {
    let parse_result = and_rule(input);


    let node = match parse_result {
        Done(i, o) => {
            Done(i, Box::new(BinaryExpression{
                operator: BinaryOperator::And,
                left: o.0,
                right: o.2
            }) as Box<Expression>)
        },
        // TODO: Error type
        IResult::Incomplete(_) => IResult::Error(ErrorKind::Alpha),
        IResult::Error(e) => IResult::Error(e)
    };
    return node;
}

fn bool_expr_ast(input: &[u8]) -> IResult<&[u8], Box<Expression>> {
    let parse_result= boolean_rule(input);


    let node= match parse_result {
        Done(i,o) => {
            match from_utf8(o) {
                Ok("true") => Done(i, Box::new(Boolean::True) as Box<Expression>),
                Ok("false") => Done(i, Box::new(Boolean::False) as Box<Expression>),
                _ => panic!(),
            }
        },
        x => panic!()
    };
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
        let l = line.unwrap() + "\n";
        contents = l.chars().collect();
        println!("File contents: {}", contents);

        // parse first line
        let results = parse_grace(&contents);
    }
}

