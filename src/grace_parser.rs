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
    let output = block_ast(input, None); // for now this is all it can do
    match output {
        Done(i, o) => println!("{}", (*o).to_string()),
        _ => panic!()
    }


    Err(GraceError::GenericError)
}

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

//named!(if_rule<&[u8], (&[u8])>,
//    tuple!(
//
//)
//)

named_args!(indent_rule (number_of_indents: usize) <Vec<&[u8]>>,
    many_m_n!(number_of_indents, number_of_indents, tag!(" "))
);

fn block_rule(input: &[u8], minimum_indent: usize) -> IResult<&[u8], Vec<Box<Statement>>> {
    let first_indent_parse: IResult<&[u8], Vec<&[u8]>> = many0!(input, tag!(" "));
    let full_indent: (&[u8], Vec<&[u8]>) = match first_indent_parse {
        Done(i, o) => (i, o),
        _ => panic!()
    };

    // Break if the block is not indented enough.
    if full_indent.1.len() < minimum_indent {
        // TODO: This will happen if the indentation level is too low. Throw a proper error.
        return IResult::Error(ErrorKind::Count);
    } else {
        let expected_indent = full_indent.1.len();

        // Parser for statements.
        let first_statement_lam = |i| statement_ast(i, 0);
        let statement_lam = |i| statement_ast(i, expected_indent);
        // Parser for indents.
        let indent_lam = |i| indent_rule(i, expected_indent);

        // We end up reparsing the initial indent, but that's okay. The alternative is joining two
        // vectors, which is much slower.
        let statements = tuple!(input,
            many1!(preceded!(indent_lam, statement_lam))
        );

        return statements;
    }

}

fn block_ast(input: &[u8], indent: Option<usize>) -> IResult<&[u8], Box<Block>> {
    let real_indent: usize = match indent {
        Some(x) => x,
        None => 0
    };
    let parse_result = block_rule(input, real_indent);
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

//named!(statement_rule<&[u8],(Box<Statement>, char)>,
//    tuple!(alt!(assignment_ast, if_ast), newline)
//)


//fn indent_rule(input: &[u8], number_of_indents: usize) -> IResult<&[u8], u32> {
//    named!(temp <&[u8], Vec<&[u8]>>, )
//}

fn custom_eof(input: &[u8]) -> IResult<&[u8], &[u8]> {
    eof!(input, )
}

fn statement_rule(input: &[u8], indent: usize) -> IResult<&[u8], (Box<Statement>, &[u8])> {
    let if_lam = |i| if_ast(i, indent);
    tuple!(input,
        alt!(assignment_ast | if_lam),
        // Rust complains if we call eof directly. I have no idea why.
        alt!(custom_eof | tag!("\n"))
    )
}

fn statement_ast(input: &[u8], indent: usize) -> IResult<&[u8], Box<Statement>> {
    let parse_result = statement_rule(input, indent);
    let node= match parse_result {
        Done(i,o) => Done(i, o.0),
        IResult::Incomplete(n) => {
            println!("Statement incomplete: {:?}", n);
            panic!();
        },
        IResult::Error(e) => {
            println!("err: {}", e);
            panic!()
        }
    };

    return node;
}

fn if_rule(input: &[u8], indent: usize) -> IResult<&[u8], (&[u8], Box<Expression>, &[u8], char, Box<Block>)> {
    let block_lam = |i| block_ast(i, Some(indent));
    tuple!(input,
        tag!("if"),
        ws!(and_expr_ast),
        tag!(":"),
        newline,
        block_lam
    )
}

fn if_ast(input: &[u8], indent: usize) -> IResult<&[u8], Box<Statement>> {
    let parse_result = if_rule(input, indent);
    let node = match parse_result {
        Done(i, o) => {
            let if_statement = IfStatement{condition: o.1, main_block: o.4, elifs: None, else_block: None};
            Done(i, Box::new(if_statement) as Box<Statement>)
        },
        _ => panic!()
    };

    return node;
}


named!(assignment_rule<&[u8],(Identifier, &[u8], Box<Expression>)>,
    tuple!(
        identifier_ast,
        ws!(tag!("=")),
        and_expr_ast
    )
);

fn assignment_ast(input: &[u8]) -> IResult<&[u8], Box<Statement>> {
    let parse_result = assignment_rule(input);
    let node= match parse_result {
        Done(i,o) => {
            let val = Box::new(Assignment{identifier: o.0, expression: o.2}) as Box<Statement>;
            Done(i, val)
        },
        IResult::Incomplete(n) => {
            println!("inc {:?}", n);
            panic!();
        },
        IResult::Error(e) => {
            IResult::Error(ErrorKind::Alpha)
        }
    };

    return node;
}

named!(identifier_rule<&[u8],(&[u8])>,
    recognize!(
        pair!(
            alt!(alpha | tag!("_")),
            many0!(alt!(alpha | tag!("_") | digit))
            )
    )
);

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
        IResult::Incomplete(n) => {
            println!("inc {:?}", n);
            panic!();
        },
        IResult::Error(e) => {
            println!("Error in identifier_ast: {}", e);
            println!("Input was: {:?}", from_utf8(input));
            IResult::Error(ErrorKind::Alpha)
        }
    };
    return node;
}

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

named!(boolean_rule<&[u8],(&[u8])>,
    alt!(tag!("true") | tag!("false"))
);

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
    let mut contents = String::new();
    f.read_to_string(&mut contents);
    let result = parse_grace(contents.as_str());
}

