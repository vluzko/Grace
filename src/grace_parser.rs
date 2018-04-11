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

named!(whitespace_char<&[u8], &[u8]>,
    alt!(custom_eof | tag!("\n") | tag!(" "))
);

named!(inline_whitespace<&[u8], Vec<&[u8]>>,
    many0!(whitespace_char)
);

fn between_statement(input: &[u8]) -> IResult<&[u8], Vec<Vec<&[u8]>>> {
    let n = many0!(input,
        terminated!(many0!(tag!(" ")), alt!(custom_eof | tag!("\n")))
    );
    return match n {
        Done(i, o) => {
            Done(i, o)
        },
        IResult::Incomplete(n) => {
            println!("Between incomplete: {:?}. Input was: {:?}", n, from_utf8(input));
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
            println!("Between error: {:?}. Input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    };
}

fn indent_rule(input: &[u8], number_of_indents: usize) -> IResult<&[u8], Vec<&[u8]>> {
    return complete!(input, many_m_n!(number_of_indents, number_of_indents, tag!(" ")));
}

fn inline_wrapped<T>(input: &[u8], parser: fn(&[u8]) -> IResult<&[u8], T>) -> IResult<&[u8], T> {
    delimited!(input,
        inline_whitespace, parser, inline_whitespace
    )
}

fn custom_eof(input: &[u8]) -> IResult<&[u8], &[u8]> {
    return eof!(input, );
}

fn block_rule(input: &[u8], minimum_indent: usize) -> IResult<&[u8], Vec<Box<Statement>>> {
    let first_indent_parse: IResult<&[u8], Vec<&[u8]>> = preceded!(input, opt!(between_statement), many0!(tag!(" ")));
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

        let statement_lam = |i| statement_ast(i, expected_indent);
        // Parser for indents.
        let indent_lam = |i| indent_rule(i, expected_indent);

        // We end up reparsing the initial indent, but that's okay. The alternative is joining two
        // vectors, which is much slower.
        let statements = delimited!(input, 
            opt!(between_statement),
            many1!(complete!(preceded!(between_statement, preceded!(indent_lam, statement_lam)))),
            opt!(between_statement)
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
    let node = match parse_result {
        Done(i,o) => Done(i,Box::new(Block{statements: o})),
        IResult::Incomplete(n) => {
            println!("Block incomplete: {:?}. Input was: {:?}", n, from_utf8(input));
            panic!();
        },
        IResult::Error(e) => {
            println!("Block error: {}. Input was: {:?}", e, from_utf8(input));
            panic!()
        }
    };

    return node;
}



fn eof_or_line(input: &[u8]) -> IResult<&[u8], &[u8]> {
    return alt!(input, custom_eof | tag!("\n"));
}

named!(follow_value<&[u8], &[u8]>,
    alt!(whitespace_char | tag!(":") | tag!("(") | tag!(")"))
);

fn statement_ast(input: &[u8], indent: usize) -> IResult<&[u8], Box<Statement>> {
    let if_lam = |i| if_ast(i, indent);
    let function_dec_lam = |i| function_declaration_ast(i, indent);

    let n = alt!(input, assignment_ast | if_lam | function_dec_lam);

    return match n {
        Done(i, o) => {
            Done(i, o)
        },
        x => x
    }
}

fn if_rule(input: &[u8], indent: usize) -> IResult<&[u8], (
    Box<Expression>,
    Box<Block>,
    Vec<(Box<Expression>, Box<Block>)>,
    Option<Box<Block>>)> {
    let block_lam = |i| block_ast(i, Some(indent + 1));
    let elif_lam = |i| elif_rule(i, indent);
    let else_lam = |i| else_rule(i, indent);

    let full_tuple = tuple!(
        input,
        tag!("if"),
        ws!(and_expr_ast),
        tag!(":"),
        newline,
        block_lam,
        many0!(elif_lam),
        opt!(complete!(else_lam))
    );

    return match full_tuple {
        Done(i,o) => {
            Done(i, (o.1, o.4, o.5, o.6))
        },
        IResult::Incomplete(n) => {
            println!("else incomplete: {:?}", n);
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
            println!("else error {:?}", e);
            IResult::Error(e)
        }
    };
}

fn elif_rule(input: &[u8], indent: usize) -> IResult<&[u8], (Box<Expression>, Box<Block>)> {
    let block_lam = |i| block_ast(i, Some(indent + 1));
    let elif_tuple = tuple!(
        input,
        tag!("elif"),
        ws!(and_expr_ast),
        tag!(":"),
        newline,
        block_lam
    );
    return match elif_tuple {
        Done(i,o) => {
            Done(i, (o.1, o.4))
        },
        IResult::Incomplete(n) => {
//            println!("elif incomplete: {:?}. Input was: {:?}", n, from_utf8(input));
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
//            println!("elif error {:?}. Input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    };
}

fn else_rule(input: &[u8], indent: usize) -> IResult<&[u8], Box<Block>> {

    let block_lam = |i| block_ast(i, Some(indent + 1));
    let else_tuple = tuple!(
        input,
        tag!("else"),
        tag!(":"),
        newline,
        block_lam
    );
    return match else_tuple {
        Done(i,o) => {
            Done(i, o.3)
        },
        IResult::Incomplete(n) => {
            println!("else incomplete: {:?}", n);
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
            println!("else error {:?}", e);
            IResult::Error(e)
        }
    };
}

fn if_ast(input: &[u8], indent: usize) -> IResult<&[u8], Box<Statement>> {
    let parse_result = if_rule(input, indent);
    let node = match parse_result {
        Done(i, o) => {
            let if_statement = IfStatement{condition: o.0, main_block: o.1, elifs: o.2, else_block: o.3};
            Done(i, Box::new(if_statement) as Box<Statement>)
        },
        IResult::Incomplete(n) => {
            println!("if incomplete: {:?}", n);
            panic!();
        },
        IResult::Error(e) => {
            println!("if error: {:?}", e);

            IResult::Error(e)
        }
    };

    return node;
}


//// TODO: Move indent lambda creation into a separate function.
fn function_declaration_ast(input: &[u8], indent: usize) -> IResult<&[u8], Box<Statement>> {
    let block_lam = |i| block_ast(i, Some(indent + 1));
    let inline_lam = |i| inline_wrapped(i, identifier_ast);
    let full_tuple = tuple!(input,
        tag!("fn"),
        inline_lam,
        tag!("():"),
        block_lam
    );

    let node = match full_tuple {
        Done(i, o) => {
            let func_dec = Box::new(FunctionDec{name: o.1, args: vec!(), body: o.3}) as Box<Statement>;
            Done(i, func_dec)
        },
         IResult::Incomplete(n) => {
            println!("function incomplete: {:?}", n);
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
            println!("function error {:?}", e);
            IResult::Error(e)
        }
    };

    return node;
}

named!(assignment_rule<&[u8],(Identifier, &[u8], Box<Expression>)>,
    terminated!(tuple!(
        identifier_ast,
        ws!(tag!("=")),
        and_expr_ast
    ), eof_or_line)
);

fn assignment_ast(input: &[u8]) -> IResult<&[u8], Box<Statement>> {
    let parse_result = assignment_rule(input);
    let node = match parse_result {
        Done(i,o) => {
            let val = Box::new(Assignment{identifier: o.0, expression: o.2}) as Box<Statement>;
            Done(i, val)
        },
        IResult::Incomplete(n) => {
//            println!("Assignment incomplete: {:?}. Input was: {:?}", n, from_utf8(input));
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
//            println!("Assignment error: {:?}. Input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
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
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
            IResult::Error(e)
        }
    };
    return node;
}

// named!(and_rule<&[u8], (Box<Expression>, Option<Box<Expression>>)>,
//     tuple!(
//         bool_expr_ast,
//         opt!(preceded!(
//             inline_wrapped(tag!("and")),
//             alt!(and_expr_ast | bool_expr_ast)
//         ))
//     )
// );


fn and_expr_ast(input: &[u8]) -> IResult<&[u8], Box<Expression>> {
    let parse_result = tuple!(input,
        bool_expr_ast,
        opt!(complete!(preceded!(
            delimited!(many1!(tag!(" ")), tag!("and"), many1!(tag!(" "))),
            alt!(and_expr_ast | bool_expr_ast)
        )))
    );
    // let parse_result = and_rule(input);

    let node = match parse_result {
        Done(i, o) => {
            match o.1 {

                Some(x) => {
                    let bin_exp = Box::new(BinaryExpression{
                        operator: BinaryOperator::And,
                        left: o.0,
                        right: x
                    }) as Box<Expression>;
                    Done(i, bin_exp)
                } ,
                None => Done(i, o.0)
            }
        },
        // TODO: Error type
        IResult::Incomplete(x) => {
//             println!("And expr incomplete: {:?}. Input was: {:?}", x, from_utf8(input));
             IResult::Incomplete(x)
        },
        IResult::Error(e) => {
//            println!("and expr error: {:?}. input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    };
    return node;
}

named!(boolean_rule<&[u8],(&[u8])>,
    alt!(
        terminated!(tag!("true"), peek!(follow_value)) |
        terminated!(tag!("false"), peek!(follow_value))
    )
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
        IResult::Incomplete(x) => {
             println!("Bool expr incomplete: {:?}. Input was: {:?}", x, from_utf8(input));
             IResult::Incomplete(x)
        },
        IResult::Error(e) => {
            println!("Bool expr error: {:?}. input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
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

