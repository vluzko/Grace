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

pub fn parse_grace(input: &str) -> IResult<&[u8], Box<ASTNode>> {
    parse_grace_from_slice(input.as_bytes())
}

// This is the important function
pub fn parse_grace_from_slice(input: &[u8]) -> IResult<&[u8], Box<ASTNode>> {

    let output = terminated!(input, call!(block_ast, None), custom_eof);
    return match output {
        Done(i, o) => Done(i, o as Box<ASTNode>),
        IResult::Incomplete(n) => IResult::Incomplete(n),
        IResult::Error(e) => IResult::Error(e)
    };
}

/// A macro for wrapping a parser in inline whitespace.
/// Similar to ws!, but doesn't allow for \n, \r, or \t.
macro_rules! inline_wrapped(
  ($i:expr, $submac:ident!( $($args:tt)* )) => (
    {
      match tuple!($i, inline_whitespace, $submac!($($args)*), inline_whitespace) {
        IResult::Error(a)      => IResult::Error(a),
        IResult::Incomplete(i) => IResult::Incomplete(i),
        IResult::Done(remaining, (_,o, _))    => {
            IResult::Done(remaining, o)
        }
      }
    }
  );

  ($i:expr, $f:expr) => (
    inline_wrapped!($i, call!($f));
  );
);

macro_rules! indented(
  ($i:expr, $submac:ident!( $($args:tt)* ), $ind:expr) => (
    preceded!($i, complete!(many_m_n!($ind, $ind, tag!(" "))), $submac!($($args)*))
  );

  ($i:expr, $f:expr, $ind: expr) => (
    indented!($i, call!($f), $ind);
  );
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

named!(whitespace_char<&[u8], &[u8]>,
    alt!(custom_eof | tag!("\n") | tag!(" "))
);

named!(inline_whitespace<&[u8], Vec<&[u8]>>,
    many0!(tag!(" "))
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
        // We end up reparsing the initial indent, but that's okay. The alternative is joining two
        // vectors, which is much slower.
        // TODO: See if we can clean this up with a separated_list_complete.
        let statements = delimited!(input, 
            opt!(between_statement),
            many1!(
                complete!(
                    preceded!(
                        between_statement,
                        indented!(call!(statement_ast, expected_indent), expected_indent)
                    )
                )
            ),
            between_statement
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
            IResult::Error(e)
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
    let n = alt!(input,
        assignment_ast |
        call!(if_ast, indent) |
        call!(function_declaration_ast, indent)
    );

    return match n {
        Done(i, o) => {
            Done(i, o)
        },
        IResult::Incomplete(n) => {
            println!("Statement incomplete: {:?}. Input was: {:?}", n, from_utf8(input));
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
            println!("Statement error {:?}. Input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    }
}

fn if_ast(input: &[u8], indent: usize) -> IResult<&[u8], Box<Statement>> {

    let full_tuple = tuple!(
        input,
        tag!("if"),
        ws!(and_expr_ast),
        tag!(":"),
        newline,
        call!(block_ast, Some(indent + 1)),
        many0!(call!(elif_rule, indent)),
        opt!(complete!(call!(else_rule, indent)))
    );

    let node = match full_tuple {
        Done(i, o) => {
            let if_statement = IfStatement{condition: o.1, main_block: o.4, elifs: o.5, else_block: o.6};
            Done(i, Box::new(if_statement) as Box<Statement>)
        },
        IResult::Incomplete(n) => {
//            println!("if incomplete: {:?}. Input was: {:?}", n, from_utf8(input));
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
//            println!("if error: {:?}. Input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    };

    return node;
}

fn elif_rule(input: &[u8], indent: usize) -> IResult<&[u8], (Box<Expression>, Box<Block>)> {
    let elif_tuple = tuple!(
        input,
        indented!(tag!("elif"), indent),
        inline_wrapped!(and_expr_ast),
        tag!(":"),
        newline,
        call!(block_ast, Some(indent + 1))
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
    let else_tuple = tuple!(
        input,
        indented!(tag!("else"), indent),
        tag!(":"),
        newline,
        call!(block_ast, Some(indent + 1))
    );
    return match else_tuple {
        Done(i,o) => {
            Done(i, o.3)
        },
        IResult::Incomplete(n) => {
//            println!("else incomplete: {:?}. Input was: {:?}", n, from_utf8(input));
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
//            println!("else error {:?}. Input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    };
}


fn function_declaration_ast(input: &[u8], indent: usize) -> IResult<&[u8], Box<Statement>> {
    let full_tuple = tuple!(input,
        tag!("fn"),
        inline_wrapped!(identifier_ast),
        tag!("("),
        inline_wrapped!(separated_list_complete!(inline_wrapped!(tag!(",")), identifier_ast)),
        tag!(")"),
        inline_wrapped!(tag!(":")),
        newline,
        call!(block_ast, Some(indent + 1))
    );

    let node = match full_tuple {
        Done(i, o) => {
            let func_dec = Box::new(FunctionDec{name: o.1, args: o.3, body: o.7}) as Box<Statement>;
            Done(i, func_dec)
        },
         IResult::Incomplete(n) => {
            println!("Function incomplete: {:?}. Input was: {:?}", n, from_utf8(input));
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
            println!("Function error {:?}. Input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    };

    return node;
}

fn assignment_ast(input: &[u8]) -> IResult<&[u8], Box<Statement>> {
    let parse_result = terminated!(input, tuple!(
        identifier_ast,
        ws!(tag!("=")),
        and_expr_ast
    ), eof_or_line);
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

fn identifier_ast(input: &[u8]) -> IResult<&[u8], Identifier> {
    let parse_result = recognize!(input,
        pair!(
            alt!(alpha | tag!("_")),
            many0!(alt!(alpha | tag!("_") | digit))
            )
    );
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
            println!("Identifier incomplete: {:?}. Input was: {:?}", n, from_utf8(input));
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
            println!("Identifier error: {:?}. Input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    };
    return node;
}

fn and_expr_ast(input: &[u8]) -> IResult<&[u8], Box<Expression>> {
    let parse_result = tuple!(input,
        bool_expr_ast,
        opt!(complete!(preceded!(
            delimited!(many1!(tag!(" ")), tag!("and"), many1!(tag!(" "))),
            alt!(and_expr_ast | bool_expr_ast)
        )))
    );

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

fn bool_expr_ast(input: &[u8]) -> IResult<&[u8], Box<Expression>> {
    let parse_result= alt!(input,
        terminated!(tag!("true"), peek!(follow_value)) |
        terminated!(tag!("false"), peek!(follow_value))
    );

    let node = match parse_result {
        Done(i,o) => {
            match from_utf8(o) {
                Ok("true") => Done(i, Box::new(Boolean::True) as Box<Expression>),
                Ok("false") => Done(i, Box::new(Boolean::False) as Box<Expression>),
                _ => panic!(),
            }
        },
        IResult::Incomplete(x) => {
//             println!("Bool expr incomplete: {:?}. Input was: {:?}", x, from_utf8(input));
             IResult::Incomplete(x)
        },
        IResult::Error(e) => {
//            println!("Bool expr error: {:?}. input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    };
    return node;
}

fn read_from_file(f_name: &str) -> String {
    let filename= format!("./test_data/{}.gr", f_name);
    let mut f = File::open(filename).expect("File not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents);
    return contents;
}

#[test]
pub fn basic_file_test() {
    let contents = read_from_file("simple_grace");
    let result = parse_grace(contents.as_str());

    match result {
        Done(i, o) => println!("{}", (*o).to_string()),
        _ => panic!()
    }
}

#[test]
pub fn small_file_test() {
    let contents = read_from_file("small_grace");
    let result = parse_grace(contents.as_str());

    match result {
        Done(i, o) => println!("{}", (*o).to_string()),
        _ => panic!()
    }
}


#[test]
pub fn test_assignment() {
    let input = "foo = true";
    let result = parse_grace(input);
    let assignment = Assignment{
        identifier: Identifier{name: "foo".to_string()},
        expression: Box::new(Boolean::True)
    };
    // TODO: Implement Eq, then reactivate this test.
//    assert_eq!(result, Done("", assignment));
}

