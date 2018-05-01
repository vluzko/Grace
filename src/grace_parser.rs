use grace_error::*;
use std::str;
use std::io::prelude::*;
use std::fs::File;
use std::str::from_utf8;


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

macro_rules! keyword (
  ($i:expr, $f:expr) => (
    {
      delimited!($i, many1!(inline_whitespace_char), tag!($f), many1!(inline_whitespace_char))
    }
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

named!(inline_whitespace_char<&[u8], &[u8]>,
    tag!(" ")
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

pub fn reserved_list() -> Vec<&'static str>{
    let list: Vec<&'static str> = vec!("if", "else", "elif", "for", "while", "and", "or", "not", "xor", "fn", "import", "true", "false");
    return list;
}

/// Return true if a key
/// 
fn reserved_words(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let tag_lam = |x: &[u8]| recognize!(input, complete!(tag!(x)));
    let list = reserved_list();
    let tag_iter = list.iter().map(|x| x.as_bytes()).map(tag_lam);
    let mut final_result: IResult<&[u8], &[u8]> = IResult::Error(ErrorKind::Tag);
    for res in tag_iter {
        match res {
            Done(i, o) => {
                final_result = Done(i, o);
                break;
            },
            _ => continue
        }; 
    }
    return final_result;
}

fn block_rule(input: &[u8], minimum_indent: usize) -> IResult<&[u8], Vec<Box<Stmt>>> {
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

// TODO: just make it a size and pass 0 instead of None
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

fn statement_ast(input: &[u8], indent: usize) -> IResult<&[u8], Box<Stmt>> {
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

fn if_ast(input: &[u8], indent: usize) -> IResult<&[u8], Box<Stmt>> {

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
            let if_statement = Stmt::IfStmt{condition: o.1, main_block: o.4, elifs: o.5, else_block: o.6};
            Done(i, Box::new(if_statement))
        },
        IResult::Incomplete(n) => {
           println!("if incomplete: {:?}. Input was: {:?}", n, from_utf8(input));
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
           println!("if error: {:?}. Input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    };

    return node;
}

fn elif_rule(input: &[u8], indent: usize) -> IResult<&[u8], (Expr, Box<Block>)> {
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

fn function_declaration_ast(input: &[u8], indent: usize) -> IResult<&[u8], Box<Stmt>> {
    let full_tuple = tuple!(input,
        tag!("fn "),
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
            let func_dec = Box::new(Stmt::FunctionDecStmt{name: o.1, args: o.3, body: o.7});
            Done(i, func_dec)
        },
         IResult::Incomplete(n) => {
//            println!("Function incomplete: {:?}. Input was: {:?}", n, from_utf8(input));
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
//            println!("Function error {:?}. Input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    };

    return node;
}

fn assignment_ast(input: &[u8]) -> IResult<&[u8], Box<Stmt>> {
    let parse_result = terminated!(input, tuple!(
        identifier_ast,
        inline_wrapped!(tag!("=")),
        expression_ast
    ), eof_or_line);
    let node = match parse_result {
        Done(i,o) => {
            let val = Box::new(Stmt::AssignmentStmt{identifier: o.0, expression: o.2});
            Done(i, val)
        },
        IResult::Incomplete(n) => {
            println!("Assignment incomplete: {:?}. Input was: {:?}", n, from_utf8(input));
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
            println!("Assignment error: {:?}. Input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    };

    return node;
}

fn expression_ast(input: &[u8]) -> IResult<&[u8], Expr> {
    return match alt!(input,
        comparison_ast
    ) {
        Done(i, o) => {
            Done(i, o)
        },
        IResult::Incomplete(n) => {
            println!("Expression incomplete: {:?}. Input was: {:?}", n, from_utf8(input));
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
            println!("Expression error: {:?}. Input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    }
}

fn function_call_expr(input: &[u8]) -> IResult<&[u8], Expr> {
    let parse_result = tuple!(input,
        inline_wrapped!(identifier_ast),
        tag!("("),
        inline_wrapped!(separated_list_complete!(inline_wrapped!(tag!(",")), identifier_ast)),
        inline_wrapped!(tag!(")"))
    );

    return match parse_result {
        Done(i, o) => {
            Done(i, Expr::FunctionCall{name: o.0, args: o.2})
        },
        IResult::Incomplete(n) => {
           println!("Function call incomplete: {:?}. Input was: {:?}", n, from_utf8(input));
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
           println!("Function call error: {:?}. Input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    }
}

fn identifier_expr(input: &[u8]) -> IResult<&[u8], Expr> {
    let parse_result = identifier(input);
    let node = match parse_result {
        Done(i,o) => {
            let val = match from_utf8(o) {
                Ok(v) => v,
                x => panic!()
            };
            let ident: Identifier = Identifier{name: val.to_string()};
            Done(i, Expr::IdentifierExpr {ident})
        },
        IResult::Incomplete(n) => {
//            println!("Identifier incomplete: {:?}. Input was: {:?}", n, from_utf8(input));
            IResult::Incomplete(n)
        },
        IResult::Error(e) => {
//            println!("Identifier error: {:?}. Input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    };
    return node;
}

named!(identifier<&[u8], &[u8]>, 
    recognize!(
        pair!(
            not!(peek!(reserved_words)),
            pair!(
                alt!(alpha | tag!("_")),
                many0!(alt!(alpha | tag!("_") | digit))
            )
        )
    )
);

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
            Done(i, ident)
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

named!(comparisons<&[u8], &[u8]>,
    recognize!(alt!(
        tag!("==") |
        tag!("<=") |
        tag!(">=") |
        tag!("!=") |
        tag!("<")  |
        tag!(">")
    ))
);

fn match_binary_expr(operator: BinaryOperator, output: (Expr, Option<Expr>)) -> Expr {
    match output.1 {
        Some(x) => Expr::BinaryExpr {operator, left: Box::new(output.0), right: Box::new(x)},
        None => output.0
    }
}

fn comparison_ast(input: &[u8]) -> IResult<&[u8], Expr> {
    let parse_result = tuple!(input,
        and_expr_ast,
        opt!(complete!(tuple!(
            inline_wrapped!(comparisons),
            and_expr_ast
        )))
    );

    let node = match parse_result {
        Done(i, o) => {
            let expression = match (o.1) {
                None => o.0,
                Some(x) => {
                    let operator = match from_utf8(x.0) {
                        Ok("==") => ComparisonOperator::Equal,
                        Ok(">=") => ComparisonOperator::GreaterEqual,
                        Ok("<=") => ComparisonOperator::LessEqual,
                        Ok(">")  => ComparisonOperator::Greater,
                        Ok("<")  => ComparisonOperator::Less,
                        Ok("!=") => ComparisonOperator::Unequal,
                        _ => panic!(),
                    };
                    Expr::ComparisonExpr{operator, left: Box::new(o.0), right: Box::new(x.1)}
                }
            };
            Done(i, expression)
        },
        IResult::Incomplete(x) => {
            println!("Comparison expr incomplete: {:?}. Input was: {:?}", x, from_utf8(input));
            IResult::Incomplete(x)
        },
        IResult::Error(e) => {
            println!("Comparison expr error: {:?}. input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    };
    return node;
}

fn and_expr_ast(input: &[u8]) -> IResult<&[u8], Expr> {
    let parse_result = tuple!(input,
        or_expr_ast,
        opt!(complete!(preceded!(
            keyword!("and"),
            and_expr_ast  // TODO: this is broken. It should look for an "atomic" expression (fn call, identifier, value).
        )))
    );

    let node = match parse_result {
        Done(i, o) => {
            Done(i, match_binary_expr(BinaryOperator::And, o ))
        },
        // TODO: Error type
        IResult::Incomplete(x) => {
            println!("And expr incomplete: {:?}. Input was: {:?}", x, from_utf8(input));
            IResult::Incomplete(x)
        },
        IResult::Error(e) => {
            println!("And expr error: {:?}. input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    };
    return node;
}

fn or_expr_ast(input: &[u8]) -> IResult<&[u8], Expr> {
    let parse_result = tuple!(input,
        atomic_expr_ast,
        opt!(complete!(preceded!(
            keyword!("or"),
            or_expr_ast  // TODO: this is broken. It should look for an "atomic" expression (fn call, identifier, value).
        )))
    );

    let node = match parse_result {
        Done(i, o) => {
            Done(i, match_binary_expr(BinaryOperator::Or, o ))
        },
        // TODO: Error type
        IResult::Incomplete(x) => {
            println!("Or expr incomplete: {:?}. Input was: {:?}", x, from_utf8(input));
            IResult::Incomplete(x)
        },
        IResult::Error(e) => {
            println!("Or expr error: {:?}. input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        }
    };
    return node;
}

fn atomic_expr_ast(input: &[u8]) -> IResult<&[u8], Expr> {
    return match alt!(input, 
        bool_expr_ast |
        complete!(function_call_expr) |
        identifier_expr
    ) {
        Done(i, o) => {
            Done(i, o)
        },
        IResult::Incomplete(x) => {
            println!("Atomic expr incomplete: {:?}. Input was: {:?}", x, from_utf8(input));
            IResult::Incomplete(x)
        },
        IResult::Error(e) => {
            println!("Atomic expr error: {:?}. input was: {:?}", e, from_utf8(input));
            IResult::Error(e)
        } 
    }
}

fn attribute_access_ast(input: &[u8]) -> IResult<&[u8], Expr> {
    let result = match tuple!(input,
        atomic_expr_ast,
        opt!(complete!(tuple!(
            inline_wrapped!(tag!(".")),
            atomic_expr_ast
        )))
    ) { 
     x => x   
    };
    panic!();
}


fn bool_expr_ast(input: &[u8]) -> IResult<&[u8], Expr> {
    let parse_result= alt!(input,
        terminated!(tag!("true"), peek!(follow_value)) |
        terminated!(tag!("false"), peek!(follow_value))
    );

    let node = match parse_result {
        Done(i, o) => {
            match from_utf8(o) {
                Ok("true") => Done(i, Expr::Bool(Boolean::True)),
                Ok("false") => Done(i, Expr::Bool(Boolean::False)),
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

fn read_from_file(f_name: &str) -> String {
    let filename= format!("./test_data/{}.gr", f_name);
    let mut f = File::open(filename).expect("File not found");
    let mut contents = String::new();
    f.read_to_string(&mut contents);
    return contents;
}

// #[test]
pub fn basic_file_test() {
    let contents = read_from_file("simple_grace");
    let result = parse_grace(contents.as_str());

    match result {
        Done(i, o) => println!("{}", (*o).to_string()),
        _ => panic!()
    }
}

// #[test]
pub fn small_file_test() {
    let contents = read_from_file("small_grace");
    let result = parse_grace(contents.as_str());

    match result {
        Done(_, o) => println!("{}", (*o).to_string()),
        _ => panic!()
    }
}


#[test]
pub fn test_assignment() {
    let input = "foo = true";
    let result = assignment_ast(input.as_bytes());
    let assignment = Stmt::AssignmentStmt{
        identifier: Identifier{name: "foo".to_string()},
        expression: Expr::Bool(Boolean::True)
    };
    // TODO: Implement Eq, then reactivate this test.
   assert_eq!(result, Done("".as_bytes(), Box::new(assignment)));
}

#[test]
pub fn test_reserved_words() {
    for keyword in reserved_list() {
        let result = identifier(keyword.as_bytes());
        assert_eq!(result, IResult::Error(ErrorKind::Not));
    }
}

#[test]
pub fn test_function_call() {
    let function_call = expression_ast("ident()".as_bytes());
    let expected = Expr::FunctionCall{name: Identifier{name: "ident".to_string()}, args: vec!()};
    assert_eq!(function_call, Done("".as_bytes(), expected));

}

#[test]
pub fn test_binary_expr() {
    let binary_exprs = expression_ast("true and false or true".as_bytes());
    let expected = Expr::BinaryExpr{
        operator: BinaryOperator::And, 
        left: Box::new(Expr::Bool(Boolean::True)),
        right:Box::new(Expr::BinaryExpr{
            operator: BinaryOperator::Or, 
            left: Box::new(Expr::Bool(Boolean::False)), 
            right: Box::new(Expr::Bool(Boolean::True))
            })
    };
    assert_eq!(binary_exprs, Done("".as_bytes(), expected));
}

#[test]
pub fn test_identifier_expr() {
    let identifier_expr = expression_ast("words".as_bytes());
    let expected = Expr::IdentifierExpr{ident: Identifier{name: "words".to_string()}};
    assert_eq!(identifier_expr, Done("".as_bytes(), expected));
}

#[test]
pub fn test_comparison_expr() {
    let comp_strs = vec![">", "<", ">=", "<=", "==", "!="];
    let comp_ops = vec![ComparisonOperator::Greater, ComparisonOperator::Less, ComparisonOperator::GreaterEqual, 
        ComparisonOperator::LessEqual, ComparisonOperator::Equal, ComparisonOperator::Unequal];
    for (comp_str, comp_op) in comp_strs.iter().zip(comp_ops.iter()) {
        let as_str = format!("true {} false", comp_str);
        let expr = expression_ast(as_str.as_bytes());
        let expected = Expr::ComparisonExpr{
            left: Box::new(Expr::Bool(Boolean::True)),
            right: Box::new(Expr::Bool(Boolean::False)), 
            operator: *comp_op
        };

        assert_eq!(expr, Done("".as_bytes(), expected));
    }
}

#[test]
pub fn test_repeated_func_calls() {
    let rep = expression_ast("func(a)(b, c)".as_bytes());
    let expected = Expr::FunctionCall{
        name: Identifier{name: "closure".to_string()},
        args: vec!(Identifier{name: "b".to_string()}, Identifier{name: "c".to_string()})
    };
}