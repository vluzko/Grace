use std::str;
use std::io::prelude::*;
use std::fs::File;
use std::str::from_utf8;
use std::fmt::Debug;
use std::collections::HashMap;
use rand;


extern crate cute;
extern crate nom;
use self::nom::*;
use self::nom::IResult::Done as Done;
use expression::*;

type ExprRes<'a> = IResult<&'a [u8], Expr>;

// TODO: Move to a utils file
/// Map the contents of an IResult.
/// Rust functors plox
pub fn fmap_iresult<X, T, F>(res: IResult<&[u8], X>, func: F) -> IResult<&[u8], T>
    where F: Fn(X) -> T {
    return match res {
        Done(i, o) => Done(i, func(o)),
        IResult::Error(e) => IResult::Error(e),
        IResult::Incomplete(n) => IResult::Incomplete(n)
    };
}

pub fn output<T>(res: IResult<&[u8], T>) -> T {
    return match res {
        Done(_, o) => o,
        IResult::Error(e) => {
            println!("Output error: {:?}.", e);
            panic!()
        },
        _ => panic!()
    };
}

pub fn fmap_and_full_log<'a, X, T>(res: IResult<&'a [u8], X>, func: fn(X) -> T, name: &str, input: &[u8]) -> IResult<&'a [u8], T> {
    println!("{} input was: {:?}", name, from_utf8(input));
    return match res {
        Done(i, o) => {
            println!("{} leftover input is {:?}", name, from_utf8(i));
            Done(i, func(o))
        },
        IResult::Error(e) => {
            println!("{} error: {}. Input was: {:?}", name, e, from_utf8(input));
            IResult::Error(e)
        },
        IResult::Incomplete(n) => {
            println!("{} incomplete: {:?}. Input was: {:?}", name, n, from_utf8(input));
            IResult::Incomplete(n)
        }
    };
}

// TODO: Change
/// Map an IResult and log errors and incomplete values.
pub fn fmap_and_log<'a, X, T>(res: IResult<&'a [u8], X>, func: fn(X) -> T, name: &str, input: &[u8]) -> IResult<&'a [u8], T> {
    return match res {
        Done(i, o) => Done(i, func(o)),
        IResult::Error(e) => {
            println!("{} error: {}. Input was: {:?}", name, e, from_utf8(input));
            IResult::Error(e)
        },
        IResult::Incomplete(n) => {
            println!("{} incomplete: {:?}. Input was: {:?}", name, n, from_utf8(input));
            IResult::Incomplete(n)
        }
    };
}

pub fn full_log<'a, X>(res: IResult<&'a [u8], X>, name: &str, input: &[u8]) -> IResult<&'a [u8], X> {
    return fmap_and_full_log(res, |x| x, name, input);
}

pub fn log_err<'a, X>(res: IResult<&'a [u8], X>, name: &str, input: &[u8]) -> IResult<&'a [u8], X> {
    return fmap_and_log(res, |x| x, name, input);
}

pub fn parse_grace(input: &str) -> IResult<&[u8], Box<ASTNode>> {
    parse_grace_from_slice(input.as_bytes())
}

// This is the important function
pub fn parse_grace_from_slice(input: &[u8]) -> IResult<&[u8], Box<ASTNode>> {

    let output = terminated!(input, call!(block_ast, 0), custom_eof);
    return match output {
        Done(i, o) => Done(i, Box::new(o) as Box<ASTNode>),
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

named!(valid_identifier_char<&[u8], &[u8]>,
    alt!(alpha | tag!("_") | digit)
);

/// Matches a keyword within a line.
/// Used for "and", "or", "xor", "in", etc.
macro_rules! inline_keyword (
  ($i:expr, $f:expr) => (
    {
      delimited!($i,
        inline_whitespace,
        tag!($f),
        preceded!(not!(valid_identifier_char), alt!(recognize!(many1!(inline_whitespace_char)) | peek!(tag!("(")))))
    }
  );
);

/// Check that a macro is indented correctly.
macro_rules! indented(
  ($i:expr, $submac:ident!( $($args:tt)* ), $ind:expr) => (
    preceded!($i, complete!(many_m_n!($ind, $ind, tag!(" "))), $submac!($($args)*))
  );

  ($i:expr, $f:expr, $ind: expr) => (
    indented!($i, call!($f), $ind);
  );
);

named!(ending_colon <&[u8], &[u8]>,
    terminated!(
        inline_wrapped!(tag!(":")),
        newline
    )
);

named!(inline_whitespace_char<&[u8], &[u8]>,
    tag!(" ")
);

named!(inline_whitespace<&[u8], Vec<&[u8]>>,
    many0!(tag!(" "))
);

named!(num_follow<&[u8], &[u8]> ,
    peek!(alt!(custom_eof | tag!(" ") | tag!("(") | tag!(")") | tag!(":")))
);

named!(dec_digit<&[u8], &[u8]>,
    recognize!(alt!(
        tag!("0") |
        tag!("1") |
        tag!("2") |
        tag!("3") |
        tag!("4") |
        tag!("5") |
        tag!("6") |
        tag!("7") |
        tag!("8") |
        tag!("9")
    ))
);

named!(dec_seq<&[u8], &[u8]>,
    recognize!(many1!(dec_digit))
);

named!(sign<&[u8], &[u8]>,
    recognize!(alt!(tag!("+") | tag!("-")))
);

named!(exponent<&[u8], (Option<&[u8]>, &[u8])>,
    preceded!(
        alt!(tag!("e") | tag!("E")),
        tuple!(
            opt!(sign),
            dec_seq
        )
    )
);

fn between_statement(input: &[u8]) -> IResult<&[u8], Vec<Vec<&[u8]>>> {
    let n = many0!(input,
        terminated!(many0!(tag!(" ")), alt!(custom_eof | tag!("\n")))
    );
    return n;
}

fn custom_eof(input: &[u8]) -> IResult<&[u8], &[u8]> {
    return eof!(input, );
}

pub fn reserved_list() -> Vec<&'static str>{
    let list: Vec<&'static str> = vec!("if", "else", "elif", "for", "while", "and", "or", "not", "xor", "fn", "import", "true", "false", "in");
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

// TODO: Merge block_rule with block_ast
fn block_rule(input: &[u8], minimum_indent: usize) -> IResult<&[u8], Vec<Stmt>> {
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
fn block_ast(input: &[u8], indent: usize) -> IResult<&[u8], Block> {
    let parse_result = block_rule(input, indent);
    return fmap_iresult(parse_result, |x| Block{statements: x});
}

fn eof_or_line(input: &[u8]) -> IResult<&[u8], &[u8]> {
    return alt!(input, custom_eof | tag!("\n"));
}

fn statement_ast(input: &[u8], indent: usize) -> IResult<&[u8], Stmt> {
    let node = alt!(input,
        assignment_ast |
        call!(while_ast, indent) |
        call!(for_in_ast, indent) |
        call!(if_ast, indent) |
        call!(function_declaration_ast, indent) |
        call!(import) |
        call!(return_stmt)
    );

    return node;
}

fn import(input: &[u8]) -> IResult<&[u8], Stmt> {
    let parse_result = tuple!(input, inline_keyword!("import"), dotted_identifier);
    return fmap_iresult(parse_result,|x| Stmt::ImportStmt {module: x.1});
}

fn return_stmt(input: &[u8]) -> IResult<&[u8], Stmt> {
    let parse_result = tuple!(input, inline_keyword!("return"), expression_ast);
    return fmap_iresult(parse_result,|x| Stmt::ReturnStmt {value: x.1});
}

/// Parse a while loop.
fn while_ast(input: &[u8], indent: usize) -> IResult<&[u8], Stmt> {
    let parse_result = tuple!(
        input,
        tag!("while "),
        inline_wrapped!(expression_ast),
        tag!(":"),
        newline,
        call!(block_ast, indent+1)
    );

    return fmap_iresult(parse_result, |x| Stmt::WhileStmt {condition: x.1, block: x.4});
}

/// Parse a for in loop.
fn for_in_ast(input: &[u8], indent: usize) -> IResult<&[u8], Stmt> {
    let parse_result = tuple!(input,
        delimited!(
            tag!("for "),
            inline_wrapped!(identifier_ast),
            inline_keyword!("in")
        ),
        terminated!(
            inline_wrapped!(expression_ast),
            ending_colon
        ),
        call!(block_ast, indent+1)
    );

    return fmap_iresult(parse_result, |x| Stmt::ForInStmt {iter_var: x.0, iterator: x.1, block: x.2});
}

fn if_ast(input: &[u8], indent: usize) -> IResult<&[u8], Stmt> {
    // TODO: Should be expression_ast.
    let parse_result = tuple!(
        input,
        tag!("if"),
        many1!(inline_whitespace_char),
        inline_wrapped!(expression_ast),
        inline_wrapped!(tag!(":")),
        newline,
        call!(block_ast, indent + 1),
        many0!(call!(elif_rule, indent)),
        opt!(complete!(call!(else_rule, indent)))
    );

    let node = fmap_iresult(parse_result, |x|Stmt::IfStmt{condition: x.2, main_block: x.5, elifs: x.6, else_block: x.7});

    return node;
}

fn elif_rule(input: &[u8], indent: usize) -> IResult<&[u8], (Expr, Block)> {
    let parse_result = tuple!(
        input,
        indented!(tag!("elif"), indent),
        many1!(inline_whitespace_char),
        inline_wrapped!(and_expr_ast),
        inline_wrapped!(tag!(":")),
        newline,
        call!(block_ast, indent + 1)
    );

    let node = fmap_iresult(parse_result, |x| (x.2, x.5));
    return node;
}

fn else_rule(input: &[u8], indent: usize) -> IResult<&[u8], Block> {
    let parse_result = tuple!(
        input,
        indented!(tag!("else"), indent),
        inline_wrapped!(tag!(":")),
        newline,
        call!(block_ast, indent + 1)
    );

    let node = fmap_iresult(parse_result, |x| x.3);

    return node;
}

fn function_declaration_ast(input: &[u8], indent: usize) -> IResult<&[u8], Stmt> {
    let full_tuple = tuple!(input,
        tag!("fn "),
        inline_wrapped!(identifier_ast),
        tag!("("),
        inline_wrapped!(separated_list_complete!(inline_wrapped!(tag!(",")), identifier_ast)),
        tag!(")"),
        inline_wrapped!(tag!(":")),
        newline,
        call!(block_ast, indent + 1)
    );

    let node = fmap_iresult(full_tuple, |x| Stmt::FunctionDecStmt{name: x.1, args: x.3, body: x.7});

    return node;
}

fn assignment_ast(input: &[u8]) -> IResult<&[u8], Stmt> {
    let parse_result = terminated!(input, tuple!(
        identifier_ast,
        inline_wrapped!(assignments),
        expression_ast
    ), eof_or_line);

    let node = fmap_iresult(parse_result, |x| Stmt::AssignmentStmt{
        identifier: x.0, operator:Assignment::from(from_utf8(x.1).unwrap()), expression: x.2});

    return node;
}

fn expression_ast(input: &[u8]) -> ExprRes {
    let node = alt!(input,
        comparison_ast
    );

    return node;
}


named!(assignments<&[u8], &[u8]>,
    recognize!(alt!(
        tag!("=")   |
        tag!("+=")  |
        tag!("-=")  |
        tag!("*=")  |
        tag!("/=")  |
        tag!("**=") |
        tag!("%=")  |
        tag!(">>=") |
        tag!("<<=") |
        tag!("|=")  |
        tag!("&=")  |
        tag!("^=")
    ))
);

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
    return match output.1 {
        Some(x) => Expr::BinaryExpr {operator, left: Box::new(output.0), right: Box::new(x)},
        None => output.0
    };
}

/// Create a binary expression, where one of several operators is possible.
fn match_binary_exprs(operators: &HashMap<&[u8], BinaryOperator>, output: (Expr, Option<(&[u8], Expr)>)) -> Expr {
    return match output.1 {
        Some(x) => {
            let op: BinaryOperator = *operators.get(x.0).unwrap();
            Expr::BinaryExpr {operator: op, left: Box::new(output.0), right: Box::new(x.1)}
        },
        None => output.0
    };
}

/// Create a binary expression, where one of several operators is possible.
fn match_unary_expr(operator: UnaryOperator, output: (Option<&[u8]>, Expr)) -> Expr {
    return match output.0 {
        Some(x) => {
            assert_eq!(x, operator.to_string().as_bytes());
            Expr::UnaryExpr {operator: operator, operand: Box::new(output.1)}
        },
        None => output.1
    };
}

fn comparison_ast(input: &[u8]) -> ExprRes {
    let parse_result = tuple!(input,
        and_expr_ast,
        opt!(complete!(tuple!(
            inline_wrapped!(comparisons),
            and_expr_ast
        )))
    );

    let map = |x: (Expr, Option<(&[u8], Expr)>)| match x.1 {
        None => x.0,
        Some(y) => {
            let operator = match from_utf8(y.0) {
                Ok("==") => ComparisonOperator::Equal,
                Ok(">=") => ComparisonOperator::GreaterEqual,
                Ok("<=") => ComparisonOperator::LessEqual,
                Ok(">")  => ComparisonOperator::Greater,
                Ok("<")  => ComparisonOperator::Less,
                Ok("!=") => ComparisonOperator::Unequal,
                _ => panic!(),
            };
            Expr::ComparisonExpr{operator, left: Box::new(x.0), right: Box::new(y.1)}
        }
    };

    let node = fmap_iresult(parse_result, map);
    return node;
}

/// Match any of a list of strings. Return the matched string.
fn match_any<'a>(input: &'a[u8], keywords: &Vec<&str>) -> IResult<&'a[u8], &'a[u8]> {
    let tag_lam = |x: &[u8]| recognize!(input, complete!(tag!(x)));
    let tag_iter = keywords.iter().map(|x| x.as_bytes()).map(tag_lam);
    let mut ret = IResult::Error(ErrorKind::Tag);
    for res in tag_iter {
        match res {
            Done(i, o) => ret = Done(i, o),
            _ => continue
        };
    }
    return ret
}

/// Match a binary expression whose operator is a keyword.
fn binary_op_keyword<'a>(input: &'a [u8], symbol: &str, operator: BinaryOperator, next_expr: fn(&[u8]) -> ExprRes) -> IResult<&'a [u8], Expr> {
    let parse_result = tuple!(input,
        next_expr,
        opt!(complete!(preceded!(
            inline_keyword!(symbol),
            call!(binary_op_keyword, symbol, operator, next_expr)
        )))
    );

    let node = fmap_iresult(parse_result, |x| match_binary_expr(operator, x));
    return node;
}

/// Match a binary expression whose operator is a symbol.
fn binary_op_symbol<'a>(input: &'a [u8], symbol: &str, operator: BinaryOperator, next_expr: fn(&[u8]) -> ExprRes) -> IResult<&'a [u8], Expr> {
    let parse_result = tuple!(input,
        next_expr,
        opt!(complete!(preceded!(
            inline_wrapped!(tag!(symbol)),
            call!(binary_op_symbol, symbol, operator, next_expr)
        )))
    );

    let node = fmap_iresult(parse_result, |x| match_binary_expr(operator, x));
    return node;
}

fn binary_op_list<'a>(input: &'a [u8], symbols: &Vec<&str>, operators: &HashMap<&[u8], BinaryOperator>, next_expr: fn(&[u8]) -> ExprRes) -> IResult<&'a [u8], Expr> {
    let parse_result = tuple!(input,
        next_expr,
        opt!(tuple!(
            inline_wrapped!(call!(match_any, symbols)),
            call!(binary_op_list, symbols, operators, next_expr)
        ))
    );

    let node = fmap_iresult(parse_result, |x| match_binary_exprs(operators, x));
    return node;
}

fn unary_expr(input: & [u8]) -> IResult<& [u8], Expr> {
    let parse_result: IResult<&[u8], (Option<&[u8]>, Expr)> = alt!(input,
        tuple!(
            map!(inline_wrapped!(alt!(tag!("+") | tag!("-") | tag!("~") | inline_keyword!("not"))), Some),
            unary_expr)
         |
        tuple!(
            value!(None, tag!("")),
            power_expr_ast
        )
    );

    let node = fmap_iresult(parse_result, |x: (Option<&[u8]>, Expr)|
        match x.0 {
            Some(y) => {
                let unary_op = match from_utf8(y).unwrap() {
                    "+" => UnaryOperator::Positive,
                    "-" => UnaryOperator::Negative,
                    "~" => UnaryOperator::BitNot,
                    "not" => UnaryOperator::Not,
                    _ => panic!()

                };
                Expr::UnaryExpr {operator: unary_op, operand: Box::new(x.1)}
            },
            None => x.1

    });
    return node;
}

// TODO: Maybe abstract these out?

//TODO munge or and and together, they should have the same precedence
// TODO research the Chesterton's fence situation first
fn and_expr_ast(input: &[u8]) -> ExprRes {
    return binary_op_keyword(input, "and", BinaryOperator::And, or_expr_ast);
}

fn or_expr_ast(input: &[u8]) -> ExprRes {
    return binary_op_keyword(input, "or", BinaryOperator::Or, xor_expr_ast);
}

fn xor_expr_ast(input: &[u8]) -> ExprRes {
    return binary_op_keyword(input, "xor", BinaryOperator::Xor, bit_or);
}


//TODO munge bit or and bit and together, they should have the same precedence
fn bit_or(input: &[u8]) -> ExprRes {
    return binary_op_symbol(input, "|", BinaryOperator::BitOr, bit_and);
}

fn bit_and(input: &[u8]) -> ExprRes {
    return binary_op_symbol(input, "&", BinaryOperator::BitAnd, bit_xor);
}

fn bit_xor(input: &[u8]) -> ExprRes {
    return binary_op_symbol(input, "^", BinaryOperator::BitXor, bit_shift);
}

fn bit_shift(input: &[u8]) -> ExprRes {
    let symbols = vec![">>", "<<"];
    let operators = c!{k.as_bytes() => BinaryOperator::from(*k), for k in symbols.iter()};
    return binary_op_list(input, &symbols, &operators, additive_expr_ast);
}

fn additive_expr_ast(input: &[u8]) -> ExprRes {
    let symbols = vec!["+", "-"];
    let operators = c!{k.as_bytes() => BinaryOperator::from(*k), for k in symbols.iter()};
    return binary_op_list(input, &symbols, &operators, mult_expr_ast);
}

fn mult_expr_ast(input: &[u8]) -> ExprRes {
    let symbols = vec!["*", "/", "%"];
    let operators = c!{k.as_bytes() => BinaryOperator::from(*k), for k in symbols.iter()};
    return binary_op_list(input, &symbols, &operators, unary_expr);
}

fn power_expr_ast(input: &[u8]) -> ExprRes {
    return binary_op_symbol(input, "**", BinaryOperator::Exponent, atomic_expr_ast);
}

// TODO: Use everywhere
named!(args_list<&[u8], Vec<Expr>>,
    separated_list_complete!(
        inline_wrapped!(tag!(",")), 
        expression_ast
    )
);

/// Parse dot separated identifiers.
/// e.g. ident1.ident2   .   ident3
fn dotted_identifier(input: &[u8]) -> IResult<&[u8], DottedIdentifier> {
    let parse_result = separated_nonempty_list_complete!(input,
        inline_wrapped!(tag!(".")),
        identifier
    );

    let map = |x: Vec<&[u8]>| {
        let attributes = x.iter().map(|y| match from_utf8(y) {
            Ok(i) => i.to_string(),
            _ => panic!()
        }).collect();
        return DottedIdentifier{attributes: attributes};
    };

    return fmap_iresult(parse_result, map);
}

//TODO get rid of all the _ast bits
fn atomic_expr_ast(input: &[u8]) -> ExprRes {
    let node = alt!(input,
        bool_expr_ast |
        complete!(float_ast) |
        complete!(int_ast) |
        complete!(string_ast) |
        expr_with_trailer
    );
    return node;
}

fn wrapped_expr(input: &[u8]) -> ExprRes {
    let node = delimited!(input,
        inline_wrapped!(tag!("(")),
        expression_ast,
        inline_wrapped!(tag !(")"))
    );

    return node;
}

/// An expression that can be followed by an arbitrary number of function calls or attribute accesses.
fn expr_with_trailer(input: &[u8]) -> ExprRes {
    let ident = |x| fmap_iresult(
        identifier_ast(x),
        |y: Identifier| Expr::IdentifierExpr {ident: y}
    );

    let parse_result = tuple!(input,
        alt!(ident | wrapped_expr),
        many0!(trailer)
    );


    let map = |x: (Expr, Vec<PostIdent>)| {
        let mut tree_base = x.0;
        for postval in x.1 {
            match postval {
                PostIdent::Call{args} => {
                    tree_base = Expr::FunctionCall {func_expr: Box::new(tree_base), args:args};
                },

                PostIdent::Access{attributes} => {
                    tree_base = Expr::AttributeAccess {container: Box::new(tree_base), attributes: attributes};
                }
            }
        };
        return tree_base;
    };

    let node = fmap_iresult(parse_result, map);
    return node;
}

fn trailer(input: &[u8]) -> IResult<&[u8], PostIdent> {
    let call_to_enum = |x: Vec<Expr>| PostIdent::Call{args: x};
    let access_to_enum = |x: Vec<Identifier>| PostIdent::Access{attributes: x};
    let result = alt!(input,
        map!(post_call, call_to_enum) |
        map!(post_access, access_to_enum)
    );
    return result;
}

named!(post_call<&[u8], Vec<Expr>>,
    delimited!(
        inline_wrapped!(tag!("(")),
        inline_wrapped!(args_list),
        inline_wrapped!(tag!(")"))
    )
);

named!(post_access<&[u8], Vec<Identifier>>,
    many1!(
        preceded!(
            inline_wrapped!(tag!(".")),
            identifier_ast
        )
    )
);

/// Parser to recognize a valid Grace identifier.
named!(identifier<&[u8], &[u8]>,
    recognize!(
        pair!(
            not!(peek!(reserved_words)),
            pair!(
                alt!(alpha | tag!("_")),
                many0!(valid_identifier_char)
            )
        )
    )
);

/// Parser to return an Identifier AST.
fn identifier_ast(input: &[u8]) -> IResult<&[u8], Identifier> {
    let parse_result = identifier(input);
    let node = fmap_iresult(parse_result,  Identifier::from);
    return node;
}

// TODO: Use Boolean::from
fn bool_expr_ast(input: &[u8]) -> ExprRes {
    let parse_result= alt!(input,
        terminated!(tag!("true"), peek!(not!(valid_identifier_char))) |
        terminated!(tag!("false"), peek!(not!(valid_identifier_char)))
    );
    return fmap_iresult(parse_result, |x| match from_utf8(x) {
        Ok("true") => Expr::Bool(Boolean::True),
        Ok("false") => Expr::Bool(Boolean::False),
        _ => panic!(),
    });
}

// TODO: Hex encoded, byte encoded
// TODO:
fn int_ast(input: &[u8]) -> ExprRes {
    let parse_result: IResult<&[u8], &[u8]> = recognize!(input,
        tuple!(
            opt!(sign),
            terminated!(
                dec_seq,
                num_follow
            )
        )
    );
    return fmap_iresult(parse_result, |x| Expr::Int(IntegerLiteral::from(x)));
}

fn float_ast<'a>(input: &'a[u8]) -> ExprRes {

    let with_dec = |x: &'a[u8]| tuple!(x,
        tag!("."),
        many0!(dec_digit),
        opt!(complete!(exponent))
    );

    let parse_result = recognize!(input, tuple!(
        opt!(sign),
        many0!(dec_digit),
        alt!(
            value!((), with_dec) |
            value!((), complete!(exponent))
        ),
        num_follow
    ));

    return fmap_iresult(parse_result, |x| Expr::Float(FloatLiteral::from(x)));
}

named!(string_char<&[u8], &[u8]>,
    recognize!(
        alt!(
            tag!("\\\"") |
            tag!("\\\\") |
            tag!("\\\n") |
            tag!("\\\r") |
            recognize!(none_of!("\n\""))
        )
    )
);

named!(string_literal<&[u8],&[u8]>,
    recognize!(
        tuple!(
            tag!("\""),
            many0!(string_char),
            tag!("\"")
        )
    )
);

fn string_ast(input: &[u8]) -> ExprRes {
    let parse_result = string_literal(input);

    return fmap_iresult(parse_result, |x: &[u8]| Expr::String(from_utf8(x).unwrap().to_string()));
}

fn read_from_file(f_name: &str) -> String {
    let filename= format!("./test_data/{}.gr", f_name);
    let mut f = File::open(filename).expect("File not found");
    let mut contents = String::new();
    match f.read_to_string(&mut contents) {
        Ok(_) => return contents,
        _ => panic!()
    };
}


fn check_match<T>(input: &str, parser: fn(&[u8]) -> IResult<&[u8], T>, expected: T)
    where T: Debug + PartialEq + Eq {
    let res = parser(input.as_bytes());
    match res {
        Done(i, o) => {
            let l_r = format!("\n    Expected: {:?}\n    Actual: {:?}", expected, o);
            assert_eq!(i, "".as_bytes(), "Leftover input should have been empty, was: {:?}\nResults were: {}", from_utf8(i), l_r);
            assert_eq!(o, expected);
        },
        IResult::Error(e) => {
            println!("Error: {}. Input was: {}", e, input);
            panic!()
        },
        _ => panic!()
    }
}

fn check_failed<T>(input: &str, parser: fn(&[u8]) -> IResult<&[u8], T>, expected: ErrorKind) {
    let res = parser(input.as_bytes());
    match res {
        IResult::Error(e) => {
            assert_eq!(e, expected);
        },
        _ => panic!()
    }
}

#[test]
fn test_literals() {
    let int = format!("{}", rand::random::<i64>().abs());
    check_match(int.as_str(), expression_ast, Expr::Int(IntegerLiteral{string_rep: int.clone()}));
    let float = format!("{}", rand::random::<f64>().abs());
    check_match(float.as_str(), float_ast, Expr::Float(FloatLiteral{string_rep: float.clone()}));

    check_match("\"asdf\\\"\\\rasdf\"", expression_ast, Expr::String("\"asdf\\\"\\\rasdf\"".to_string()));
}

#[test]
fn test_reserved_words() {
    for keyword in reserved_list() {
        let result = identifier(keyword.as_bytes());
        assert_eq!(result, IResult::Error(ErrorKind::Not));
    }
}

#[test]
fn test_simple_parenthetical_expressions() {
    let expected =Expr::BinaryExpr {
        operator:BinaryOperator::And,
        left: Box::new(Expr::Bool(Boolean::True)),
        right: Box::new(Expr::Bool(Boolean::False))};
    check_match("(true and false)", expression_ast, expected);
}

#[test]
fn test_parenthetical_expressions() {
    let expected = Expr::BinaryExpr {
        operator: BinaryOperator::Or,
        left:Box::new(Expr::BinaryExpr {
            operator:BinaryOperator::And,
            left: Box::new(Expr::Bool(Boolean::True)),
            right: Box::new(Expr::Bool(Boolean::False))}),
        right:Box::new(Expr::Bool(Boolean::True))};
    check_match("(true and false) or true", expression_ast, expected);
}

#[test]
fn test_function_call() {
    let a = output(and_expr_ast("true and false".as_bytes()));
    let b = output(expression_ast("func()".as_bytes()));
    let expected = Expr::FunctionCall{func_expr: Box::new(Expr::IdentifierExpr{ident: Identifier{name: "ident".to_string()}}), args: vec!(a, b)};
    check_match("ident(true and false, func())", expression_ast, expected);
}

#[test]
fn test_binary_expr() {
    check_match("true and false or true", expression_ast, Expr::BinaryExpr{
        operator: BinaryOperator::And,
        left: Box::new(Expr::from(true)),
        right:Box::new(Expr::BinaryExpr{
            operator: BinaryOperator::Or,
            left: Box::new(Expr::from(false)),
            right: Box::new(Expr::from(true))
        })
    });

    let all_ops = vec!["and", "or", "xor", "&", "|", "^", "+", "-", "*", "/", "%", ">>", "<<", "**"];
    for op in all_ops {
        let input = format!("x {} y", op);
        check_match(input.as_str(), expression_ast, Expr::BinaryExpr {
            operator: BinaryOperator::from(op),
            left: Box::new(Expr::from("x")),
            right: Box::new(Expr::from("y")),
        });
    }
}

#[test]
fn test_unary_expr() {
    let ops = vec!["not", "+", "-", "~"];
    for op in ops {
        let input = format!("{} y", op);
        check_match(input.as_str(), expression_ast, Expr::UnaryExpr {
            operator: UnaryOperator::from(op),
            operand: Box::new(Expr::from("y")),
        });
    }
    check_match("~+y", expression_ast, Expr::UnaryExpr {
        operator: UnaryOperator::BitNot,
        operand: Box::new(output(expression_ast("+y".as_bytes()))),
    });
    check_match("not true", expression_ast, Expr::UnaryExpr {operator: UnaryOperator::Not, operand: Box::new(Expr::from(true))});
}

#[test]
fn test_identifier_expr() {
    let identifier_expr = expression_ast("words".as_bytes());
    let expected = Expr::IdentifierExpr{ident: Identifier{name: "words".to_string()}};
    assert_eq!(identifier_expr, Done("".as_bytes(), expected));
}

#[test]
fn test_comparison_expr() {
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
fn test_repeated_func_calls() {
    let expected = Expr::FunctionCall{
        func_expr: Box::new(Expr::FunctionCall{func_expr: Box::new(Expr::from("func")), args: vec!(Expr::from("a"))}),
        args: vec!(Expr::from("b"), Expr::from("c"))
    };
    check_match("func(a)(b, c)", expression_ast, expected);

    check_match("(a and b)(true)", expression_ast, Expr::FunctionCall {
        func_expr: Box::new(output(and_expr_ast("a and b".as_bytes()))),
        args: vec!(Expr::from(true))
    });
}

#[test]
fn test_dotted_identifier() {
    let expected = DottedIdentifier{attributes: vec!("asdf".to_string(), "dfgr_1".to_string(), "_asdf".to_string())};
    check_match("asdf.dfgr_1   .   _asdf", dotted_identifier, expected);
}

#[test]
fn test_post_ident() {
    let expected_args = vec!("a", "b", "c").iter().map(|x| Expr::from(*x)).collect();
    check_match("(a, b, c)", trailer, PostIdent::Call{args: expected_args});

    check_match(".asdf_   .   asdf", trailer, PostIdent::Access{attributes: vec!(Identifier::from("asdf_"), Identifier::from("asdf"))});
}

#[test]
fn test_assignment() {
    check_match("foo = true", assignment_ast, Stmt::AssignmentStmt {
        identifier: Identifier::from("foo"),
        operator: Assignment::Normal,
        expression: Expr::from(true)
    });

    let all_ops = vec!["&=", "|=", "^=", "+=", "-=", "*=", "/=", "%=", ">>=", "<<=", "**=", "="];
    for op in all_ops {
        let input = format!("x {} y", op);
        check_match(input.as_str(), assignment_ast, Stmt::AssignmentStmt {
            identifier: Identifier::from("x"),
            operator: Assignment::from(op),
            expression: Expr::from("y"),
        });
    }

}

#[test]
fn test_if_stmt() {
    let good_input = "if (a and b):\n x = true";

    let good_output = Stmt::IfStmt{
        condition: output(expression_ast("a and b".as_bytes())),
        main_block: Block{statements: vec!(output(assignment_ast("x = true".as_bytes())))},
        elifs: vec!(),
        else_block: None
    };

    check_match(good_input, |x| statement_ast(x, 0), good_output);

    check_failed("ifa and b:\n x = true", |x| statement_ast(x, 0), nom::ErrorKind::Alt);
}

#[test]
fn test_while_stmt() {
    check_match("while true:\n x=true", |x| statement_ast(x, 0), Stmt::WhileStmt {
        condition: Expr::from(true),
        block: Block{statements: vec!(output(assignment_ast("x=true".as_bytes())))}
    });
}

#[test]
fn test_import() {
    let expected = Stmt::ImportStmt {module: DottedIdentifier{attributes: vec!("foo".to_string(), "bar".to_string(), "baz".to_string())}};
    check_match("import foo.bar.baz", |x| statement_ast(x, 0), expected);
}

#[test]
fn test_return() {
    let expected = Stmt::ReturnStmt {value: Expr::from(true)};
    check_match("return true", |x| statement_ast(x, 0), expected);
}

#[test]
fn test_for_in() {
    check_match("for x in y:\n a=true", |x| statement_ast(x, 0), Stmt::ForInStmt {
        iter_var: Identifier::from("x"),
        iterator: Expr::from("y"),
        block: output(block_ast("a=true".as_bytes(), 0))
    });
}


