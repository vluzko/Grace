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
use utils::*;

type ExprRes<'a> = IResult<&'a [u8], Expr>;
type StmtRes<'a> = IResult<&'a[u8], Stmt>;

pub fn parse_grace(input: &str) -> IResult<&[u8], Box<ASTNode>> {
    parse_grace_from_slice(input.as_bytes())
}

// This is the important function
pub fn parse_grace_from_slice(input: &[u8]) -> IResult<&[u8], Box<ASTNode>> {

    let output = terminated!(input, call!(block, 0), custom_eof);
    return match output {
        Done(i, o) => Done(i, Box::new(o) as Box<ASTNode>),
        IResult::Incomplete(n) => IResult::Incomplete(n),
        IResult::Error(e) => IResult::Error(e)
    };
}

/// Create a rule of the form: KEYWORD PARSER COLON BLOCK
/// if, elif, except, fn are all rules of this form.
macro_rules! line_then_block (
    ($i:expr, $keyword: expr, $submac: ident!($($args:tt)* ), $indent: expr) => (
        tuple!($i,
            delimited!(
                terminated!(
                    indented!(tag!($keyword), $indent),
                    not!(valid_identifier_char)
                ),
                inline_wrapped!($submac!($($args)*)),
                ending_colon
            ),
            call!(block, $indent + 1)
        )
    );

    ($i:expr, $keyword: expr, $func: expr, $indent: expr) => (
        line_then_block!($i, $keyword, call!($func), $indent)
    );
);

/// Create a rule of the form: KEYWORD COLON BLOCK
/// else and try are both rules of this form.
macro_rules! keyword_then_block (
    ($i:expr, $keyword: expr, $indent: expr) => (
        match line_then_block!($i, $keyword, inline_whitespace, $indent) {
            IResult::Error(a)      => IResult::Error(a),
            IResult::Incomplete(i) => IResult::Incomplete(i),
            IResult::Done(remaining, (_,o)) => IResult::Done(remaining, o)
        }
    );
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

pub fn reserved_list() -> Vec<&'static str>{
    let list: Vec<&'static str> = vec!("if", "else", "elif", "for", "while", "and", "or", "not", "xor", "fn", "import", "true", "false", "in", "match", "pass", "continue", "break", "yield");
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

// TODO: Merge block_rule with block
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
                        indented!(call!(statement, expected_indent), expected_indent)
                    )
                )
            ),
            between_statement
        );

        return statements;
    }

}

fn block(input: &[u8], indent: usize) -> IResult<&[u8], Block> {
    let parse_result = block_rule(input, indent);
    return fmap_iresult(parse_result, |x| Block{statements: x});
}

/// Match any statement.
fn statement(input: &[u8], indent: usize) -> StmtRes {
    let node = alt_complete!(input,
        assignment |
        call!(while_stmt, indent) |
        call!(for_in, indent) |
        call!(if_stmt, indent) |
        call!(function_declaration, indent) |
        call!(try_except, indent) |
        import |
        return_stmt |
        break_stmt |
        pass_stmt |
        continue_stmt |
        yield_stmt
    );

    return fmap_iresult(node, |x| x);
}

/// Match an import statement.
fn import(input: &[u8]) -> StmtRes {
    let parse_result = tuple!(input, inline_keyword!("import"), dotted_identifier);
    return fmap_iresult(parse_result,|x| Stmt::ImportStmt {module: x.1});
}

/// Match a return statement.
fn return_stmt(input: &[u8]) -> StmtRes {
    let parse_result = tuple!(input, inline_keyword!("return"), expression);
    return fmap_iresult(parse_result,|x| Stmt::ReturnStmt {value: x.1});
}

/// Parse a while loop.
fn while_stmt(input: &[u8], indent: usize) -> StmtRes {
    let parse_result = line_then_block!(input, "while", expression, indent);
    return fmap_iresult(parse_result, |x| Stmt::WhileStmt {condition: x.0, block: x.1});
}

/// Parse a for in loop.
fn for_in(input: &[u8], indent: usize) -> StmtRes {
    let parse_result = line_then_block!(input, "for", tuple!(
        inline_wrapped!(identifier),
        preceded!(
            inline_keyword!("in"),
            inline_wrapped!(expression)
        )
    ), indent);

    return fmap_iresult(parse_result, |x| Stmt::ForInStmt {iter_var: (x.0).0, iterator: (x.0).1, block: x.1});
}

fn if_stmt(input: &[u8], indent: usize) -> StmtRes {
    let parse_result = tuple!(input,
        line_then_block!("if", expression, indent),
        many0!(line_then_block!("elif", expression, indent)),
        opt!(complete!(keyword_then_block!("else", indent)))
    );

    return fmap_iresult(parse_result, |x|Stmt::IfStmt{condition: (x.0).0, main_block: (x.0).1, elifs: x.1, else_block: x.2});
}

/// Match all normal arguments.
named!(args_dec_list<&[u8], Vec<Identifier>>,
    inline_wrapped!(separated_list_complete!(inline_wrapped!(tag!(",")), identifier))
);

/// Match the variable length argument.
named!(vararg<&[u8], Option<Identifier>>,
    opt!(complete!(preceded!(
        tuple!(
            inline_wrapped!(tag!(",")),
            inline_wrapped!(tag!("*"))
        ),
        inline_wrapped!(identifier)
    )))
);

/// Match all default arguments
named!(keyword_args<&[u8], Option<Vec<(Identifier, Expr)>>>,
    opt!(complete!(preceded!(
        inline_wrapped!(tag!(",")),
        inline_wrapped!(separated_list_complete!(inline_wrapped!(tag!(",")),
            tuple!(
                inline_wrapped!(identifier),
                preceded!(
                    inline_wrapped!(tag!("=")),
                    inline_wrapped!(expression)
                )
            )
        ))
    )))
);

/// Match the variable length keyword argument.
named!(kwvararg<&[u8], Option<Identifier>>,
    opt!(complete!(preceded!(
        tuple!(
            inline_wrapped!(tag!(",")),
            inline_wrapped!(tag!("**"))
        ),
        inline_wrapped!(identifier)
    )))
);

/// Match a function declaration.
fn function_declaration<'a>(input: &'a [u8], indent: usize) -> StmtRes {
    let arg_parser = |i: &'a [u8]| tuple!(i,
        inline_wrapped!(identifier),
        delimited!(
            tag!("("),
            tuple!(
                args_dec_list,
                vararg,
                keyword_args,
                kwvararg
            ),
            tag!(")")
        )
    );

    let parse_result = line_then_block!(input, "fn", arg_parser, indent);

    return fmap_iresult(parse_result, |x| Stmt::FunctionDecStmt{
        name: x.0 .0,
        args: x.0 .1 .0,
        vararg: x.0 .1 .1,
        keyword_args: x.0 .1 .2,
        varkwarg: x.0 .1 . 3,
        body: x.1
    });
}

/// Match a try except statement.
fn try_except(input: &[u8], indent: usize) -> StmtRes {
    let parse_result = tuple!(input,
        keyword_then_block!("try", indent),
        many1!(keyword_then_block!("except", indent)),
        opt!(complete!(
            keyword_then_block!("else", indent)
        )),
        opt!(complete!(keyword_then_block!("finally", indent)))
    );

    return fmap_iresult(parse_result, |x| Stmt::TryExceptStmt {
        main: x.0,
        exception: x.1,
        else_block: x.2,
        finally: x.3
    });
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

fn assignment(input: &[u8]) -> StmtRes {
    let parse_result = terminated!(input,
        tuple!(
            identifier,
            inline_wrapped!(assignments),
            inline_wrapped!(expression)
        ),
        alt_complete!(recognize!(newline)| custom_eof)
    );

    return fmap_iresult(parse_result, |x| Stmt::AssignmentStmt{
        identifier: x.0, operator:Assignment::from(from_utf8(x.1).unwrap()), expression: x.2});
}

fn break_stmt(input: &[u8]) -> StmtRes {
    let parse_result = terminated!(input,
        tag!("break"),
        terminated!(
            inline_whitespace,
            eof_or_line
        )
    );

    return fmap_iresult(parse_result, |_x| Stmt::BreakStmt);
}

fn pass_stmt(input: &[u8]) -> StmtRes {
    let parse_result = terminated!(input,
        tag!("pass"),
        terminated!(
            inline_whitespace,
            eof_or_line
        )
    );

    return fmap_iresult(parse_result, |_x| Stmt::PassStmt);
}

fn continue_stmt(input: &[u8]) -> StmtRes {
    let parse_result = terminated!(input,
        tag!("continue"),
        terminated!(
            inline_whitespace,
            eof_or_line
        )
    );

    return fmap_iresult(parse_result, |_x| Stmt::ContinueStmt);
}

fn yield_stmt(input: &[u8]) -> StmtRes {
    let parse_result = preceded!(input,
        tag!("yield"),
        inline_wrapped!(expression)
    );

    return fmap_iresult(parse_result, |x| Stmt::YieldStmt(x))
}

fn expression(input: &[u8]) -> ExprRes {
    return alt_complete!(input,
        comparison
    );
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

fn comparison(input: &[u8]) -> ExprRes {
    let parse_result = tuple!(input,
        alt!(match_expr | boolean_op_expr),
        opt!(complete!(tuple!(
            inline_wrapped!(comparisons),
            boolean_op_expr
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

fn match_expr(input: &[u8]) -> ExprRes {

    let parse_result = tuple!(input,
        delimited!(
            tag!("match"),
            inline_wrapped!(expression),
            tuple!(
                inline_wrapped!(tag!(":")),
                between_statement
            )
        ),
        separated_nonempty_list_complete!(
            between_statement,
            separated_pair!(
                alt!(float | int | string),
                inline_wrapped!(tag!("=>")),
                expression
            )
        )
    );

    return fmap_iresult(parse_result, |x| Expr::MatchExpr {value: Box::new(x.0), cases: x.1});
}

/// Match a single binary expression.
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

/// Match a binary expression whose operator is a keyword
/// Currently only used for and, or, and xor.
fn binary_keyword_list<'a>(input: &'a [u8], symbols: &Vec<&str>, operators: &HashMap<&[u8], BinaryOperator>, next_expr: fn(&[u8]) -> ExprRes) -> IResult<&'a [u8], Expr> {
    let parse_result = tuple!(input,
        next_expr,
        opt!(tuple!(
            inline_keyword!(call!(match_any, symbols)),
            call!(binary_op_list, symbols, operators, next_expr)
        ))
    );

    let node = fmap_iresult(parse_result, |x| match_binary_exprs(operators, x));
    return node;
}

/// Match a list of binary operations
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

/// Match boolean operators.
fn boolean_op_expr(input: &[u8]) -> ExprRes {
    let symbols = vec!["and", "or", "xor"];
    let operators = c!{k.as_bytes() => BinaryOperator::from(*k), for k in symbols.iter()};
    return binary_keyword_list(input, &symbols, &operators, bit_boolean_op_expr);
}

/// Match bitwise boolean operations.
fn bit_boolean_op_expr(input: &[u8]) -> ExprRes {
    let symbols = vec!["&", "|", "^"];
    let operators = c!{k.as_bytes() => BinaryOperator::from(*k), for k in symbols.iter()};
    return binary_op_list(input, &symbols, &operators,bit_shift);
}

/// Match the bit shift operators.
fn bit_shift(input: &[u8]) -> ExprRes {
    let symbols = vec![">>", "<<"];
    let operators = c!{k.as_bytes() => BinaryOperator::from(*k), for k in symbols.iter()};
    return binary_op_list(input, &symbols, &operators, additive_expr);
}

/// Match addition and subtraction.
fn additive_expr(input: &[u8]) -> ExprRes {
    let symbols = vec!["+", "-"];
    let operators = c!{k.as_bytes() => BinaryOperator::from(*k), for k in symbols.iter()};
    return binary_op_list(input, &symbols, &operators, mult_expr);
}

/// Match multiplication, division, and modulo.
fn mult_expr(input: &[u8]) -> ExprRes {
    let symbols = vec!["*", "/", "%"];
    let operators = c!{k.as_bytes() => BinaryOperator::from(*k), for k in symbols.iter()};
    return binary_op_list(input, &symbols, &operators, unary_expr);
}

/// Match any unary expression.
/// Implemented as a single parser because all unary expressions have the same precedence.
fn unary_expr(input: & [u8]) -> IResult<& [u8], Expr> {
    let parse_result: IResult<&[u8], (Option<&[u8]>, Expr)> = alt!(input,
        tuple!(
            map!(inline_wrapped!(alt!(tag!("+") | tag!("-") | tag!("~") | inline_keyword!("not"))), Some),
            unary_expr)
         |
        tuple!(
            value!(None, tag!("")),
            power_expr
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

fn power_expr(input: &[u8]) -> ExprRes {
    return binary_op_symbol(input, "**", BinaryOperator::Exponent, atomic_expr);
}

// TODO: Use everywhere
named!(args_list<&[u8], Vec<Expr>>,
    separated_list_complete!(
        inline_wrapped!(tag!(",")),
        expression
    )
);

/// Parse dot separated identifiers.
/// e.g. ident1.ident2   .   ident3
fn dotted_identifier(input: &[u8]) -> IResult<&[u8], DottedIdentifier> {
    let parse_result = separated_nonempty_list_complete!(input,
        inline_wrapped!(tag!(".")),
        identifier
    );

//    let map = |x: Vec<&[u8]>| {
//        let attributes = x.iter().map(|y| match from_utf8(y) {
//            Ok(i) => i.to_string(),
//            _ => panic!()
//        }).collect();
//        return DottedIdentifier{attributes: attributes};
//    };

    return fmap_iresult(parse_result, |x: Vec<Identifier>| DottedIdentifier{attributes: x});
}

//TODO get rid of all the  bits
fn atomic_expr(input: &[u8]) -> ExprRes {
    let node = alt_complete!(input,
        bool_expr |
        float |
        int |
        string |
        delimited!(
            inline_wrapped!(tag!("{")),
            map_or_set_comprehension,
            inline_wrapped!(tag!("}"))
        ) |
        delimited!(
            inline_wrapped!(tag!("[")),
            vector_comprehension,
            inline_wrapped!(tag!("]"))
        ) |
        expr_with_trailer
    );
    return node;
}

/// Match the for part of a comprehension.
fn comprehension_for(input: &[u8]) -> IResult<&[u8], Vec<Identifier>> {
    let parse_result = separated_nonempty_list_complete!(input,
        inline_wrapped!(tag!(",")),
        identifier
    );

    return parse_result;
}

/// Match the if part of a comprehension.
fn comprehension_if(input: &[u8]) -> ExprRes {
    return preceded!(input,
        inline_keyword!("if"),
        expression
    );
}

/// Match a vector comprehension.
fn vector_comprehension(input: &[u8]) -> ExprRes {
    let parse_result = tuple!(input,
        boolean_op_expr,
        delimited!(
            inline_keyword!("for"),
            comprehension_for,
            inline_keyword!("in")
        ),
        boolean_op_expr,
        opt!(complete!(comprehension_if))
    );

    return fmap_iresult(parse_result, |x: (Expr, Vec<Identifier>, Expr, Option<Expr>)| Expr::VecComprehension {
        values: Box::new(x.0),
        iterator_unpacking: x.1,
        iterator: Box::new(x.2)
    });
}

/// Match a map or a set.
fn map_or_set_comprehension(input: &[u8]) -> ExprRes {
    let parse_result = tuple!(input,
            boolean_op_expr,
            opt!(complete!(preceded!(
                inline_wrapped!(tag!(":")),
                boolean_op_expr
            ))),
            delimited!(
                inline_keyword!("for"),
                comprehension_for,
                inline_keyword!("in")
            ),
            boolean_op_expr,
            opt!(complete!(comprehension_if))
    );

    return fmap_iresult(parse_result, |x: (Expr, Option<Expr>, Vec<Identifier>, Expr, Option<Expr>)| match x.1 {
        Some(y) => {
            Expr::MapComprehension {keys: Box::new(x.0), values: Box::new(y), iterator_unpacking: x.2, iterator: Box::new(x.3)}
        },
        None => {
            Expr::SetComprehension {values: Box::new(x.0), iterator_unpacking: x.2, iterator: Box::new(x.3)}
        }
    })
}

/// An expression wrapped in parentheses.
fn wrapped_expr(input: &[u8]) -> ExprRes {
    let node = delimited!(input,
        inline_wrapped!(tag!("(")),
        expression,
        inline_wrapped!(tag !(")"))
    );

    return node;
}

/// An expression that can be followed by an arbitrary number of function calls or attribute accesses.
fn expr_with_trailer(input: &[u8]) -> ExprRes {
    let ident = |x| fmap_iresult(
        identifier(x),
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
            identifier
        )
    )
);

/// Parser to return an Identifier AST.
fn identifier(input: &[u8]) -> IResult<&[u8], Identifier> {
    let parse_result = recognize!(input,
        pair!(
            not!(peek!(reserved_words)),
            pair!(
                alt!(alpha | tag!("_")),
                many0!(valid_identifier_char)
            )
        )
    );
    let node = fmap_iresult(parse_result,  Identifier::from);
    return node;
}

// TODO: Use Boolean::from
fn bool_expr(input: &[u8]) -> ExprRes {
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
fn int(input: &[u8]) -> ExprRes {
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

fn float<'a>(input: &'a[u8]) -> ExprRes {

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

fn string(input: &[u8]) -> ExprRes {
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

#[cfg(test)]
mod tests {

    use super::*;

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
    fn test_block() {
        check_match(" x=0\n y=true\n\n  \n", |x| block(x, 1), Block{
            statements: vec![
                output(assignment("x=0\n".as_bytes())),
                output(assignment("y=true".as_bytes()))
            ]
        });
    }

    #[test]
    fn test_func_dec() {
        check_match("fn x(a, b, *args, c=5, d=7, **kwargs):\n x = 5", |x| function_declaration(x, 0), Stmt::FunctionDecStmt {
            name: Identifier::from("x"),
            args: c![Identifier::from(x), for x in vec!("a", "b")],
            vararg: Some(Identifier::from("args")),
            keyword_args: Some(vec!(
                (Identifier::from("c"), output(expression("5".as_bytes()))),
                (Identifier::from("d"), output(expression("7".as_bytes())))
            )),
            varkwarg: Some(Identifier::from("kwargs")),
            body: output(block("x=5\n".as_bytes(), 0))
        });
    }

    #[test]
    fn test_literals() {
        let int = format!("{}", rand::random::<i64>().abs());
        check_match(int.as_str(), expression, Expr::Int(IntegerLiteral{string_rep: int.clone()}));
        let rand_float = format!("{}", rand::random::<f64>().abs());
        check_match(rand_float.as_str(), float, Expr::Float(FloatLiteral{string_rep: rand_float.clone()}));

        check_match("\"asdf\\\"\\\rasdf\"", expression, Expr::String("\"asdf\\\"\\\rasdf\"".to_string()));
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
        let expected = Expr::BinaryExpr {
            operator:BinaryOperator::And,
            left: Box::new(Expr::Bool(Boolean::True)),
            right: Box::new(Expr::Bool(Boolean::False))};
        check_match("(true and false)", expression, expected);
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
        check_match("(true and false) or true", expression, expected);
    }

    #[test]
    fn test_function_call() {
        let a = output(boolean_op_expr("true and false".as_bytes()));
        let b = output(expression("func()".as_bytes()));
        let expected = Expr::FunctionCall{func_expr: Box::new(Expr::IdentifierExpr{ident: Identifier{name: "ident".to_string()}}), args: vec!(a, b)};
        check_match("ident(true and false, func())", expression, expected);
    }

    #[test]
    fn test_binary_expr() {
        check_match("true and false or true", expression, Expr::BinaryExpr{
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
            check_match(input.as_str(), expression, Expr::BinaryExpr {
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
            check_match(input.as_str(), expression, Expr::UnaryExpr {
                operator: UnaryOperator::from(op),
                operand: Box::new(Expr::from("y")),
            });
        }
        check_match("~+y", expression, Expr::UnaryExpr {
            operator: UnaryOperator::BitNot,
            operand: Box::new(output(expression("+y".as_bytes()))),
        });
        check_match("not true", expression, Expr::UnaryExpr {operator: UnaryOperator::Not, operand: Box::new(Expr::from(true))});
    }

    #[test]
    fn test_identifier_expr() {
        let identifier_expr = expression("words".as_bytes());
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
            let expr = expression(as_str.as_bytes());
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
        check_match("func(a)(b, c)", expression, expected);

        check_match("(a and b)(true)", expression, Expr::FunctionCall {
            func_expr: Box::new(output(boolean_op_expr("a and b".as_bytes()))),
            args: vec!(Expr::from(true))
        });
    }

    #[test]
    fn test_dotted_identifier() {
        let expected = DottedIdentifier{attributes: vec!(Identifier::from("asdf"), Identifier::from("dfgr_1"), Identifier::from("_asdf"))};
        check_match("asdf.dfgr_1   .   _asdf", dotted_identifier, expected);
    }

    #[test]
    fn test_post_ident() {
        let expected_args = vec!("a", "b", "c").iter().map(|x| Expr::from(*x)).collect();
        check_match("(a, b, c)", trailer, PostIdent::Call{args: expected_args});

        check_match(".asdf_   .   asdf", trailer, PostIdent::Access{attributes: vec!(Identifier::from("asdf_"), Identifier::from("asdf"))});
    }

    #[test]
    fn test_try_except() {
        let blk = output(block("x=0".as_bytes(), 0));
        check_match("try    :     \n\n\n x = 0\n\n     \n\nexcept:\n x =      0     \nelse:\n x=0\nfinally:\n x=0     \n\n\n   \n\n", |x| try_except(x, 0), Stmt::TryExceptStmt {
            main: blk.clone(),
            exception: vec!(blk.clone()),
            else_block: Some(blk.clone()),
            finally: Some(blk.clone())
        });
    }

    #[test]
    fn test_assignment() {
        check_match("foo = true", assignment, Stmt::AssignmentStmt {
            identifier: Identifier::from("foo"),
            operator: Assignment::Normal,
            expression: Expr::from(true)
        });

        check_match("x = 0\n", assignment, Stmt::AssignmentStmt {
            identifier: Identifier::from("x"),
            operator: Assignment::Normal,
            expression: Expr::Int(IntegerLiteral::from(0))
        });

        let all_ops = vec!["&=", "|=", "^=", "+=", "-=", "*=", "/=", "%=", ">>=", "<<=", "**=", "="];
        for op in all_ops {
            let input = format!("x {} y", op);
            check_match(input.as_str(), assignment, Stmt::AssignmentStmt {
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
            condition: output(expression("a and b".as_bytes())),
            main_block: Block{statements: vec!(output(assignment("x = true".as_bytes())))},
            elifs: vec!(),
            else_block: None
        };

        check_match(good_input, |x| statement(x, 0), good_output);

        check_failed("ifa and b:\n x = true", |x| statement(x, 0), nom::ErrorKind::Alt);

        check_match("if    true   :     \n\n\n x = true\nelif    false   :   \n\n\n y = true\nelse     :  \n z = true", |x| if_stmt(x, 0), Stmt::IfStmt {
            condition: Expr::from(true),
            main_block: output(block("x = true".as_bytes(), 0)),
            elifs: vec!((Expr::from(false), output(block("y = true".as_bytes(), 0)))),
            else_block: Some(output(block("z = true".as_bytes(), 0)))
        });
    }

    #[test]
    fn test_while_stmt() {
        check_match("while true:\n x=true", |x| statement(x, 0), Stmt::WhileStmt {
            condition: Expr::from(true),
            block: Block{statements: vec!(output(assignment("x=true".as_bytes())))}
        });
    }

    #[test]
    fn test_import() {
        check_match("import foo.bar.baz", |x| statement(x, 0), Stmt::ImportStmt {
            module: DottedIdentifier{attributes: vec!(Identifier::from("foo"), Identifier::from("bar"), Identifier::from("baz"))}
        });
    }

    #[test]
    fn test_returns() {
        check_match("return true", |x| statement(x, 0), Stmt::ReturnStmt {
            value: Expr::from(true)
        });

        check_match("yield true", |x| statement(x, 0), Stmt::YieldStmt (Expr::from(true)));
    }

    #[test]
    fn test_for_in() {
        check_match("for x in y:\n a=true", |x| statement(x, 0), Stmt::ForInStmt {
            iter_var: Identifier::from("x"),
            iterator: Expr::from("y"),
            block: output(block("a=true".as_bytes(), 0))
        });
    }

    #[test]
    fn test_comprehensions() {
        check_match("{x for x in y}", expression, Expr::SetComprehension {
            values: Box::new(Expr::from("x")),
            iterator_unpacking: vec![Identifier::from("x")],
            iterator: Box::new(Expr::from("y"))
        });

        check_match("{x:z for x in y}", expression, Expr::MapComprehension {
            keys: Box::new(Expr::from("x")),
            values: Box::new(Expr::from("z")),
            iterator_unpacking: vec![Identifier::from("x")],
            iterator: Box::new(Expr::from("y"))
        });

        check_match("[x for x in y]", expression, Expr::VecComprehension {
            values: Box::new(Expr::from("x")),
            iterator_unpacking: vec![Identifier::from("x")],
            iterator: Box::new(Expr::from("y"))
        });
    }

    #[test]
    fn test_simple_statements() {
        check_match("pass", |x| statement(x, 0), Stmt::PassStmt);
        check_match("continue", |x| statement(x, 0), Stmt::ContinueStmt);
        check_match("break", |x| statement(x, 0), Stmt::BreakStmt);
    }

    #[test]
    fn test_match() {
        check_match("match x:\n5 => 5", expression, Expr::MatchExpr {
            value: Box::new(Expr::from("x")),
            cases: vec![(output(expression("5".as_bytes())), output(expression("5".as_bytes())))]
        });
    }
}
