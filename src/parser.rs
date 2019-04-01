use std::str;
use std::io::prelude::*;
use std::fs::File;
use std::str::from_utf8;
use std::collections::HashMap;

extern crate cute;
extern crate nom;
use self::nom::*;
use self::nom::IResult::Done as Done;
use expression::*;
use utils::*;

type StmtNode = IdNode<Stmt2>;
type ExprNode = IdNode<Expr2>;
type StmtRes<'a> = IResult<&'a[u8], StmtNode>;
type ExprRes<'a> = IResult<&'a [u8], ExprNode>;
// type NodeRes<'a> = IResult<&'a[u8], IdNode>;


/// Create a rule of the form: KEYWORD SUBPARSER COLON BLOCK
/// if, elif, except, fn are all rules of this form.
macro_rules! line_then_block (
    ($i:expr, $keyword: expr, $submac: ident!($($args:tt)* ), $indent: expr) => (
        tuple!($i,
            delimited!(
                terminated!(
                    tag!($keyword),
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

pub fn module(input: &[u8]) -> IResult<&[u8], IdNode<Module2>>{
    let parse_result = preceded!(input,
    opt!(between_statement),
    many1!(complete!(
        terminated!(
            call!(function_declaration, 0),
            between_statement
        )
    ))
);

return fmap_idnode(parse_result, |x| Module2{declarations: x}.into());
}


pub fn reserved_list() -> Vec<&'static str>{
    let list: Vec<&'static str> = vec!("if", "else", "elif", "for", "while", "and", "or", "not", "xor", "fn", "import", "true", "false", "in", "match", "pass", "continue", "break", "yield", "let");
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

/// Parse a function declaration.
fn function_declaration<'a>(input: &'a [u8], indent: usize) -> StmtRes {
    let arg_parser = |i: &'a [u8]| tuple!(i,
        identifier,
        preceded!(
            open_paren,
            args_dec_list
        ),
        vararg,
        keyword_args,
        terminated!(
            kwvararg,
            close_paren
        ),
        opt!(complete!(preceded!(
            w_followed!(tag!("->")),
            type_annotation
        )))
    );

    let parse_result = line_then_block!(input, "fn", arg_parser, indent);

    return fmap_idnode(parse_result, |((name, args, vararg, keyword_args, varkwarg, return_type), body)| Stmt2::FunctionDecStmt{
        name: name,
        args: args,
        vararg: vararg,
        kwargs: keyword_args,
        varkwarg: varkwarg,
        block: body,
        return_type: return_type
    });
}

/// Match all normal arguments.
fn args_dec_list(input: &[u8]) -> IResult<&[u8], Vec<TypedIdent>> {
    inline_wrapped!(input,
        separated_list_complete!(
            w_followed!(tag!(",")),
            terminated!(
                typed_identifier,
                alt!(recognize!(many1!(inline_whitespace_char)) |
                peek!(tag!(",")) |
                peek!(tag!(")")))
            )
        )
    )
}

/// Match the variable length argument.
fn vararg(input: &[u8]) -> IResult<&[u8], Option<Identifier>> {
    return opt!(input, complete!(preceded!(
        tuple!(
            w_followed!(tag!(",")),
            w_followed!(tag!("*"))
        ),
        w_followed!(identifier)
    )));
}

/// Match all default arguments
fn keyword_args(input: &[u8]) -> IResult<&[u8], Vec<(TypedIdent, IdNode<Expr2>)>> {
    let parse_result = opt!(input, complete!(preceded!(
        w_followed!(tag!(",")),
        w_followed!(separated_list_complete!(inline_wrapped!(tag!(",")),
            tuple!(
                w_followed!(typed_identifier),
                preceded!(
                    w_followed!(tag!("=")),
                    w_followed!(expression)
                )
            )
        ))
    )));

    return fmap_iresult(parse_result, |x| match x {
        Some(y) => y,
        None => vec!()
    });
}

/// Match the variable length keyword argument.
named!(kwvararg<&[u8], Option<Identifier>>,
    opt!(complete!(preceded!(
        tuple!(
            w_followed!(tag!(",")),
            w_followed!(tag!("**"))
        ),
        w_followed!(identifier)
    )))
);

fn variable_unpacking(input: &[u8]) -> IResult<&[u8], Vec<Identifier>> {
    return separated_nonempty_list_complete!(input,
        w_followed!(tag!(",")),
        w_followed!(identifier)
    );
}

fn type_annotation(input: &[u8]) -> IResult<&[u8], TypeAnnotation> {
    let parse_result = identifier(input);

    return fmap_iresult(parse_result, |x| TypeAnnotation::Simple(x));
}

// TODO: Merge block_rule with block
fn block_rule(input: &[u8], minimum_indent: usize) -> IResult<&[u8], Vec<StmtNode>> {
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
        let statements = preceded!(input,
            opt!(between_statement),
            many1!(
                complete!(
                    terminated!(
                        indented!(call!(statement, expected_indent), expected_indent),
                        between_statement
                    )
                )
            )
        );

        return statements;
    }

}

fn block(input: &[u8], indent: usize) -> IResult<&[u8], IdNode<Block2>> {
    let parse_result = block_rule(input, indent);
    return fmap_idnode(parse_result, |x| Block2{statements: x});
}

/// Match any statement.
pub fn statement(input: &[u8], indent: usize) -> StmtRes {
    let node = alt_complete!(input,
        let_stmt |
        assignment_stmt |
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
    let parse_result = tuple!(input, initial_keyword!("import"), dotted_identifier);
    return fmap_idnode(parse_result,|x| Stmt2::ImportStmt (x.1));
}

/// Match a return statement.
fn return_stmt(input: &[u8]) -> StmtRes {
    let parse_result = tuple!(input, initial_keyword!("return"), expression);
    return fmap_idnode(parse_result,|x| Stmt2::ReturnStmt (x.1));
}

/// Parse a while loop.
fn while_stmt(input: &[u8], indent: usize) -> StmtRes {
    let parse_result = line_then_block!(input, "while", expression, indent);
    return fmap_idnode(parse_result, |x| Stmt2::WhileStmt {condition: x.0, block: x.1});
}

/// Parse a for in loop.
fn for_in(input: &[u8], indent: usize) -> StmtRes {
    let parse_result = line_then_block!(input, "for", tuple!(
        w_followed!(identifier),
        preceded!(
            inline_keyword!("in"),
            w_followed!(expression)
        )
    ), indent);

    return fmap_idnode(parse_result, |x| Stmt2::ForInStmt {iter_vars: (x.0).0, iterator: (x.0).1, block: x.1});
}

fn if_stmt(input: &[u8], indent: usize) -> StmtRes {
    let parse_result = tuple!(input,
        line_then_block!("if", expression, indent),
        many0!(indented!(line_then_block!("elif", expression, indent), indent)),
        opt!(complete!(indented!(keyword_then_block!("else", indent), indent)))
    );

    return fmap_idnode(parse_result, |x|Stmt2::IfStmt{condition: (x.0).0, block: (x.0).1, elifs: x.1, else_block: x.2});
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

    return fmap_idnode(parse_result, |x| Stmt2::TryExceptStmt {
        block: x.0,
        exceptions: x.1,
        else_block: x.2,
        final_block: x.3
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

fn let_stmt(input: &[u8]) -> StmtRes {
    let parse_result = separated_pair!(input,
        preceded!(
            tuple!(tag!("let"), many1!(inline_whitespace_char)),
            typed_identifier
        ),
        w_followed!(tag!("=")),
        w_followed!(expression)
    );

    return fmap_idnode(parse_result, |x| Stmt2::LetStmt {typed_name: x.0, expression: x.1});
}

pub fn assignment_stmt(input: &[u8]) -> StmtRes {
    let parse_result = terminated!(input,
        tuple!(
            identifier,
            w_followed!(assignments),
            w_followed!(expression)
        ),
        alt_complete!(recognize!(newline)| custom_eof)
    );

    return fmap_idnode(parse_result, |x| Stmt2::AssignmentStmt{
        name: x.0, operator:Assignment::from(from_utf8(x.1).unwrap()), expression: x.2
    });
}

fn break_stmt(input: &[u8]) -> StmtRes {
    let parse_result = terminated!(input,
        tag!("break"),
        terminated!(
            inline_whitespace,
            eof_or_line
        )
    );

    return fmap_idnode(parse_result, |x| Stmt2::BreakStmt);
}

fn pass_stmt(input: &[u8]) -> StmtRes {
    let parse_result = terminated!(input,
        tag!("pass"),
        terminated!(
            inline_whitespace,
            eof_or_line
        )
    );

    return fmap_idnode(parse_result, |_x| Stmt2::PassStmt);
}

fn continue_stmt(input: &[u8]) -> StmtRes {
    let parse_result = terminated!(input,
        tag!("continue"),
        terminated!(
            inline_whitespace,
            eof_or_line
        )
    );

    return fmap_idnode(parse_result, |_x| Stmt2::ContinueStmt);
}

fn yield_stmt(input: &[u8]) -> StmtRes {
    let parse_result = preceded!(input,
        initial_keyword!("yield"),
        w_followed!(expression)
    );

    return fmap_idnode(parse_result, |x| Stmt2::YieldStmt(x))
}

pub fn expression(input: &[u8]) -> ExprRes {
    return alt_complete!(input, comparison);
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
type opt_binary<'a> = (IdNode<Expr2>, Option<(&'a [u8], IdNode<Expr2>)>);
fn comparison(input: &[u8]) -> ExprRes {
    let parse_result = tuple!(input,
        alt!(match_expr | boolean_op_expr),
        opt!(complete!(tuple!(
            w_followed!(comparisons),
            boolean_op_expr
        )))
    );

    let map = |x: (IdNode<Expr2>, Option<(&[u8], IdNode<Expr2>)>)| match x.1 {
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
            IdNode::from(Expr2::ComparisonExpr{operator, left: Box::new(x.0), right: Box::new(y.1)})
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
                w_followed!(tag!(":")),
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

    return fmap_idnode(parse_result, |x| Expr2::MatchExpr {value: Box::new(x.0), cases: x.1});
}

/// Match a single binary expression.
fn match_binary_expr(operator: BinaryOperator, output: (IdNode<Expr2>, Option<IdNode<Expr2>>)) -> IdNode<Expr2> {
    return match output.1 {
        Some(x) => IdNode::from(Expr2::BinaryExpr {operator, left: Box::new(output.0), right: Box::new(x)}),
        None => output.0
    };
}

/// Create a binary expression, where one of several operators is possible.
fn match_binary_exprs(operators: &HashMap<&[u8], BinaryOperator>, output: (IdNode<Expr2>, Option<(&[u8], IdNode<Expr2>)>)) -> IdNode<Expr2> {
    return match output.1 {
        Some(x) => {
            let op: BinaryOperator = *operators.get(x.0).unwrap();
            IdNode::from(Expr2::BinaryExpr {operator: op, left: Box::new(output.0), right: Box::new(x.1)})
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
fn binary_op_symbol<'a>(input: &'a [u8], symbol: &str, operator: BinaryOperator, next_expr: fn(&[u8]) -> ExprRes) -> ExprRes<'a> {
    let parse_result = tuple!(input,
        w_followed!(next_expr),
        opt!(complete!(preceded!(
            w_followed!(tag!(symbol)),
            call!(binary_op_symbol, symbol, operator, next_expr)
        )))
    );

    let node = fmap_iresult(parse_result, |x| match_binary_expr(operator, x));
    return node;
}

/// Match a binary expression whose operator is a keyword
/// Currently only used for and, or, and xor.
fn binary_keyword_list<'a>(input: &'a [u8], symbols: &Vec<&str>, operators: &HashMap<&[u8], BinaryOperator>, next_expr: fn(&[u8]) -> ExprRes) -> ExprRes<'a> {
    let parse_result = tuple!(input,
        w_followed!(next_expr),
        opt!(tuple!(
            w_followed!(call!(match_any, symbols)),
            call!(binary_op_list, symbols, operators, next_expr)
        ))
    );

    let node = fmap_iresult(parse_result, |x| match_binary_exprs(operators, x));
    return node;
}

/// Match a list of binary operations
fn binary_op_list<'a>(input: &'a [u8], symbols: &Vec<&str>, operators: &HashMap<&[u8], BinaryOperator>, next_expr: fn(&[u8]) -> ExprRes) -> ExprRes<'a> {
    let parse_result = tuple!(input,
        w_followed!(next_expr),
        opt!(tuple!(
            w_followed!(call!(match_any, symbols)),
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
fn unary_expr(input: & [u8]) -> ExprRes {
    let parse_result = alt!(input,
        tuple!(
            map!(w_followed!(alt!(tag!("+") | tag!("-") | tag!("~") | inline_keyword!("not"))), Some),
            unary_expr)
         |
        tuple!(
            value!(None, tag!("")),
            power_expr
        )
    );

    let node = fmap_iresult(parse_result, |x|
        match x.0 {
            Some(y) => {
                let unary_op = match from_utf8(y).unwrap() {
                    "+" => UnaryOperator::Positive,
                    "-" => UnaryOperator::Negative,
                    "~" => UnaryOperator::BitNot,
                    "not" => UnaryOperator::Not,
                    _ => panic!()

                };
                IdNode::from(Expr2::UnaryExpr {operator: unary_op, operand: Box::new(x.1)})
            },
            None => x.1

        });
    return node;
}

/// An exponentiation.
fn power_expr(input: &[u8]) -> ExprRes {
    return binary_op_symbol(input, "**", BinaryOperator::Exponent, atomic_expr);
}

/// Parse dot separated identifiers.
/// e.g. ident1.ident2   .   ident3
fn dotted_identifier(input: &[u8]) -> IResult<&[u8], DottedIdentifier> {
    let parse_result = separated_nonempty_list_complete!(input,
        w_followed!(tag!(".")),
        identifier
    );

    return fmap_iresult(parse_result, |x: Vec<Identifier>| DottedIdentifier{attributes: x});
}

//TODO get rid of all the bits
fn atomic_expr(input: &[u8]) -> ExprRes {
    let node = alt_complete!(input,
        bool_expr |
        float |
        int |
        string |
        delimited!(
            w_followed!(tag!("{")),
            alt_complete!(
                map_or_set_comprehension |
                set_literal |
                map_literal
            ),
            inline_wrapped!(tag!("}"))
        ) |
        delimited!(
            inline_wrapped!(tag!("[")),
            alt_complete!(vector_comprehension | vec_literal),
            inline_wrapped!(tag!("]"))
        ) |
        expr_with_trailer
    );
    return node;
}

/// Match the for part of a comprehension.
fn comprehension_for(input: &[u8]) -> IResult<&[u8], ComprehensionIter2> {
    let parse_result = tuple!(input,
        delimited!(
            inline_keyword!("for"),
            variable_unpacking,
            inline_keyword!("in")
        ),
        boolean_op_expr,
        comprehension_if
    );

    return fmap_iresult(parse_result, |(iter_vars, iterator, if_clauses)| ComprehensionIter2{
        iter_vars: iter_vars,
        iterator: Box::new(iterator),
        if_clauses: if_clauses
    });
}

/// Match the if part of a comprehension.
fn comprehension_if(input: &[u8]) -> IResult<&[u8], Vec<IdNode<Expr2>>> {
    return many0!(input,
        preceded!(
            inline_keyword!("if"),
            boolean_op_expr
        )
    );
}

fn vec_literal(input: &[u8]) -> ExprRes {

    let parse_result = terminated!(input,
        separated_nonempty_list_complete!(
            inline_wrapped!(tag!(",")),
            inline_wrapped!(boolean_op_expr)
        ),
        peek!(close_bracket)
    );

    return fmap_idnode(parse_result, |x| Expr2::VecLiteral(x));
}

fn set_literal(input: &[u8]) -> ExprRes {
    let parse_result = terminated!(input,
        separated_nonempty_list_complete!(
            inline_wrapped!(tag!(",")),
            inline_wrapped!(boolean_op_expr)
        ),
        peek!(close_brace)
    );

    return fmap_idnode(parse_result, |x| Expr2::SetLiteral(x));
}

/// Match a map literal.
fn map_literal(input: &[u8]) -> ExprRes {

    let parse_result = terminated!(input,
        separated_nonempty_list_complete!(
            inline_wrapped!(tag!(",")),
            separated_pair!(
                inline_wrapped!(identifier),
                inline_wrapped!(tag!(":")),
                inline_wrapped!(boolean_op_expr)
            )
        ),
        peek!(close_brace)
    );

    return fmap_idnode(parse_result, |x| Expr2::MapLiteral (x));
}

/// Match a tuple literal
/// e.g. (), (1, ), (1,2,3), (1,2,3,)
fn tuple_literal(input: &[u8]) -> ExprRes {
    let parse_result = alt_complete!(input,
        map!(
            terminated!(
                inline_whitespace,
                peek!(close_paren)
            ), |_| vec!()) |
        map!(
            terminated!(
                inline_wrapped!(boolean_op_expr),
                tuple!(
                    comma,
                    peek!(close_paren)
                )
            ), |x| vec!(x)
        ) |
        terminated!(
            separated_at_least_m!(2, comma, boolean_op_expr),
            terminated!(
                opt!(complete!(comma)),
                peek!(close_paren)
            )
        )
    );

    return fmap_idnode(parse_result, |x| Expr2::TupleLiteral(x));
}

/// Match a vector comprehension.
fn vector_comprehension(input: &[u8]) -> ExprRes {
    let parse_result = tuple!(input,
        inline_wrapped!(boolean_op_expr),
        many1!(comprehension_for)
    );

    return fmap_idnode(parse_result, |x| Expr2::VecComprehension {
        
        values: Box::new(x.0),
        iterators: x.1
    });
}

fn generator_comprehension(input: &[u8]) -> ExprRes {
    let parse_result = tuple!(input,
        boolean_op_expr,
        many1!(comprehension_for)
    );

    return fmap_idnode(parse_result, |x| Expr2::GenComprehension {
        values: Box::new(x.0),
        iterators: x.1
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
            many1!(comprehension_for)
    );

    return fmap_idnode(parse_result, |(keys_or_values, values, iters)| match values {
        Some(y) => Expr2::MapComprehension {
            keys: Box::new(keys_or_values),
            values: Box::new(y),
            iterators: iters
        },
        None => Expr2::SetComprehension {
            values: Box::new(keys_or_values),
            iterators: iters
        }
    });
}

/// An expression wrapped in parentheses.
fn wrapped_expr(input: &[u8]) -> ExprRes {
    let node = delimited!(input,
        open_paren,
        alt_complete!(generator_comprehension | tuple_literal | expression),
        close_paren
    );

    return node;
}

/// An expression that can be followed by an arbitrary number of function calls or attribute accesses.
fn expr_with_trailer(input: &[u8]) -> ExprRes {
    let ident = |x| fmap_iresult(
        identifier(x),
        |y: Identifier| IdNode::from(Expr2::IdentifierExpr (y))
    );

    let parse_result = tuple!(input,
        alt!(ident | wrapped_expr),
        many0!(trailer)
    );

    let map = |x: (IdNode<Expr2>, Vec<PostIdent2>)| {
        let mut tree_base = x.0.data;
        for postval in x.1 {
            match postval {
                PostIdent2::Call{args, kwargs} => {
                    tree_base = Expr2::FunctionCall {function: Box::new(IdNode::from(tree_base)), args: args, kwargs: kwargs};
                },
                PostIdent2::Access{attributes} => {
                    tree_base = Expr2::AttributeAccess {base: Box::new(IdNode::from(tree_base)), attributes: attributes};
                }
                PostIdent2::Index{slices} => {
                    tree_base = Expr2::Index {slices: slices};
                }
            };
        };
        return IdNode::from(tree_base);
    };

    let node = fmap_iresult(parse_result, map);
    return node;
}

fn args_list(input: &[u8]) -> IResult<&[u8], Vec<IdNode<Expr2>>> {
    let parse_result = separated_nonempty_list_complete!(input,
        inline_wrapped!(tag!(",")),
        terminated!(
            w_followed!(boolean_op_expr),
            not!(equals)
        )
    );
    return parse_result;
}

fn kwargs_list(input: &[u8]) -> IResult<&[u8], Vec<(Identifier, IdNode<Expr2>)>> {
    let parse_result = separated_list!(input,
        inline_wrapped!(tag!(",")),
        tuple!(
            identifier,
            preceded!(
                inline_wrapped!(tag!("=")),
                boolean_op_expr
            )
        )
    );
    return parse_result;
}

fn post_call(input: &[u8]) -> IResult<&[u8], (Vec<IdNode<Expr2>>, Vec<(Identifier, IdNode<Expr2>)>)> {
    let parse_result = delimited!(input,
        open_paren,
        alt_complete!(
            tuple!(
                inline_wrapped!(args_list),
                opt!(preceded!(
                    comma,
                    inline_wrapped!(kwargs_list)
                ))
            ) |
            map!(
                w_followed!(kwargs_list), |x| (vec![], Some(x))
            )
        ),

        close_paren
    );
    return fmap_iresult(parse_result, |(x, y)| (x, match y {
        Some(z) => z,
        None => vec![]
    }));
}

fn post_index(input: &[u8]) -> IResult<&[u8], PostIdent2> {
    let parse_result = delimited!(input,
        open_bracket,
        separated_nonempty_list_complete!(
            comma,
            alt_complete!(
                tuple!(
                    map!(boolean_op_expr, |x| Some(x)),
                    optc!(tuple!(
                        preceded!(
                            colon,
                            boolean_op_expr
                        ),
                        optc!(preceded!(
                            colon,
                            boolean_op_expr
                        ))
                    ))
                ) |
                map!(colon, |_| (None, None))
            )
        ),
        close_bracket
    );

    fn flatten((lower_or_upper, rest): (Option<IdNode<Expr2>>, Option<(IdNode<Expr2>, Option<IdNode<Expr2>>)>)) -> (Option<IdNode<Expr2>>, Option<IdNode<Expr2>>, Option<IdNode<Expr2>>) {
        match lower_or_upper {
            Some(_) => {
                match rest {
                    Some((upper, step)) => (lower_or_upper, Some(upper), step),
                    None => (lower_or_upper, None, None)
                }
            },
            None => (None, None, None)
        }
    }

    return fmap_iresult(parse_result, |x: Vec<(Option<IdNode<Expr2>>, Option<(IdNode<Expr2>, Option<IdNode<Expr2>>)>)>| PostIdent2::Index {
        slices: c![flatten(y), for y in x]
    });
}

// named!(post_access<&[u8], Vec<Identifier>>,
//     many1!(
//         preceded!(
//             inline_wrapped!(tag!(".")),
//             identifier
//         )
//     )
// );

fn post_access(input: &[u8]) -> IResult<&[u8], Vec<Identifier>> {
    return many1!(input, 
        preceded!(
            inline_wrapped!(tag!(".")),
            identifier
        )
    );
}

fn trailer(input: &[u8]) -> IResult<&[u8], PostIdent2> {
    let call_to_enum = |x: (Vec<IdNode<Expr2>>, Vec<(Identifier, IdNode<Expr2>)>)| PostIdent2::Call{args: x.0, kwargs: x.1};
    let access_to_enum = |x: Vec<Identifier>| PostIdent2::Access{attributes: x};
    let result = alt!(input,
        map!(post_call, call_to_enum) |
        map!(post_access, access_to_enum) |
        post_index
    );
    return result;
}

/// Parser to return an Identifier AST.
fn identifier(input: &[u8]) -> IResult<&[u8], Identifier> {
    let parse_result = inline_wrapped!(input,
        recognize!(
            pair!(
                not!(peek!(tuple!(reserved_words, not!(valid_identifier_char)))),
                pair!(
                    alt!(alpha | tag!("_")),
                    many0!(valid_identifier_char)
                )
            )
        )
    );
    return fmap_iresult(parse_result,  Identifier::from);
}

fn typed_identifier(input: &[u8]) -> IResult<&[u8], TypedIdent> {
    let parse_result = tuple!(input,
        identifier,
        opt!(complete!(preceded!(inline_wrapped!(tag!(":")), type_annotation)))
    );

    return fmap_iresult(parse_result, |x| TypedIdent{name: x.0, type_annotation: x.1});
}

fn bool_expr(input: &[u8]) -> ExprRes {
    let parse_result= alt!(input,
        terminated!(tag!("true"), peek!(not!(valid_identifier_char))) |
        terminated!(tag!("false"), peek!(not!(valid_identifier_char)))
    );
    return fmap_idnode(parse_result, |x| Expr2::Bool(match from_utf8(x).unwrap() {
        "true" => true,
        "false" => false,
        _ => panic!()
    }));
}

// TODO: Hex encoded, byte encoded
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
    return fmap_idnode(parse_result, |x| Expr2::Int(from_utf8(x).unwrap().to_string()));
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

    return fmap_idnode(parse_result, |x| Expr2::Float(from_utf8(x).unwrap().to_string()));
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

    return fmap_idnode(parse_result, |x: &[u8]| Expr2::String(from_utf8(x).unwrap().to_string()));
}

pub fn read_from_file(f_name: &str) -> String {
    //let filename= format!("./test_data/{}.gr", f_name);
    let mut f = File::open(f_name).expect("File not found");
    let mut contents = String::new();
    match f.read_to_string(&mut contents) {
        Ok(_) => return contents,
        _ => panic!()
    };
}

#[cfg(test)]
mod tests {

    use super::*;
    use rand;
    use std::fmt::Debug;

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

    fn check_data<T>(input: &str, parser: fn(&[u8]) -> IResult<&[u8], IdNode<T>>, expected: T)
        where T: Debug + PartialEq + Eq {
        let res = parser(input.as_bytes());
        match res {
            Done(i, o) => {
                let l_r = format!("\n    Expected: {:?}\n    Actual: {:?}", expected, o);
                assert_eq!(i, "".as_bytes(), "Leftover input should have been empty, was: {:?}\nResults were: {}", from_utf8(i), l_r);
                assert_eq!(o.data, expected);
            },
            IResult::Error(e) => {
                println!("Error: {}. Input was: {}", e, input);
                panic!()
            },
            _ => panic!()
        }
    }

    fn simple_check_failed<T>(input: &str, parser: fn(&[u8]) -> IResult<&[u8], T>) {
        let res = parser(input.as_bytes());
        match res {
            IResult::Error(_) => {},
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

    #[cfg(test)]
    mod statements {
        use super::*;

        #[test]
        fn test_let() {
            check_data("let x: int = 3.0", |x| statement(x, 0), Stmt2::LetStmt {
                typed_name: TypedIdent {
                    name: Identifier::from("x"),
                    type_annotation: Some(TypeAnnotation::Simple(Identifier::from("int")))
                },
                expression: IdNode::from(Expr2::Float("3.0".to_string()))
            });
        }

        #[test]
        fn test_func_dec_parts() {
            // Args
            let expected = vec!(TypedIdent::from("a"));
            let actual = output(args_dec_list("a)".as_bytes()));
            assert_eq!(expected, actual);

            // Vararg
            let expected = Some(Identifier::from("args"));
            let actual = output(vararg(", *args".as_bytes()));
            assert_eq!(expected, actual);

            // Kwargs.
            let expected = vec!(
                   (TypedIdent::from("c"), output(expression("5".as_bytes()))),
                   (TypedIdent::from("d"), output(expression("7".as_bytes())))
            );
            let actual = output(keyword_args(", c=5, d=7".as_bytes()));
            assert_eq!(expected, actual);
        }

        #[test]
       fn test_func_dec() {
           check_data("fn x(a, b, *args, c=5, d=7, **kwargs):\n x = 5", |x| function_declaration(x, 0), Stmt2::FunctionDecStmt {
               name: Identifier::from("x"),
               args: c![TypedIdent::from(x), for x in vec!("a", "b")],
               vararg: Some(Identifier::from("args")),
               kwargs: vec!(
                   (TypedIdent::from("c"), output(expression("5".as_bytes()))),
                   (TypedIdent::from("d"), output(expression("7".as_bytes())))
               ),
               varkwarg: Some(Identifier::from("kwargs")),
               block: output(block("x=5\n".as_bytes(), 0)),
               return_type: None
           });

           check_data("fn x(a: int, c: int=5) -> int:\n x = 5", |x| function_declaration(x, 0), Stmt2::FunctionDecStmt {
               name: Identifier::from("x"),
               args: vec![TypedIdent{name: Identifier::from("a"), type_annotation: Some(TypeAnnotation::from("int"))}],
               vararg: None,
               kwargs: vec!(
                   (TypedIdent{name: Identifier::from("c"), type_annotation: Some(TypeAnnotation::from("int"))}, output(expression("5".as_bytes()))),
               ),
               varkwarg: None,
               block: output(block("x=5\n".as_bytes(), 0)),
               return_type: Some(TypeAnnotation::Simple(Identifier::from("int")))
           });

           check_data("fn a(b):\n let x = 5 + 6\n return x\n", |x| function_declaration(x, 0), Stmt2::FunctionDecStmt {
               name: Identifier::from("a"),
               args: vec!(TypedIdent::from("b")),
               vararg: None,
               kwargs: vec!(),
               varkwarg: None,
               block: output(block("let x = 5 + 6\nreturn x".as_bytes(), 0)),
               return_type: None
           });
       }

        #[test]
        fn test_try_except() {
            let blk = output(block("x=0".as_bytes(), 0));
            check_data("try    :     \n\n\n x = 0\n\n     \n\nexcept:\n x =      0     \nelse:\n x=0\nfinally:\n x=0     \n\n\n   \n\n", |x| try_except(x, 0), Stmt2::TryExceptStmt {
                block: blk.clone(),
                exceptions: vec!(blk.clone()),
                else_block: Some(blk.clone()),
                final_block: Some(blk.clone())
            });
        }

        #[test]
        fn test_assignment() {
            check_data("foo = true", assignment_stmt, Stmt2::AssignmentStmt {
                name: Identifier::from("foo"),
                operator: Assignment::Normal,
                expression: IdNode::from(true)
            });

            check_data("x = 0\n", assignment_stmt, Stmt2::AssignmentStmt {
                name: Identifier::from("x"),
                operator: Assignment::Normal,
                expression: IdNode::from(0)
            });

            let all_ops = vec!["&=", "|=", "^=", "+=", "-=", "*=", "/=", "%=", ">>=", "<<=", "**=", "="];
            for op in all_ops {
                let input = format!("x {} y", op);
                check_data(input.as_str(), assignment_stmt, Stmt2::AssignmentStmt {
                    name: Identifier::from("x"),
                    operator: Assignment::from(op),
                    expression: IdNode::from("y"),
                });
            }

        }

        #[test]
        fn test_if_stmt() {
            let good_input = "if (a and b):\n x = true";

            let good_output = Stmt2::IfStmt{
                condition: output(expression("a and b".as_bytes())),
                block: IdNode::from(Block2{statements: vec!(output(assignment_stmt("x = true".as_bytes())))}),
                elifs: vec!(),
                else_block: None
            };

            check_data(good_input, |x| statement(x, 0), good_output);

            check_failed("ifa and b:\n x = true", |x| statement(x, 0), nom::ErrorKind::Alt);

            check_data("if    true   :     \n\n\n  x = true\n elif    false   :   \n\n\n  y = true\n else     :  \n  z = true", |x| if_stmt(x, 1), Stmt2::IfStmt {
                
                condition: IdNode::from(true),
                block: output(block("x = true".as_bytes(), 0)),
                elifs: vec!((IdNode::from(false), output(block("y = true".as_bytes(), 0)))),
                else_block: Some(output(block("z = true".as_bytes(), 0)))
            });
        }

        #[test]
        fn test_while_stmt() {
            check_data("while true:\n x=true", |x| statement(x, 0), Stmt2::WhileStmt {
                condition: IdNode::from(true),
                block: IdNode::from(Block2{statements: vec!(output(assignment_stmt("x=true".as_bytes())))})
            });
        }

        #[test]
        fn test_simple_statements() {
            check_data("pass", |x| statement(x, 0), Stmt2::PassStmt);
            check_data("continue", |x| statement(x, 0), Stmt2::ContinueStmt);
            check_data("break", |x| statement(x, 0), Stmt2::BreakStmt);
        }

        #[test]
        fn test_import() {
            check_data("import foo.bar.baz", |x| statement(x, 0), Stmt2::ImportStmt(DottedIdentifier{
                attributes: vec!(Identifier::from("foo"), Identifier::from("bar"), Identifier::from("baz"))
            }));
        }

        #[test]
        fn test_returns() {
            check_data("return true", |x| statement(x, 0), Stmt2::ReturnStmt (IdNode::from(true)));

            check_data("yield true", |x| statement(x, 0), Stmt2::YieldStmt (IdNode::from(true)));
        }

        #[test]
        fn test_for_in() {
            check_data("for x in y:\n a=true", |x| statement(x, 0), Stmt2::ForInStmt {
                iter_vars: Identifier::from("x"),
                iterator: IdNode::from("y"),
                block: output(block("a=true".as_bytes(), 0))
            });
        }
    }

    #[cfg(test)]
    mod expressions {
        use super::*;

        #[test]
        fn test_parenthetical_expressions() {
            let expected = Expr2::BinaryExpr {
                operator: BinaryOperator::Or,
                left: Box::new(IdNode::from(Expr2::BinaryExpr {
                    operator:BinaryOperator::And,
                    left: Box::new(IdNode::from(true)),
                    right: Box::new(IdNode::from(false))
                })),
                right: Box::new(IdNode::from(true))
            };
            check_data("(true and false) or true", expression, expected);

            check_data("(true and false)", expression, Expr2::BinaryExpr {
                
                operator:BinaryOperator::And,
                left: Box::new(IdNode::from(true)),
                right: Box::new(IdNode::from(false))
            });
        }

        #[test]
        fn test_function_call() {
            let a = output(boolean_op_expr("true and false".as_bytes()));
            let b = output(expression("func()".as_bytes()));
            let expected = Expr2::FunctionCall{
                function: Box::new(IdNode::from(Expr2::IdentifierExpr(Identifier{name: "ident".to_string()}))), 
                args: vec!(a, b), 
                kwargs: vec!()
            };
            check_data("ident(true and false, func())", expression, expected);

            check_data("func(a, b, c=true, d=true)", expression, Expr2::FunctionCall {
                function: Box::new(IdNode::from("func")),
                args: c![IdNode::from(x), for x in vec!("a", "b")],
                kwargs: vec!(
                    (Identifier::from("c"), IdNode::from(true)),
                    (Identifier::from("d"), IdNode::from(true))
                )
            });
        }

        #[test]
        fn test_binary_expr() {
            check_data("true and false or true", expression, Expr2::BinaryExpr{
                
                operator: BinaryOperator::And,
                left: Box::new(IdNode::from(true)),
                right:Box::new(IdNode::from(Expr2::BinaryExpr{
                    
                    operator: BinaryOperator::Or,
                    left: Box::new(IdNode::from(false)),
                    right: Box::new(IdNode::from(true))
                }))
            });

            let all_ops = vec!["and", "or", "xor", "&", "|", "^", "+", "-", "*", "/", "%", ">>", "<<", "**"];
            for op in all_ops {
                let input = format!("x {} y", op);
                check_data(input.as_str(), expression, Expr2::BinaryExpr {
                    
                    operator: BinaryOperator::from(op),
                    left: Box::new(IdNode::from("x")),
                    right: Box::new(IdNode::from("y")),
                });
            }
        }

        #[test]
        fn test_unary_expr() {
            let ops = vec!["not", "+", "-", "~"];
            for op in ops {
                let input = format!("{} y", op);
                check_data(input.as_str(), expression, Expr2::UnaryExpr {
                    
                    operator: UnaryOperator::from(op),
                    operand: Box::new(IdNode::from("y")),
                });
            }
            check_data("~+y", expression, Expr2::UnaryExpr {
                operator: UnaryOperator::BitNot,
                operand: Box::new(output(expression("+y".as_bytes()))),
            });
            check_data("not true", expression, Expr2::UnaryExpr {operator: UnaryOperator::Not, operand: Box::new(IdNode::from(true))});
        }

        #[test]
        fn test_identifier_expr() {
            let identifier_expr = expression("words".as_bytes());
            let expected = Expr2::IdentifierExpr(Identifier{name: "words".to_string()});
            assert_eq!(identifier_expr, Done("".as_bytes(), IdNode::from(expected)));
        }

        #[test]
        fn test_comparison_expr() {
            let comp_strs = vec![">", "<", ">=", "<=", "==", "!="];
            let comp_ops = vec![ComparisonOperator::Greater, ComparisonOperator::Less, ComparisonOperator::GreaterEqual,
                                ComparisonOperator::LessEqual, ComparisonOperator::Equal, ComparisonOperator::Unequal];
            for (comp_str, comp_op) in comp_strs.iter().zip(comp_ops.iter()) {
                let as_str = format!("true {} false", comp_str);
                let expr = expression(as_str.as_bytes());
                let expected = IdNode::from(Expr2::ComparisonExpr{
                    
                    left: Box::new(IdNode::from(true)),
                    right: Box::new(IdNode::from(false)),
                    operator: *comp_op
                });

                assert_eq!(expr, Done("".as_bytes(), expected));
            }
        }

        #[test]
        fn test_repeated_func_calls() {
            let expected = Expr2::FunctionCall{
                function: Box::new(IdNode::from(Expr2::FunctionCall{
                    function: Box::new(IdNode::from("func")), 
                    args: vec!(IdNode::from("a")), 
                    kwargs: vec!()
                })),
                args: vec!(IdNode::from("b"), IdNode::from("c")),
                kwargs: vec!()
            };
            check_data("func(a)(b, c)", expression, expected);

            check_data("(a and b)(true)", expression, Expr2::FunctionCall {
                function: Box::new(output(boolean_op_expr("a and b".as_bytes()))),
                args: vec!(IdNode::from(true)),
                kwargs: vec!()
            });
        }

        #[test]
        fn test_comprehensions() {
            check_match("{x for x in y}", expression, IdNode::from(Expr2::SetComprehension {
                
                values: Box::new(IdNode::from("x")),
                iterators: vec!(ComprehensionIter2 {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(IdNode::from("y")),
                    if_clauses: vec!()
                })
            }));

            check_match("{x:z for x in y}", expression, IdNode::from(Expr2::MapComprehension {
                
                keys: Box::new(IdNode::from("x")),
                values: Box::new(IdNode::from("z")),
                iterators: vec!(ComprehensionIter2 {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(IdNode::from("y")),
                    if_clauses: vec!()
                })
            }));

            check_match("[x for x in y]", expression, IdNode::from(Expr2::VecComprehension {
                
                values: Box::new(IdNode::from("x")),
                iterators: vec!(ComprehensionIter2 {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(IdNode::from("y")),
                    if_clauses: vec!()
                })
            }));

            check_match("(x for x in y)", expression, IdNode::from(Expr2::GenComprehension {
                values: Box::new(IdNode::from("x")),
                iterators: vec!(ComprehensionIter2 {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(IdNode::from("y")),
                    if_clauses: vec!()
                })
            }));

            check_match("[x for x in y for x in y]", expression, IdNode::from(Expr2::VecComprehension {
                values: Box::new(IdNode::from("x")),
                iterators: vec!(ComprehensionIter2 {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(IdNode::from("y")),
                    if_clauses: vec!()
                }, ComprehensionIter2 {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(IdNode::from("y")),
                    if_clauses: vec!()
                })
            }));

            check_match("for a, b in c if true if false", comprehension_for, ComprehensionIter2{
                iter_vars: c![Identifier::from(x), for x in vec!("a", "b")],
                iterator: Box::new(IdNode::from("c")),
                if_clauses: c![IdNode::from(x), for x in vec!(true, false)]
            });
        }

        #[test]
        fn test_match() {
            check_match("match x:\n5 => 5", expression, IdNode::from(Expr2::MatchExpr {
                value: Box::new(IdNode::from("x")),
                cases: vec![(output(expression("5".as_bytes())), output(expression("5".as_bytes())))]
            }));
        }

        #[test]
        fn test_literals() {
            let int = rand::random::<i64>().abs();
            check_match(&int.to_string(), expression, IdNode::from(int));
            let rand_float = rand::random::<f64>().abs();
            check_match(&rand_float.to_string(), float, IdNode::from(rand_float));
            let expr = Expr2::String("\"asdf\\\"\\\rasdf\"".to_string());
            check_match("\"asdf\\\"\\\rasdf\"", expression, IdNode::from(expr));

            check_match("{x : y}", expression, IdNode::from(Expr2::MapLiteral(vec!((Identifier::from("x"), IdNode::from("y"))))));
            check_match("[true, false]", expression, IdNode::from(Expr2::VecLiteral(vec!(IdNode::from(true), IdNode::from(false)))));
            check_match("{true, false}", expression, IdNode::from(Expr2::SetLiteral(vec!(IdNode::from(true), IdNode::from(false)))));
            check_match("(true, false)", expression, IdNode::from(Expr2::TupleLiteral(vec!(IdNode::from(true), IdNode::from(false)))));
        }
    }

    #[test]
//    fn test_module() {
//        let module_str = "fn a():\n return 0\n\nfn b():\n return 1";
//        check_match(module_str, module, Module{
//            declarations: vec!(
//                output(function_declaration("fn a():\n return 0".as_bytes(), 0)),
//                output(function_declaration("fn b():\n return 1".as_bytes(), 0))
//            )
//        })
//    }

    #[test]
    fn test_block() {
        let exp_block = Block2 {
            statements: vec![
                output(assignment_stmt("x=0\n".as_bytes())),
                output(assignment_stmt("y=true".as_bytes()))
            ]
        };

        check_match(" x=0\n y=true\n\n  \n", |x| block(x, 1), IdNode::from(exp_block));
    }

    #[test]
    fn test_reserved_words() {
        for keyword in reserved_list() {
            let result = identifier(keyword.as_bytes());
            assert_eq!(result, IResult::Error(ErrorKind::Not));
        }
    }

    #[test]
    fn test_dotted_identifier() {
        let expected = DottedIdentifier{attributes: vec!(Identifier::from("asdf"), Identifier::from("dfgr_1"), Identifier::from("_asdf"))};
        check_match("asdf.dfgr_1   .   _asdf", dotted_identifier, expected);
    }

    #[test]
    fn test_post_ident() {
        let expected_args = vec!("a", "b", "c").iter().map(|x| IdNode::from(*x)).collect();
        check_match("( a ,  b , c ) ", trailer, PostIdent2::Call{args: expected_args, kwargs: vec![]});
        check_match("( a   ,  b  =    true)", trailer, PostIdent2::Call {
            args: vec!(IdNode::from("a")),
            kwargs: vec!((Identifier::from("b"), IdNode::from(true)))
        });

        check_match("( a   = true,  b = true ) ", trailer, PostIdent2::Call {
            args: vec![],
            kwargs: vec![(Identifier::from("a"), IdNode::from(true)), (Identifier::from("b"), IdNode::from(true))]
        });



        simple_check_failed("(a | b=false)", trailer);
        simple_check_failed("(a   b=false)", trailer);
        simple_check_failed("(a,, b=false)", trailer);
        simple_check_failed("(a,, b)", trailer);
        simple_check_failed("(a, b =  true, c)", trailer);

//
//        check_match(".asdf_   .   asdf", trailer, PostIdent2::Access{attributes: vec!(Identifier::from("asdf_"), Identifier::from("asdf"))});
//
//        check_match("[a:b:c, :, d]", trailer, PostIdent2::Index {
//            slices: vec!(
//                (Some(IdNode::from("a")), Some(IdNode::from("b")), Some(IdNode::from("c"))),
//                (None, None, None),
//                (Some(IdNode::from("d")), None, None)
//            )
//        })
    }

}
