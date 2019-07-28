use std::str;
use std::str::from_utf8;
use std::collections::HashMap;

extern crate cute;
extern crate nom;
use self::nom::*;

use expression::*;
use parser_utils::*;
use parser_utils::tokens::*;
use typing;
use typing::Type;
use self::expr_parsers::*;

type StmtNode = Node<Stmt>;
type ExprNode = Node<Expr>;
type StmtRes<'a> = IResult<&'a[u8], StmtNode>;
type ExprRes<'a> = IResult<&'a[u8], ExprNode>;
type TypeRes<'a> = IResult<&'a[u8], typing::Type>;

pub trait Parseable {
    fn parse(input: &[u8]) -> Self;
}

impl Parseable for Node<Module> {
    fn parse(input: &[u8]) -> Node<Module> {
        return  output(module(input));
    }
}

impl Parseable for Node<Block> {
    fn parse(input: &[u8]) -> Node<Block> {
        return output(block(input, 0));
    }
}

impl Parseable for Node<Stmt> {
    fn parse(input: &[u8]) -> Node<Stmt> {
        return output(statement(input, 0));
    }
}

impl Parseable for Node<Expr> {
    fn parse(input: &[u8]) -> Node<Expr> {
        return output(expression(input));
    }
}

pub fn reserved_list() -> Vec<&'static str>{
    let list: Vec<&'static str> = vec!("if", "else", "elif", "for", "while", "and", "or", "not", "xor", "fn", "import", "true", "false", "in", "match", "pass", "continue", "break", "yield", "let");
    return list;
}

/// Return true if a key
pub fn reserved_words(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let tag_lam = |x: &[u8]| recognize!(input, complete!(tag!(x)));
    let list = reserved_list();
    let tag_iter = list.iter().map(|x| x.as_bytes()).map(tag_lam);
    let mut final_result: IResult<&[u8], &[u8]> = wrap_err(input, ErrorKind::Tag);
    for res in tag_iter {
        match res {
            Ok((i, o)) => {
                final_result = Ok((i, o));
                break;
            },
            _ => continue
        };
    }
    return final_result;
}

pub fn variable_unpacking(input: &[u8]) -> IResult<&[u8], Vec<Identifier>> {
    return separated_nonempty_list_complete!(input,
        w_followed!(tag!(",")),
        IDENTIFIER
    );
}

pub fn module(input: &[u8]) -> IResult<&[u8], Node<Module>>{
    let parse_result = preceded!(input,
        opt!(between_statement),
        many1!(complete!(
            terminated!(
                alt!(complete!(call!(function_declaration, 0)) | complete!(import)),
                between_statement
            )
        ))
    );

    return fmap_node(parse_result, |x| Module{declarations: x.into_iter().map(Box::new).collect()});
}

pub fn block(input: &[u8], indent: usize) -> IResult<&[u8], Node<Block>> {
    let first_indent_parse: IResult<&[u8], Vec<&[u8]>> = preceded!(input, opt!(between_statement), many0c!(tag!(" ")));
    let full_indent: (&[u8], Vec<&[u8]>) = match first_indent_parse {
        Ok((i, o)) => (i, o),
        _ => panic!()
    };

    // Break if the block is not indented enough.
    let parse_result = if full_indent.1.len() < indent {
        // TODO: This will happen if the indentation level is too low. Throw a proper error.
        Result::Err(Err::Error(Context::Code(input, ErrorKind::Count)))
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
        statements
    };
    return fmap_node(parse_result, |x| Block{statements: x.into_iter().map(Box::new).collect()});
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

/// Match all normal arguments.
pub fn args_dec_list(input: &[u8]) -> IResult<&[u8], Vec<(Identifier, Type)>> {
    inline_wrapped!(input,
        separated_list_complete!(
            w_followed!(tag!(",")),
            terminated!(
                tuple!(
                    IDENTIFIER,
                    preceded!(
                        colon,
                        type_parser::any_type
                    )
                ),
                alt!(recognize!(many1!(inline_whitespace_char)) |
                peek!(tag!(",")) |
                peek!(tag!(")")))
            )
        )
    )
}

/// Match the variable length argument.
pub fn vararg(input: &[u8]) -> IResult<&[u8], Option<Identifier>> {
    return opt!(input, complete!(preceded!(
        tuple!(
            w_followed!(tag!(",")),
            w_followed!(tag!("*"))
        ),
        IDENTIFIER
    )));
}

/// Match all default arguments
pub fn keyword_args(input: &[u8]) -> IResult<&[u8], Vec<(Identifier, Type, Node<Expr>)>> {
    let parse_result = opt!(input, complete!(preceded!(
        w_followed!(tag!(",")),
        w_followed!(separated_list_complete!(inline_wrapped!(tag!(",")),
            tuple!(
                IDENTIFIER,
                preceded!(
                    colon,
                    type_parser::any_type
                ),
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

/// Parse a variable length keyword argument
/// e.g. "**varkwarg"
pub fn varkwarg(input: &[u8]) -> IResult<&[u8], Option<Identifier>> {
    return opt!(input, complete!(preceded!(
        tuple!(
            w_followed!(tag!(",")),
            w_followed!(tag!("**"))
        ),
        IDENTIFIER
    )));
}

/// Parse a function declaration.
pub fn function_declaration<'a>(input: &'a [u8], indent: usize) -> StmtRes {
    let arg_parser = |i: &'a [u8]| tuple!(i,
        IDENTIFIER,
        preceded!(
            open_paren,
            args_dec_list
        ),
        vararg,
        keyword_args,
        terminated!(
            varkwarg,
            close_paren
        ),
        opt!(complete!(preceded!(
            w_followed!(tag!("->")),
            type_parser::any_type
        )))
    );

    let parse_result = line_then_block!(input, "fn", arg_parser, indent);

    return fmap_node(parse_result, |((name, args, vararg, keyword_args, varkwarg, return_type), body)| Stmt::FunctionDecStmt{
        name: name,
        args: args,
        vararg: vararg,
        kwargs: keyword_args,
        varkwarg: varkwarg,
        block: body,
        return_type: match return_type {
            Some(x) => x,
            None => typing::Type::empty
        }
    });
}

/// Parse a while loop.
pub fn while_stmt(input: &[u8], indent: usize) -> StmtRes {
    let parse_result = line_then_block!(input, "while", expression, indent);
    return fmap_node(parse_result, |x| Stmt::WhileStmt {condition: x.0, block: x.1});
}

/// Parse a for in loop.
pub fn for_in(input: &[u8], indent: usize) -> StmtRes {
    let parse_result = line_then_block!(input, "for", tuple!(
        IDENTIFIER,
        preceded!(
            inline_keyword!("in"),
            w_followed!(expression)
        )
    ), indent);

    return fmap_node(parse_result, |x| Stmt::ForInStmt {iter_vars: (x.0).0, iterator: (x.0).1, block: x.1});
}

pub fn if_stmt(input: &[u8], indent: usize) -> StmtRes {
    let parse_result = tuple!(input,
        line_then_block!("if", expression, indent),
        many0c!(indented!(line_then_block!("elif", expression, indent), indent)),
        opt!(complete!(indented!(keyword_then_block!("else", indent), indent)))
    );

    return fmap_node(parse_result, |x|Stmt::IfStmt{condition: (x.0).0, block: (x.0).1, elifs: x.1, else_block: x.2});
}

/// Match a try except statement.
pub fn try_except(input: &[u8], indent: usize) -> StmtRes {
    let parse_result = tuple!(input,
        keyword_then_block!("try", indent),
        many1!(keyword_then_block!("except", indent)),
        opt!(complete!(
            keyword_then_block!("else", indent)
        )),
        opt!(complete!(keyword_then_block!("finally", indent)))
    );

    return fmap_node(parse_result, |x| Stmt::TryExceptStmt {
        block: x.0,
        exceptions: x.1,
        else_block: x.2,
        final_block: x.3
    });
}

pub fn let_stmt(input: &[u8]) -> StmtRes {
    let parse_result = separated_pair!(input,
        preceded!(
            tuple!(tag!("let"), many1!(inline_whitespace_char)),
            typed_identifier
        ),
        w_followed!(tag!("=")),
        w_followed!(expression)
    );

    return fmap_node(parse_result, |x| Stmt::LetStmt {typed_name: x.0, expression: x.1});
}

pub fn assignments(input: &[u8]) -> IResult<&[u8], &[u8]> {
    return recognize!(input, alt!(
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
    ));
}

pub fn assignment_stmt(input: &[u8]) -> StmtRes {
    let parse_result = terminated!(input,
        tuple!(
            IDENTIFIER,
            w_followed!(assignments),
            w_followed!(expression)
        ),
        alt_complete!(recognize!(NEWLINE)| custom_eof)
    );

    return fmap_node(parse_result, |x| Stmt::AssignmentStmt{
        name: x.0, operator:Assignment::from(from_utf8(x.1).unwrap()), expression: x.2
    });
}

/// Match an import statement.
pub fn import(input: &[u8]) -> StmtRes {
    let parse_result = tuple!(input, initial_keyword!("import"), dotted_identifier);
    return fmap_node(parse_result,|x| Stmt::ImportStmt (x.1));
}

/// Match a return statement.
pub fn return_stmt(input: &[u8]) -> StmtRes {
    let parse_result = tuple!(input, initial_keyword!("return"), expression);
    return fmap_node(parse_result,|x| Stmt::ReturnStmt (x.1));
}

pub fn yield_stmt(input: &[u8]) -> StmtRes {
    let parse_result = preceded!(input,
        initial_keyword!("yield"),
        w_followed!(expression)
    );

    return fmap_node(parse_result, |x| Stmt::YieldStmt(x))
}

pub fn break_stmt(input: &[u8]) -> StmtRes {
    let parse_result = terminated!(input,
        tag!("break"),
        terminated!(
            inline_whitespace,
            eof_or_line
        )
    );

    return fmap_node(parse_result, |_x| Stmt::BreakStmt);
}

pub fn pass_stmt(input: &[u8]) -> StmtRes {
    let parse_result = terminated!(input,
        tag!("pass"),
        terminated!(
            inline_whitespace,
            eof_or_line
        )
    );

    return fmap_node(parse_result, |_x| Stmt::PassStmt);
}

pub fn continue_stmt(input: &[u8]) -> StmtRes {
    let parse_result = terminated!(input,
        tag!("continue"),
        terminated!(
            inline_whitespace,
            eof_or_line
        )
    );

    return fmap_node(parse_result, |_x| Stmt::ContinueStmt);
}

pub fn expression(input: &[u8]) -> ExprRes {
    return alt_complete!(input, comparison);
}

pub fn comparisons(input: &[u8]) -> IResult<&[u8], &[u8]> {
    return recognize!(input, alt!(
        tag!("==") |
        tag!("<=") |
        tag!(">=") |
        tag!("!=") |
        tag!("<")  |
        tag!(">")
    ));
}

pub fn comparison(input: &[u8]) -> ExprRes {
    let parse_result = tuple!(input,
        alt!(match_expr | boolean_op_expr),
        opt!(complete!(tuple!(
            w_followed!(comparisons),
            boolean_op_expr
        )))
    );

    let map = |x: (Node<Expr>, Option<(&[u8], Node<Expr>)>)| match x.1 {
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
            Node::from(Expr::ComparisonExpr{operator, left: Box::new(x.0), right: Box::new(y.1)})
        }
    };

    let node = fmap_iresult(parse_result, map);
    return node;
}

pub fn match_expr(input: &[u8]) -> ExprRes {

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

    return fmap_node(parse_result, |x| Expr::MatchExpr {value: Box::new(x.0), cases: x.1});
}

/// Match a single binary expression.
pub fn match_binary_expr(operator: BinaryOperator, output: (Node<Expr>, Option<Node<Expr>>)) -> Node<Expr> {
    return match output.1 {
        Some(x) => Node::from(Expr::BinaryExpr {operator, left: Box::new(output.0), right: Box::new(x)}),
        None => output.0
    };
}

/// Create a binary expression, where one of several operators is possible.
pub fn match_binary_exprs(operators: &HashMap<&[u8], BinaryOperator>, output: (Node<Expr>, Option<(&[u8], Node<Expr>)>)) -> Node<Expr> {
    return match output.1 {
        Some(x) => {
            let op: BinaryOperator = *operators.get(x.0).unwrap();
            Node::from(Expr::BinaryExpr {operator: op, left: Box::new(output.0), right: Box::new(x.1)})
        },
        None => output.0
    };
}

/// Match bitwise boolean operations.
pub fn bit_boolean_op_expr(input: &[u8]) -> ExprRes {
    let symbols = vec!["&", "|", "^"];
    let operators = c!{k.as_bytes() => BinaryOperator::from(*k), for k in symbols.iter()};
    return binary_op_list(input, &symbols, &operators, bit_shift);
}

/// Match the bit shift operators.
pub fn bit_shift(input: &[u8]) -> ExprRes {
    let symbols = vec![">>", "<<"];
    let operators = c!{k.as_bytes() => BinaryOperator::from(*k), for k in symbols.iter()};
    return binary_op_list(input, &symbols, &operators, additive_expr);
}

/// Match addition and subtraction.
pub fn additive_expr(input: &[u8]) -> ExprRes {
    let symbols = vec!["+", "-"];
    let operators = c!{k.as_bytes() => BinaryOperator::from(*k), for k in symbols.iter()};
    return binary_op_list(input, &symbols, &operators, mult_expr);
}

/// Match multiplication, division, and modulo.
pub fn mult_expr(input: &[u8]) -> ExprRes {
    let symbols = vec!["*", "/", "%"];
    let operators = c!{k.as_bytes() => BinaryOperator::from(*k), for k in symbols.iter()};
    return binary_op_list(input, &symbols, &operators, unary_expr);
}

/// Match any unary expression.
/// Implemented as a single parser because all unary expressions have the same precedence.
pub fn unary_expr(input: & [u8]) -> ExprRes {
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
                Node::from(Expr::UnaryExpr {operator: unary_op, operand: Box::new(x.1)})
            },
            None => x.1

        });
    return node;
}

/// Parse dot separated identifiers.
/// e.g. ident1.ident2   .   ident3
pub fn dotted_identifier(input: &[u8]) -> IResult<&[u8], DottedIdentifier> {
    let parse_result = separated_nonempty_list_complete!(input,
        w_followed!(tag!(".")),
        IDENTIFIER
    );

    return fmap_iresult(parse_result, |x: Vec<Identifier>| DottedIdentifier{attributes: x});
}

//TODO get rid of all the bits
pub fn atomic_expr(input: &[u8]) -> ExprRes {
    let node = w_followed!(input, alt_complete!(
        bool_expr |
        float |
        int |
        string |
        delimited!(
            open_brace,
            alt_complete!(
                map_or_set_comprehension |
                set_literal |
                map_literal
            ),
            close_brace
        ) |
        delimited!(
            open_bracket,
            alt_complete!(vector_comprehension | vec_literal),
            close_bracket
        ) |
        expr_with_trailer
    ));
    return node;
}

/// Match the for part of a comprehension.
pub fn comprehension_for(input: &[u8]) -> IResult<&[u8], ComprehensionIter> {
    let parse_result = tuple!(input,
        delimited!(
            inline_keyword!("for"),
            variable_unpacking,
            inline_keyword!("in")
        ),
        boolean_op_expr,
        comprehension_if
    );

    return fmap_iresult(parse_result, |(iter_vars, iterator, if_clauses)| ComprehensionIter{
        iter_vars: iter_vars,
        iterator: Box::new(iterator),
        if_clauses: if_clauses
    });
}

/// Match the if part of a comprehension.
pub fn comprehension_if(input: &[u8]) -> IResult<&[u8], Vec<Node<Expr>>> {
    return many0c!(input,
        preceded!(
            inline_keyword!("if"),
            boolean_op_expr
        )
    );
}

/// Match a vector literal.
pub fn vec_literal(input: &[u8]) -> ExprRes {

    let parse_result = terminated!(input,
        separated_nonempty_list_complete!(
            inline_wrapped!(tag!(",")),
            inline_wrapped!(boolean_op_expr)
        ),
        peek!(close_bracket)
    );

    return fmap_node(parse_result, |x| Expr::VecLiteral(x));
}

/// Match a set literal.
pub fn set_literal(input: &[u8]) -> ExprRes {
    let parse_result = terminated!(input,
        separated_nonempty_list_complete!(
            inline_wrapped!(tag!(",")),
            inline_wrapped!(boolean_op_expr)
        ),
        peek!(close_brace)
    );

    return fmap_node(parse_result, |x| Expr::SetLiteral(x));
}

/// Match a map literal.
pub fn map_literal(input: &[u8]) -> ExprRes {

    let parse_result = terminated!(input,
        separated_nonempty_list_complete!(
            inline_wrapped!(tag!(",")),
            separated_pair!(
                IDENTIFIER,
                inline_wrapped!(tag!(":")),
                inline_wrapped!(boolean_op_expr)
            )
        ),
        peek!(close_brace)
    );

    return fmap_node(parse_result, |x| Expr::MapLiteral (x));
}

/// Match a tuple literal
/// e.g. (), (1, ), (1,2,3), (1,2,3,)
pub fn tuple_literal(input: &[u8]) -> ExprRes {
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

    return fmap_node(parse_result, |x| Expr::TupleLiteral(x));
}

/// Match a vector comprehension.
pub fn vector_comprehension(input: &[u8]) -> ExprRes {
    let parse_result = tuple!(input,
        inline_wrapped!(boolean_op_expr),
        many1!(comprehension_for)
    );

    return fmap_node(parse_result, |x| Expr::VecComprehension {
        
        values: Box::new(x.0),
        iterators: x.1
    });
}

/// Match a generator comprehension.
pub fn generator_comprehension(input: &[u8]) -> ExprRes {
    let parse_result = tuple!(input,
        boolean_op_expr,
        many1!(comprehension_for)
    );

    return fmap_node(parse_result, |x| Expr::GenComprehension {
        values: Box::new(x.0),
        iterators: x.1
    });
}

/// Match a map or a set.
pub fn map_or_set_comprehension(input: &[u8]) -> ExprRes {
    let parse_result = tuple!(input,
            boolean_op_expr,
            opt!(complete!(preceded!(
                inline_wrapped!(tag!(":")),
                boolean_op_expr
            ))),
            many1!(comprehension_for)
    );

    return fmap_node(parse_result, |(keys_or_values, values, iters)| match values {
        Some(y) => Expr::MapComprehension {
            keys: Box::new(keys_or_values),
            values: Box::new(y),
            iterators: iters
        },
        None => Expr::SetComprehension {
            values: Box::new(keys_or_values),
            iterators: iters
        }
    });
}

/// An expression wrapped in parentheses.
pub fn wrapped_expr(input: &[u8]) -> ExprRes {
    let node = delimited!(input,
        open_paren,
        alt_complete!(generator_comprehension | tuple_literal | expression),
        close_paren
    );

    return node;
}

/// An expression that can be followed by an arbitrary number of function calls or attribute accesses.
pub fn expr_with_trailer(input: &[u8]) -> ExprRes {
    let ident = |x| fmap_iresult(
        IDENTIFIER(x),
        |y: Identifier| Node::from(Expr::IdentifierExpr (y))
    );

    let parse_result = tuple!(input,
        alt!(ident | wrapped_expr),
        many0c!(trailer)
    );

    let map = |x: (Node<Expr>, Vec<PostIdent>)| {
        let mut tree_base = x.0.data;
        for postval in x.1 {
            match postval {
                PostIdent::Call{args, kwargs} => {
                    tree_base = Expr::FunctionCall {function: Box::new(Node::from(tree_base)), args: args, kwargs: kwargs};
                },
                PostIdent::Access{attributes} => {
                    tree_base = Expr::AttributeAccess {base: Box::new(Node::from(tree_base)), attributes: attributes};
                }
                PostIdent::Index{slices} => {
                    tree_base = Expr::Index {slices: slices};
                }
            };
        };
        return Node::from(tree_base);
    };

    let node = fmap_iresult(parse_result, map);
    return node;
}

/// Match a list of arguments.
pub fn args_list(input: &[u8]) -> IResult<&[u8], Vec<Node<Expr>>> {
    let parse_result = separated_nonempty_list_complete!(input,
        inline_wrapped!(tag!(",")),
        terminated!(
            w_followed!(boolean_op_expr),
            not!(equals)
        )
    );
    return parse_result;
}

/// Match a list of keyword arguments.
pub fn kwargs_list(input: &[u8]) -> IResult<&[u8], Vec<(Identifier, Node<Expr>)>> {
    let parse_result = separated_list!(input,
        inline_wrapped!(tag!(",")),
        tuple!(
            IDENTIFIER,
            preceded!(
                inline_wrapped!(tag!("=")),
                boolean_op_expr
            )
        )
    );
    return parse_result;
}

/// Match a function call following an expression.
pub fn post_call(input: &[u8]) -> IResult<&[u8], (Vec<Node<Expr>>, Vec<(Identifier, Node<Expr>)>)> {
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

/// Match an indexing operation following an expression.
pub fn post_index(input: &[u8]) -> IResult<&[u8], PostIdent> {
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

    fn flatten((lower_or_upper, rest): (Option<Node<Expr>>, Option<(Node<Expr>, Option<Node<Expr>>)>)) -> (Option<Node<Expr>>, Option<Node<Expr>>, Option<Node<Expr>>) {
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

    return fmap_iresult(parse_result, |x: Vec<(Option<Node<Expr>>, Option<(Node<Expr>, Option<Node<Expr>>)>)>| PostIdent::Index {
        slices: c![flatten(y), for y in x]
    });
}

/// Match an access operation following an expression.
pub fn post_access(input: &[u8]) -> IResult<&[u8], Vec<Identifier>> {
    return many1!(input, 
        preceded!(
            inline_wrapped!(tag!(".")),
            IDENTIFIER
        )
    );
}

/// Match a trailer behind an expression.
/// A trailer is a function call, an attribute access, or an index.
pub fn trailer(input: &[u8]) -> IResult<&[u8], PostIdent> {
    let call_to_enum = |x: (Vec<Node<Expr>>, Vec<(Identifier, Node<Expr>)>)| PostIdent::Call{args: x.0, kwargs: x.1};
    let access_to_enum = |x: Vec<Identifier>| PostIdent::Access{attributes: x};
    let result = alt!(input,
        map!(post_call, call_to_enum) |
        map!(post_access, access_to_enum) |
        post_index
    );
    return result;
}

/// Parser to return an Identifier AST.
pub fn identifier(input: &[u8]) -> IResult<&[u8], Identifier> {
    // let not_result: IResult<&[u8], (&[u8], Vec<&[u8]>)> = pair!(input, 
    //                 alt!(alpha | tag!("_")),
    //                 many0c!(valid_identifier_char)
    //             );
    let parse_result = inline_wrapped!(input,
        recognize!(
            pair!(
                not!(peek!(tuple!(reserved_words, not!(valid_identifier_char)))),
                pair!(
                    alt!(alpha | tag!("_")),
                    many0c!(valid_identifier_char)
                )
            )
        )
    );
    return fmap_iresult(parse_result,  Identifier::from);
}

pub fn typed_identifier(input: &[u8]) -> IResult<&[u8], TypedIdent> {
    let parse_result = tuple!(input,
        IDENTIFIER,
        opt!(complete!(preceded!(inline_wrapped!(tag!(":")), type_parser::any_type)))
    );

    return fmap_iresult(parse_result, |x| TypedIdent{name: x.0, type_annotation: x.1});
}

pub fn bool_expr(input: &[u8]) -> ExprRes {
    let parse_result= alt!(input,
        terminated!(tag!("true"), peek!(not!(IDENT_CHAR))) |
        terminated!(tag!("false"), peek!(not!(IDENT_CHAR)))
    );
    return fmap_node(parse_result, |x| Expr::Bool(match from_utf8(x).unwrap() {
        "true" => true,
        "false" => false,
        _ => panic!()
    }));
}

// TODO: Hex encoded, byte encoded
/// Match an integer literal.
pub fn int(input: &[u8]) -> ExprRes {
    let parse_result: IResult<&[u8], &[u8]> = recognize!(input,
        tuple!(
            optc!(sign),
            terminated!(
                DIGIT,
                VALID_NUM_FOLLOW
            )
        )
    );
    return fmap_node(parse_result, |x| Expr::Int(from_utf8(x).unwrap().to_string()));
}

/// Match a floating point literal.
pub fn float<'a>(input: &'a[u8]) -> ExprRes {

    let with_dec = |x: &'a[u8]| tuple!(x,
        tag!("."),
        many0c!(dec_digit),
        opt!(complete!(exponent))
    );

    let parse_result = recognize!(input, tuple!(
        opt!(sign),
        many0c!(dec_digit),
        alt!(
            value!((), with_dec) |
            value!((), complete!(exponent))
        ),
        VALID_NUM_FOLLOW
    ));

    return fmap_node(parse_result, |x| Expr::Float(from_utf8(x).unwrap().to_string()));
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

named!(string_literal<&[u8], &[u8]>,
    recognize!(
        tuple!(
            tag!("\""),
            many0c!(string_char),
            tag!("\"")
        )
    )
);

/// Match a string literal.
pub fn string(input: &[u8]) -> ExprRes {
    let parse_result = string_literal(input);

    return fmap_node(parse_result, |x: &[u8]| Expr::String(from_utf8(x).unwrap().to_string()));
}

pub mod expr_parsers {
    use super::*;

    /// Match a binary expression whose operator is a symbol.
    pub fn binary_op_symbol<'a>(input: &'a [u8], symbol: &str, operator: BinaryOperator, next_expr: fn(&[u8]) -> ExprRes) -> ExprRes<'a> {
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
    // pub fn binary_keyword_list<'a>(input: &'a [u8], symbols: &Vec<&str>, operators: &HashMap<&[u8], BinaryOperator>, next_expr: fn(&[u8]) -> ExprRes) -> ExprRes<'a> {
    //     let parse_result = tuple!(input,
    //         w_followed!(next_expr),
    //         opt!(tuple!(
    //             w_followed!(call!(match_any, symbols)),
    //             call!(binary_op_list, symbols, operators, next_expr)
    //         ))
    //     );

    //     let node = fmap_iresult(parse_result, |x| match_binary_exprs(operators, x));
    //     return node;
    // }

    /// Match a list of binary operations
    pub fn binary_op_list<'a>(input: &'a [u8], symbols: &Vec<&str>, operators: &HashMap<&[u8], BinaryOperator>, next_expr: fn(&[u8]) -> ExprRes) -> ExprRes<'a> {
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
    pub fn boolean_op_expr(input: &[u8]) -> ExprRes {
        let parse_result = tuple!(input,
            w_followed!(bit_boolean_op_expr),
            optc!(tuple!(
                w_followed!(alt_complete!(AND | OR | XOR)),
                boolean_op_expr
            ))
        );
        // println!("second match: {:?}", optc!("and false".as_bytes(), w_followed!(alt_complete!(AND | OR | XOR))));
        return fmap_iresult(parse_result, flatten_binary);
    }

    /// An exponentiation.
    pub fn power_expr(input: &[u8]) -> ExprRes {
        let parse_result = tuple!(input,
            w_followed!(atomic_expr),
            optc!(tuple!(
                EXP,
                power_expr
            ))
        );
        return fmap_iresult(parse_result, flatten_binary);
    }

    /// Flatten a possible binary expression into a single expression.
    pub fn flatten_binary(result: (Node<Expr>, Option<(&[u8], Node<Expr>)>)) -> Node<Expr> {
        return match result.1 {
            Some(x) => {
                let op = BinaryOperator::from(x.0);
                Node::from(Expr::BinaryExpr {operator: op, left: Box::new(result.0), right: Box::new(x.1)})
            },
            None => result.0
        };
    }

}

pub mod type_parser {
    use super::*;
    /// Parse a type.
    pub fn any_type(input: &[u8]) -> TypeRes {
        return alt_complete!(input, product_type | sum_type);
    }

    pub fn product_type(input: &[u8]) -> TypeRes {
        let result = delimited!(input,
            open_paren,
            separated_list!(
                comma,
                alt_complete!(product_type | sum_type)
            ),
            close_paren
        );

        return fmap_iresult(result, |x| typing::Type::Product(x))
    }

    pub fn sum_type(input: &[u8]) -> TypeRes {
        let result = tuple!(input, 
            parameterized_type,
            many0c!(
                preceded!(
                    VBAR,
                    parameterized_type
                )
            )
        );

        return fmap_iresult(result, |mut x| match x.1.len() {
            0 => x.0,
            _ => {
                x.1.insert(0, x.0);
                typing::Type::Sum(x.1)
            }
        });
    }

    pub fn parameterized_type(input: &[u8]) -> TypeRes {
        let result = tuple!(input, 
            IDENTIFIER,
            opt!(complete!(delimited!(
                LANGLE,
                separated_nonempty_list!(
                    comma,
                    any_type
                ),
                RANGLE
            )))
        );
        return fmap_iresult(result, |x| match x.1 {
            Some(y) => typing::Type::Parameterized(x.0, y),
            None => typing::Type::from(x.0)
        });
    }

}

#[cfg(test)]
mod tests {

    use super::*;
    use self::expr_parsers::*;
    use rand;
    use std::fmt::Debug;
    use parser_utils::iresult_helpers::*;

    #[test]
    fn test_module() {
       let module_str = "fn a():\n return 0\n\nfn b():\n return 1";
       check_match(module_str, module, Node::from(Module{
           declarations: vec!(
               Box::new(output(function_declaration("fn a():\n return 0".as_bytes(), 0))),
               Box::new(output(function_declaration("fn b():\n return 1".as_bytes(), 0)))
           )
       }))
    }

        #[test]
    fn test_module_with_import() {
       let module_str = "import foo\nfn a():\n return 0\n\nfn b():\n return 1";
       check_match(module_str, module, Node::from(Module{
           declarations: vec!(
               Box::new(output(import("import foo".as_bytes()))),
               Box::new(output(function_declaration("fn a():\n return 0".as_bytes(), 0))),
               Box::new(output(function_declaration("fn b():\n return 1".as_bytes(), 0)))
           )
       }))
    }

    #[test]
    fn test_block() {
        let exp_block = Block {
            statements: vec![
                Box::new(output(assignment_stmt("x=0\n".as_bytes()))),
                Box::new(output(assignment_stmt("y=true".as_bytes())))
            ]
        };

        check_match(" x=0\n y=true\n\n  \n", |x| block(x, 1), Node::from(exp_block));
    }

    #[test]
    fn test_reserved_words() {
        for keyword in reserved_list() {
            let result = IDENTIFIER(keyword.as_bytes());
            unwrap_and_check_error(result, ErrorKind::Not);
        }
    }

    #[test]
    fn test_dotted_identifier() {
        let expected = DottedIdentifier{attributes: vec!(Identifier::from("asdf"), Identifier::from("dfgr_1"), Identifier::from("_asdf"))};
        check_match("asdf.dfgr_1   .   _asdf", dotted_identifier, expected);
    }

    #[test]
    fn test_post_ident() {
        let expected_args = vec!("a", "b", "c").iter().map(|x| Node::from(*x)).collect();
        check_match("( a ,  b , c ) ", trailer, PostIdent::Call{args: expected_args, kwargs: vec![]});
        check_match("( a   ,  b  =    true)", trailer, PostIdent::Call {
            args: vec!(Node::from("a")),
            kwargs: vec!((Identifier::from("b"), Node::from(true)))
        });

        check_match("( a   = true,  b = true ) ", trailer, PostIdent::Call {
            args: vec![],
            kwargs: vec![(Identifier::from("a"), Node::from(true)), (Identifier::from("b"), Node::from(true))]
        });



        simple_check_failed("(a | b=false)", trailer);
        simple_check_failed("(a   b=false)", trailer);
        simple_check_failed("(a,, b=false)", trailer);
        simple_check_failed("(a,, b)", trailer);
        simple_check_failed("(a, b =  true, c)", trailer);


       check_match(".asdf_   .   asdf", trailer, PostIdent::Access{attributes: vec!(Identifier::from("asdf_"), Identifier::from("asdf"))});

       check_match("[a:b:c, :, d]", trailer, PostIdent::Index {
           slices: vec!(
               (Some(Node::from("a")), Some(Node::from("b")), Some(Node::from("c"))),
               (None, None, None),
               (Some(Node::from("d")), None, None)
           )
       })
    }

    #[test]
    fn test_identifiers() {
        // check_match("(", open_paren, "(".as_bytes());
        // check_match("a", identifier, Identifier::from("a"));
        panic!()
    }

    #[cfg(test)]
    mod statements {
        use super::*;

        #[test]
        fn test_let() {
            check_data("let x: f32 = 3.0", |x| statement(x, 0), Stmt::LetStmt {
                typed_name: TypedIdent {
                    name: Identifier::from("x"),
                    type_annotation: Some(typing::Type::f32)
                },
                expression: Node::from(Expr::Float("3.0".to_string()))
            });
        }

        #[test]
        fn test_func_dec_parts() {
            // Args
            let actual = output(args_dec_list("a: i32)".as_bytes()));
            assert_eq!(vec!((Identifier::from("a"), Type::i32)), actual);

            // Vararg
            let expected = Some(Identifier::from("args"));
            let actual = output(vararg(", *args".as_bytes()));
            assert_eq!(expected, actual);

            // Kwargs.
            let expected = vec!(
                   (Identifier::from("c"), Type::i32, output(expression("5".as_bytes()))),
                   (Identifier::from("d"), Type::i32, output(expression("7".as_bytes())))
            );
            let actual = output(keyword_args(", c: i32=5, d: i32=7".as_bytes()));
            assert_eq!(expected, actual);
        }
       
        #[test]
        fn test_func_dec() {
            check_data("fn wvars(a: i32, b: i32, *args, c: i32=5, d: i32 = 7, **kwargs):\n let val = 5", |x| function_declaration(x, 0), Stmt::FunctionDecStmt {
                name: Identifier::from("wvars"),
                args: vec!((Identifier::from("a"), typing::Type::i32), (Identifier::from("b"), typing::Type::i32)),
                vararg: Some(Identifier::from("args")),
                kwargs: vec!(
                    (Identifier::from("c"), typing::Type::i32, output(expression("5".as_bytes()))),
                    (Identifier::from("d"), typing::Type::i32, output(expression("7".as_bytes())))
                ),
                varkwarg: Some(Identifier::from("kwargs")),
                block: output(block("let val =  5\n".as_bytes(), 0)),
                return_type: typing::Type::empty
            });

            check_data("fn wkwargs(a: i32, c: i32=5):\n let val = 5", |x| function_declaration(x, 0), Stmt::FunctionDecStmt {
                name: Identifier::from("wkwargs"),
                args: vec![(Identifier::from("a"), typing::Type::i32)],
                vararg: None,
                kwargs: vec!(
                    (Identifier::from("c"), typing::Type::i32, output(expression("5".as_bytes()))),
                ),
                varkwarg: None,
                block: output(block("let val=5\n".as_bytes(), 0)),
                return_type: typing::Type::empty
            });

            check_data("fn a(b: i32) -> i32:\n let x = 5 + 6\n return x\n", |x| function_declaration(x, 0), Stmt::FunctionDecStmt {
                name: Identifier::from("a"),
                args: vec!((Identifier::from("b"), typing::Type::i32)),
                vararg: None,
                kwargs: vec!(),
                varkwarg: None,
                block: output(block("let x = 5 + 6\nreturn x".as_bytes(), 0)),
                return_type: typing::Type::i32
            });
        }

        #[test]
        fn test_try_except() {
            let blk = output(block("x=0".as_bytes(), 0));
            check_data("try    :     \n\n\n x = 0\n\n     \n\nexcept:\n x =      0     \nelse:\n x=0\nfinally:\n x=0     \n\n\n   \n\n", |x| try_except(x, 0), Stmt::TryExceptStmt {
                block: blk.clone(),
                exceptions: vec!(blk.clone()),
                else_block: Some(blk.clone()),
                final_block: Some(blk.clone())
            });
        }

        #[test]
        fn test_assignment() {
            check_data("foo = true", assignment_stmt, Stmt::AssignmentStmt {
                name: Identifier::from("foo"),
                operator: Assignment::Normal,
                expression: Node::from(true)
            });

            check_data("x = 0\n", assignment_stmt, Stmt::AssignmentStmt {
                name: Identifier::from("x"),
                operator: Assignment::Normal,
                expression: Node::from(0)
            });

            let all_ops = vec!["&=", "|=", "^=", "+=", "-=", "*=", "/=", "%=", ">>=", "<<=", "**=", "="];
            for op in all_ops {
                let input = format!("x {} y", op);
                check_data(input.as_str(), assignment_stmt, Stmt::AssignmentStmt {
                    name: Identifier::from("x"),
                    operator: Assignment::from(op),
                    expression: Node::from("y"),
                });
            }

        }

        #[test]
        fn test_if_stmt() {
            let good_input = "if (a and b):\n x = true";

            let good_output = Stmt::IfStmt{
                condition: output(expression("a and b".as_bytes())),
                block: Node::from(Block{statements: vec!(Box::new(output(assignment_stmt("x = true".as_bytes()))))}),
                elifs: vec!(),
                else_block: None
            };

            check_data(good_input, |x| statement(x, 0), good_output);

            check_failed("ifa and b:\n x = true", |x| statement(x, 0), ErrorKind::Alt);

            check_data("if    true   :     \n\n\n  x = true\n elif    false   :   \n\n\n  y = true\n else     :  \n  z = true", |x| if_stmt(x, 1), Stmt::IfStmt {
                
                condition: Node::from(true),
                block: output(block("x = true".as_bytes(), 0)),
                elifs: vec!((Node::from(false), output(block("y = true".as_bytes(), 0)))),
                else_block: Some(output(block("z = true".as_bytes(), 0)))
            });
        }

        #[test]
        fn test_while_stmt() {
            check_data("while true:\n x=true", |x| statement(x, 0), Stmt::WhileStmt {
                condition: Node::from(true),
                block: Node::from(Block{statements: vec!(Box::new(output(assignment_stmt("x=true".as_bytes()))))})
            });
        }

        #[test]
        fn test_simple_statements() {
            check_data("pass", |x| statement(x, 0), Stmt::PassStmt);
            check_data("continue", |x| statement(x, 0), Stmt::ContinueStmt);
            check_data("break", |x| statement(x, 0), Stmt::BreakStmt);
        }

        #[test]
        fn test_import() {
            check_data("import foo.bar.baz", |x| statement(x, 0), Stmt::ImportStmt(DottedIdentifier{
                attributes: vec!(Identifier::from("foo"), Identifier::from("bar"), Identifier::from("baz"))
            }));
        }

        #[test]
        fn test_returns() {
            check_data("return true", |x| statement(x, 0), Stmt::ReturnStmt (Node::from(true)));

            check_data("yield true", |x| statement(x, 0), Stmt::YieldStmt (Node::from(true)));
        }

        #[test]
        fn test_for_in() {
            check_data("for x in y:\n a=true", |x| statement(x, 0), Stmt::ForInStmt {
                iter_vars: Identifier::from("x"),
                iterator: Node::from("y"),
                block: output(block("a=true".as_bytes(), 0))
            });
        }
    }

    #[cfg(test)]
    mod expressions {
        use super::*;

        #[test]
        fn test_parenthetical_expressions() {
            let expected = Expr::BinaryExpr {
                operator: BinaryOperator::Or,
                left: Box::new(Node::from(Expr::BinaryExpr {
                    operator:BinaryOperator::And,
                    left: Box::new(Node::from(true)),
                    right: Box::new(Node::from(false))
                })),
                right: Box::new(Node::from(true))
            };
            check_data("(true and false) or true", expression, expected);

            check_data("(true and false)", expression, Expr::BinaryExpr {
                
                operator:BinaryOperator::And,
                left: Box::new(Node::from(true)),
                right: Box::new(Node::from(false))
            });
        }

        #[test]
        fn parse_function_call() {
            let a = output(boolean_op_expr("true and false".as_bytes()));
            let no_args = Node::from(Expr::FunctionCall{
                function: Box::new(Node::from("func")),
                args: vec!(),
                kwargs: vec!()
            });
            iresult_helpers::check_match("func()", expr_with_trailer, no_args);
            let b = output(expression("func()".as_bytes()));
            let expected = Expr::FunctionCall{
                function: Box::new(Node::from(Expr::IdentifierExpr(Identifier{name: "ident".to_string()}))), 
                args: vec!(a, b), 
                kwargs: vec!()
            };
            iresult_helpers::check_data("ident(true and false, func())", expression, expected);

            iresult_helpers::check_data("func(a, b, c=true, d=true)", expression, Expr::FunctionCall {
                function: Box::new(Node::from("func")),
                args: c![Node::from(x), for x in vec!("a", "b")],
                kwargs: vec!(
                    (Identifier::from("c"), Node::from(true)),
                    (Identifier::from("d"), Node::from(true))
                )
            });
        }

        #[test]
        fn parse_binary_expr() {

            check_data("1 ** 2", expression, Expr::BinaryExpr{
                operator: BinaryOperator::Exponent,
                left: Box::new(Node::from(1)),
                right: Box::new(Node::from(2))
            });

            check_data("true and false", boolean_op_expr, Expr::BinaryExpr{
                operator: BinaryOperator::And,
                left: Box::new(Node::from(true)),
                right: Box::new(Node::from(false))
            });

            check_data("true or false", expression, Expr::BinaryExpr{
                operator: BinaryOperator::Or,
                left: Box::new(Node::from(true)),
                right: Box::new(Node::from(false))
            });

            check_data("true and false or true", expression, Expr::BinaryExpr{
                
                operator: BinaryOperator::And,
                left: Box::new(Node::from(true)),
                right:Box::new(Node::from(Expr::BinaryExpr{
                    
                    operator: BinaryOperator::Or,
                    left: Box::new(Node::from(false)),
                    right: Box::new(Node::from(true))
                }))
            });

            let all_ops = vec!["and", "or", "xor", "&", "|", "^", "+", "-", "*", "/", "%", ">>", "<<", "**"];
            for op in all_ops {
                let input = format!("x {} y", op);
                check_data(input.as_str(), expression, Expr::BinaryExpr {
                    
                    operator: BinaryOperator::from(op),
                    left: Box::new(Node::from("x")),
                    right: Box::new(Node::from("y")),
                });
            }
        }

        #[test]
        fn test_unary_expr() {
            let ops = vec!["not", "+", "-", "~"];
            for op in ops {
                let input = format!("{} y", op);
                check_data(input.as_str(), expression, Expr::UnaryExpr {
                    
                    operator: UnaryOperator::from(op),
                    operand: Box::new(Node::from("y")),
                });
            }
            check_data("~+y", expression, Expr::UnaryExpr {
                operator: UnaryOperator::BitNot,
                operand: Box::new(output(expression("+y".as_bytes()))),
            });
            check_data("not true", expression, Expr::UnaryExpr {operator: UnaryOperator::Not, operand: Box::new(Node::from(true))});
        }

        #[test]
        fn parse_identifier_expr() {
            let expected: Node<Expr> = Node::from("words");
            check_match("words", expr_with_trailer, expected);

            let expected = Node::from("abc_123");
            check_match("abc_123", expr_with_trailer, expected);

            check_failed("123", expr_with_trailer, ErrorKind::Alt);
            check_failed("(", expr_with_trailer, ErrorKind::Alt);
        }

        #[test]
        fn test_comparison_expr() {
            let comp_strs = vec![">", "<", ">=", "<=", "==", "!="];
            let comp_ops = vec![ComparisonOperator::Greater, ComparisonOperator::Less, ComparisonOperator::GreaterEqual,
                                ComparisonOperator::LessEqual, ComparisonOperator::Equal, ComparisonOperator::Unequal];
            for (comp_str, comp_op) in comp_strs.iter().zip(comp_ops.iter()) {
                let as_str = format!("true {} false", comp_str);
                let expr = expression(as_str.as_bytes());
                let expected = Node::from(Expr::ComparisonExpr{
                    
                    left: Box::new(Node::from(true)),
                    right: Box::new(Node::from(false)),
                    operator: *comp_op
                });

                assert_eq!(expr, Ok(("".as_bytes(), expected)));
            }
        }

        #[test]
        fn test_repeated_func_calls() {
            let expected = Expr::FunctionCall{
                function: Box::new(Node::from(Expr::FunctionCall{
                    function: Box::new(Node::from("func")), 
                    args: vec!(Node::from("a")), 
                    kwargs: vec!()
                })),
                args: vec!(Node::from("b"), Node::from("c")),
                kwargs: vec!()
            };
            check_data("func(a)(b, c)", expression, expected);

            check_data("(a and b)(true)", expression, Expr::FunctionCall {
                function: Box::new(output(boolean_op_expr("a and b".as_bytes()))),
                args: vec!(Node::from(true)),
                kwargs: vec!()
            });
        }

        #[test]
        fn test_comprehensions() {
            check_match("{x for x in y}", expression, Node::from(Expr::SetComprehension {
                
                values: Box::new(Node::from("x")),
                iterators: vec!(ComprehensionIter {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(Node::from("y")),
                    if_clauses: vec!()
                })
            }));

            check_match("{x:z for x in y}", expression, Node::from(Expr::MapComprehension {
                
                keys: Box::new(Node::from("x")),
                values: Box::new(Node::from("z")),
                iterators: vec!(ComprehensionIter {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(Node::from("y")),
                    if_clauses: vec!()
                })
            }));

            check_match("[x for x in y]", expression, Node::from(Expr::VecComprehension {
                
                values: Box::new(Node::from("x")),
                iterators: vec!(ComprehensionIter {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(Node::from("y")),
                    if_clauses: vec!()
                })
            }));

            check_match("(x for x in y)", expression, Node::from(Expr::GenComprehension {
                values: Box::new(Node::from("x")),
                iterators: vec!(ComprehensionIter {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(Node::from("y")),
                    if_clauses: vec!()
                })
            }));

            check_match("[x for x in y for x in y]", expression, Node::from(Expr::VecComprehension {
                values: Box::new(Node::from("x")),
                iterators: vec!(ComprehensionIter {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(Node::from("y")),
                    if_clauses: vec!()
                }, ComprehensionIter {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(Node::from("y")),
                    if_clauses: vec!()
                })
            }));

            check_match("for a, b in c if true if false", comprehension_for, ComprehensionIter{
                iter_vars: c![Identifier::from(x), for x in vec!("a", "b")],
                iterator: Box::new(Node::from("c")),
                if_clauses: c![Node::from(x), for x in vec!(true, false)]
            });
        }

        #[test]
        fn test_match() {
            check_match("match x:\n5 => 5", expression, Node::from(Expr::MatchExpr {
                value: Box::new(Node::from("x")),
                cases: vec![(output(expression("5".as_bytes())), output(expression("5".as_bytes())))]
            }));
        }

        #[test]
        fn parse_literals() {
            let int = rand::random::<i64>().abs();
            check_match(&int.to_string(), expression, Node::from(int));
            let rand_float = rand::random::<f64>().abs();
            check_match(&rand_float.to_string(), float, Node::from(rand_float));
            let expr = Expr::String("\"asdf\\\"\\\rasdf\"".to_string());
            check_match("\"asdf\\\"\\\rasdf\"", expression, Node::from(expr));

            check_match("{x : y}", expression, Node::from(Expr::MapLiteral(vec!((Identifier::from("x"), Node::from("y"))))));
            check_match("[true, false]", expression, Node::from(Expr::VecLiteral(vec!(Node::from(true), Node::from(false)))));
            check_match("{true, false}", expression, Node::from(Expr::SetLiteral(vec!(Node::from(true), Node::from(false)))));
            check_match("(true, false)", expression, Node::from(Expr::TupleLiteral(vec!(Node::from(true), Node::from(false)))));
        }

        #[cfg(test)]
        mod specific_exprs {
            use super::*;

            #[test]
            fn parse_atomic() {
                // check_data_and_leftover("1 ** 2", atomic_expr, Expr::from(1), "** 2");
                check_data("false", bool_expr, Expr::from(false));
                // check_data("false", atomic_expr, Expr::from(false));
            }

            #[test]
            fn parse_spec_literals() {
                check_match("123", int, Node::from(123));
            }

            #[test]
            fn parse_boolean_op() {
                check_match("true and false", boolean_op_expr, Node::from(Expr::BinaryExpr{
                    operator: BinaryOperator::And,
                    left: Box::new(Node::from(true)),
                    right: Box::new(Node::from(false))
                }));
            }
        }
    }

    #[cfg(test)]
    mod types {
        use super::*;
        use self::type_parser::*; 
        #[test]
        fn test_simple_types() {
            check_match("i32", any_type, typing::Type::i32);
            check_match("i64", any_type, typing::Type::i64);
            check_match("f32", any_type, typing::Type::f32);
            check_match("f64", any_type, typing::Type::f64);
            check_match("ui32", any_type, typing::Type::ui32);
            check_match("ui64", any_type, typing::Type::ui64);
            check_match("boolean", any_type, typing::Type::boolean);
            check_match("string", any_type, typing::Type::string);

            // TODO: Random string test
        }

        #[test]
        fn test_parameterized_types() {
            check_match("Test<i32>", any_type, typing::Type::Parameterized(
                Identifier::from("Test"), 
                vec!(typing::Type::i32)
            ));
        }

        #[test]
        fn test_sum_types() {
            check_match("i32 | i64", any_type, typing::Type::Sum(vec!(
                typing::Type::i32, typing::Type::i64
            )));
        }

        #[test]
        fn test_product_types() {
            check_match("(i32, i64)", any_type, typing::Type::Product(vec!(
                typing::Type::i32, typing::Type::i64
            )));
        }
    }

}
