use std::str::from_utf8;
use std::collections::HashSet;

extern crate cute;
extern crate nom;
use self::nom::*;

use expression::*;
use position_tracker::PosStr;
use parser_utils::*;
use parser_utils::iresult_helpers::*;
use parser_utils::tokens::*;

use typing::Type;
use general_utils::{
    get_next_id,
    get_next_var
};

// use self::expr_parsers::expression;
use self::stmt_parsers::struct_declaration_stmt;

type StmtNode = Node<Stmt>;
type ExprNode = Node<Expr>;
type IO<'a> = IResult<PosStr<'a>, PosStr<'a>>;
type Res<'a, T> = IResult<PosStr<'a>, T>;
type StmtSeq = Vec<Box<Node<Stmt>>>;
type Update = Option<StmtSeq>;
type StmtRes<'a> = IResult<PosStr<'a>, StmtNode>;
type ExprRes<'a> = IResult<PosStr<'a>, ExprNode>;
type TypeRes<'a> = IResult<PosStr<'a>, Type>;


// #[derive(Debug, Clone, PartialEq, Eq)]
/// The available context at every step of the parser.
// pub struct ParserContext {
//     pub imported_names: HashSet<Identifier>,
//     pub indent: usize
// }

pub trait Parseable {
    fn parse<'a>(input: PosStr<'a>) -> Self;
}

impl Parseable for Node<Module> {
    fn parse<'a>(input: PosStr<'a>) -> Node<Module> {
        return  output(module(PosStr::from(input)));
    }
}

impl Parseable for Node<Block> {
    fn parse<'a>(input: PosStr<'a>) -> Node<Block> {
        let e = ParserContext::empty();
        return output(e.block(PosStr::from(input), 0));
    }
}

impl Parseable for Node<Stmt> {
    fn parse<'a>(input: PosStr<'a>) -> Node<Stmt> {
        let e = ParserContext::empty();
        return output(e.statement(PosStr::from(input), 0));
    }
}

impl Parseable for Node<Expr> {
    fn parse<'a>(input: PosStr<'a>) -> Node<Expr> {
        let e = ParserContext::empty();
        return output(e.expression(PosStr::from(input)));
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserContext {
    imported: HashSet<Identifier>
}

impl ParserContext {
    pub fn block<'a>(&self, input: PosStr<'a>, indent: usize) -> IResult<PosStr<'a>, Node<Block>> {
        let first_indent_parse = preceded!(input, opt!(between_statement), many0c!(tag!(" ")));
        let full_indent = match first_indent_parse {
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
                            indented!(m!(self.statement, expected_indent), expected_indent),
                            between_statement
                        )
                    )
                )
            );
            statements
        };
        return fmap_node(parse_result, |x| Block{statements: x.into_iter().map(Box::new).collect()});
    }

    pub fn empty() -> ParserContext {
        return ParserContext{
            imported: HashSet::new()
        };
    }
}

/// Match a module.
pub fn module<'a>(input: PosStr<'a>) -> IResult<PosStr<'a>, Node<Module>>{
    let context = ParserContext::empty();
    let parse_result = preceded!(input,
        opt!(between_statement),
        tuple!(
            many0c!(
                terminated!(import, between_statement)
            ),
            many1c!(
                terminated!(
                    alt_complete!(m!(context.function_declaration_stmt, 0) | call!(struct_declaration_stmt, 0)),
                    
                    between_statement
                )
            ),
            alt_complete!(eof!() | EMPTY)
        )
    );

    return fmap_node(parse_result, |x| Module{
        imports: x.0.into_iter().map(Box::new).collect(), 
        declarations: x.1.into_iter().map(Box::new).collect()
    });
}

/// Match an import statement.
fn import<'a>(input: PosStr<'a>) -> Res<'a, Import> {
    let parse_result = preceded!(input, IMPORT, 
        pair!(
            separated_nonempty_list_complete!(DOT, IDENTIFIER), 
            optc!(preceded!(AS, IDENTIFIER))
        )
    );
    return fmap_iresult(parse_result, |(x, y)| Import{id: get_next_id(), path: x, alias: y, values: vec!()});
}

/// All statement parsers.
pub mod stmt_parsers {
    use super::*;
    use self::type_parser::any_type;

    impl ParserContext {
        /// Match any statement.
        pub fn statement<'a>(&self, input: PosStr<'a>, indent: usize) -> StmtRes<'a> {
            let node = alt_complete!(input,
                m!(self.let_stmt) |
                m!(self.assignment_stmt) |
                m!(self.while_stmt, indent) |
                m!(self.for_in, indent) |
                m!(self.if_stmt, indent) |
                m!(self.function_declaration_stmt, indent) |
                m!(self.return_stmt) |
                m!(self.yield_stmt) |
                break_stmt |
                pass_stmt |
                continue_stmt 
            );

            return fmap_iresult(node, |x| x);
        }

        /// Match a let statement.
        fn let_stmt<'a>(&self, input: PosStr<'a>) -> StmtRes<'a> {
            let parse_result = tuple!(input,
                preceded!(
                    LET,
                    IDENTIFIER
                ),
                optc!(preceded!(COLON, type_parser::any_type)),
                preceded!(
                    EQUALS,
                    m!(self.expression)
                )
            );

            return fmap_node(parse_result, |x| Stmt::LetStmt {name: x.0, type_annotation: x.1, expression: x.2});
        }

        /// Match an assignment statement.
        fn assignment_stmt<'a>(&self, input: PosStr<'a>) -> StmtRes<'a> {

            /// Match an assignment operator.
            fn assignments<'a>(input: PosStr<'a>) -> IO<'a> {
                return alt_complete!(input, 
                    EQUALS | 
                    ADDASN |
                    SUBASN |
                    MULASN |
                    DIVASN |
                    MODASN |
                    EXPASN |
                    RSHASN |
                    LSHASN |
                    BORASN |
                    BANDASN |
                    BXORASN 
                );
            }

            let parse_result = terminated!(input,
                tuple!(
                    IDENTIFIER,
                    assignments,
                    m!(self.expression)
                ),
                alt_complete!(NEWLINE | eof!() | EMPTY)
            );

            return fmap_node(parse_result, |x| Stmt::AssignmentStmt{
                name: x.0.clone(), expression: match x.1.slice {
                    b"=" => x.2,
                    _ => {
                        let subop = &x.1.slice[0..x.1.slice.len()-1];
                        Node::from(Expr::BinaryExpr{
                            operator: BinaryOperator::from(subop),
                            left: Box::new(Node::from(Expr::IdentifierExpr(x.0))),
                            right: Box::new(x.2)
                        })
                    }
                }
            });
        }

        /// Parse a function declaration.
        pub fn function_declaration_stmt<'a>(&self, input: PosStr<'a>, indent: usize) -> StmtRes<'a> {
            let arg_parser = |i: PosStr<'a>| tuple!(i,
                IDENTIFIER,
                preceded!(OPEN_PAREN, fn_dec_args),
                terminated!(m!(self.keyword_args), CLOSE_PAREN),
                optc!(preceded!(
                    TARROW,
                    type_parser::any_type
                ))
            );

            let parse_result = line_and_block2!(input, self, preceded!(FN, arg_parser), indent);

            return fmap_node(parse_result, |((name, args, keyword_args, return_type), body)| Stmt::FunctionDecStmt{
                name: name,
                args: args,
                kwargs: keyword_args,
                block: body,
                return_type: match return_type {
                    Some(x) => x,
                    None => Type::empty
                }
            });
        }

        /// Match an if statement.
        fn if_stmt<'a>(&self, input: PosStr<'a>, indent: usize) -> StmtRes<'a> {
            let parse_result = tuple!(input,
                line_and_block2!(self, preceded!(IF, m!(self.expression)), indent),
                many0c!(indented!(line_and_block2!(self, preceded!(ELIF, m!(self.expression)), indent), indent)),
                opt!(complete!(indented!(keyword_and_block2!(self, ELSE, indent), indent)))
            );

            return fmap_node(parse_result, |x|Stmt::IfStmt{condition: (x.0).0, block: (x.0).1, elifs: x.1, else_block: x.2});
        }

        /// Parse a while loop.
        fn while_stmt<'a>(&self, input: PosStr<'a>, indent: usize) -> StmtRes<'a> {
            let parse_result = line_and_block2!(input, self, preceded!(WHILE, m!(self.expression)), indent);
            return fmap_node(parse_result, |x| Stmt::WhileStmt {condition: x.0, block: x.1});
        }

        /// Parse a for in loop.
        fn for_in<'a>(&self, input: PosStr<'a>, indent: usize) -> StmtRes<'a> {
            let parse_result = line_and_block2!(input, self, tuple!(
                preceded!(
                    FOR,
                    IDENTIFIER
                ),
                preceded!(
                    IN,
                    m!(self.expression)
                )
            ), indent);

            return fmap_node(parse_result, |x| Stmt::ForInStmt {iter_vars: (x.0).0, iterator: (x.0).1, block: x.1});
        }

        /// Match a return statement.
        fn return_stmt<'a>(&self, input: PosStr<'a>) -> StmtRes<'a> {
            let parse_result = preceded!(input, RETURN, m!(self.expression));
            return fmap_node(parse_result,|x| Stmt::ReturnStmt (x));
        }

        /// Match a yield statement.
        fn yield_stmt<'a>(&self, input: PosStr<'a>) -> StmtRes<'a> {
            let parse_result = preceded!(input, YIELD, m!(self.expression));
            return fmap_node(parse_result, |x| Stmt::YieldStmt(x))
        }

        /// Match all keyword arguments in a function declaration.
        fn keyword_args<'a>(&self, input: PosStr<'a>) -> Res<'a, Vec<(Identifier, Type, Node<Expr>)>> {
            let parse_result = optc!(input, preceded!(
                COMMA,
                separated_list_complete!(
                    COMMA,
                    tuple!(
                        IDENTIFIER,
                        preceded!(COLON, type_parser::any_type),
                        preceded!(EQUALS, m!(self.expression))
                    )
                )
            ));

            return fmap_iresult(parse_result, |x| match x {
                Some(y) => y,
                None => vec!()
            });
        }
    }

    /// Match the standard arguments in a function declaration.
    fn fn_dec_args<'a>(input: PosStr<'a>) -> Res<'a, Vec<(Identifier, Type)>> {
        let result = separated_list_complete!(input,
            COMMA,
            tuple!(
                IDENTIFIER,
                preceded!(
                    COLON,
                    terminated!(any_type, not!(complete!(EQUALS)))
                )
            )
        );
        return result;
    }

    pub fn struct_declaration_stmt<'a>(input: PosStr<'a>, indent: usize) -> StmtRes {
        let header = tuple!(input,
            delimited!(
                STRUCT,
                IDENTIFIER,
                terminated!(
                    COLON,
                    between_statement
                )
            ),
            many0c!(inline_whitespace_char)
        );

        // Horrifying? Yes.
        let result = match header {
            Ok((i, o)) => {
                let new_indent = o.1.len();
                if new_indent > indent {
                    let rest = tuple!(i,
                         terminated!(
                            tuple!(
                                IDENTIFIER,
                                preceded!(COLON, any_type)
                            ),
                            between_statement
                        ), many1c!(
                            terminated!(
                                indented!(tuple!(
                                    IDENTIFIER,
                                    preceded!(COLON, any_type)
                                ), new_indent),
                                between_statement
                            )
                        )
                    );
                    match rest {
                        Ok((i, mut r)) => {
                            r.1.insert(0, r.0);
                            Ok((i, (o.0, r.1)))
                        },
                        Err(x) => Err(x)
                    }
                } else {
                    // TODO: Return an indentation error.
                    panic!()
                }
            },
            Err(x) => Err(x)
        };

        return fmap_node(result, |x| Stmt::StructDec{name: x.0, fields: x.1});
    }

    /// Match a break statement.
    pub fn break_stmt<'a>(input: PosStr<'a>) -> StmtRes {
        let parse_result = terminated!(input,
            BREAK,
            peek!(alt_complete!(
                eof!() | NEWLINE | EMPTY
            ))
        );

        return fmap_node(parse_result, |_| Stmt::BreakStmt);
    }

    /// Match a pass statement.
    pub fn pass_stmt<'a>(input: PosStr<'a>) -> StmtRes {
        let parse_result = terminated!(input,
            PASS,
            peek!(alt_complete!(
                eof!() | NEWLINE | EMPTY
            ))
        );

        return fmap_node(parse_result, |_| Stmt::PassStmt);
    }

    /// Match a continue statement.
    pub fn continue_stmt<'a>(input: PosStr<'a>) -> StmtRes {
        let parse_result = terminated!(input,
            CONTINUE,
            peek!(alt_complete!(
                eof!() | NEWLINE | EMPTY
            ))
        );

        return fmap_node(parse_result, |_| Stmt::ContinueStmt);
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn parse_let_stmt() {
            let e = ParserContext::empty();
            check_data("let x = 3.0", |x| e.statement(x, 0), Stmt::LetStmt {
                name: Identifier::from("x"),
                type_annotation: None,
                expression: Node::from(Expr::Float("3.0".to_string()))
            });

            check_data("let x: f32 = 3.0", |x| e.statement(x, 0), Stmt::LetStmt {
                name: Identifier::from("x"),
                type_annotation: Some(Type::f32),
                expression: Node::from(Expr::Float("3.0".to_string()))
            });
        }

        #[test]
        fn parse_assignment_stmt() {
            let e = ParserContext::empty();
            check_data("foo = true", |x| e.assignment_stmt(x), Stmt::AssignmentStmt {
                name: Identifier::from("foo"),
                expression: Node::from(true)
            });

            check_data("x = 0\n", |x| e.assignment_stmt(x), Stmt::AssignmentStmt {
                name: Identifier::from("x"),
                expression: Node::from(0)
            });

            let all_ops = vec!["&=", "|=", "^=", "+=", "-=", "*=", "/=", "%=", ">>=", "<<=", "**="];
            for op in all_ops {
                let input = format!("x {} y", op);
                check_data(input.as_str(), |x| e.assignment_stmt(x), Stmt::AssignmentStmt {
                    name: Identifier::from("x"),
                    expression: Node::from(Expr::BinaryExpr{
                        operator: BinaryOperator::from(&op[0..op.len()-1]),
                        left: Box::new(Node::from(Expr::from("x"))),
                        right: Box::new(Node::from(Expr::from("y")))
                    })
                });
            }
        }

        #[test]
        fn parse_func_dec_parts() {
            // Args
            let actual = output(fn_dec_args(PosStr::from("a: i32)")));
            assert_eq!(vec!((Identifier::from("a"), Type::i32)), actual);
            check_match("a: i32, b: i64", fn_dec_args, vec!(
                (Identifier::from("a"), Type::i32),
                (Identifier::from("b"), Type::i64)
            ));

            // Kwargs.
            let expected = vec!(
                   (Identifier::from("c"), Type::i32, Node::from(5)),
                   (Identifier::from("d"), Type::i32, Node::from(7))
            );

            let e = ParserContext::empty();
            check_match(", c: i32=5, d: i32=7", |x| e.keyword_args(x), expected);
        }
       
        #[test]
        fn parse_func_dec() {
            let e = ParserContext::empty();
            check_data("fn wvars(a: i32, b: i32, c: i32=5, d: i32 = 7):\n let val = 5", |x| e.statement(x, 0), Stmt::FunctionDecStmt {
                name: Identifier::from("wvars"),
                args: vec!((Identifier::from("a"), Type::i32), (Identifier::from("b"), Type::i32)),
                kwargs: vec!(
                    (Identifier::from("c"), Type::i32, output(e.expression(PosStr::from("5")))),
                    (Identifier::from("d"), Type::i32, output(e.expression(PosStr::from("7"))))
                ),
                block: output(e.block(PosStr::from("let val =  5\n"), 0)),
                return_type: Type::empty
            });

            check_data("fn wkwargs(a: i32, c: i32=5):\n let val = 5", |x| e.statement(x, 0), Stmt::FunctionDecStmt {
                name: Identifier::from("wkwargs"),
                args: vec![(Identifier::from("a"), Type::i32)],
                kwargs: vec!(
                    (Identifier::from("c"), Type::i32, output(e.expression(PosStr::from("5")))),
                ),
                block: output(e.block(PosStr::from("let val=5\n"), 0)),
                return_type: Type::empty
            });

            check_data("fn a(b: i32) -> i32:\n let x = 5 + 6\n return x\n", |x| e.statement(x, 0), Stmt::FunctionDecStmt {
                name: Identifier::from("a"),
                args: vec!((Identifier::from("b"), Type::i32)),
                kwargs: vec!(),
                block: output(e.block(PosStr::from("let x = 5 + 6\nreturn x"), 0)),
                return_type: Type::i32
            });
        }

        #[test]
        fn parse_struct_dec() {
            let input = "struct A:  \n   \n\n  x: i32\n  y: i32\n";
            check_data(input, |x| struct_declaration_stmt(x, 1), Stmt::StructDec{
                name: Identifier::from("A"),
                fields: vec!(
                    (Identifier::from("x"), Type::i32),
                    (Identifier::from("y"), Type::i32)
                )
            });
        }
        
        #[test]
        fn parse_if_stmt() {
            let e = ParserContext::empty();
            let good_input = "if (a and b):\n x = true";

            let good_output = Stmt::IfStmt{
                condition: output(e.expression(PosStr::from("a and b"))),
                block: Node::from(Block{statements: vec!(Box::new(output(e.assignment_stmt(PosStr::from("x = true")))))}),
                elifs: vec!(),
                else_block: None
            };

            check_data(good_input, |x| e.statement(x, 0), good_output);

            check_failed("ifa and b:\n x = true", |x| e.statement(x, 0), ErrorKind::Alt);

            check_data("if    true   :     \n\n\n  x = true\n elif    false   :   \n\n\n  y = true\n else     :  \n  z = true", |x| e.if_stmt(x, 1), Stmt::IfStmt {
                condition: Node::from(true),
                block: output(e.block(PosStr::from("x = true"), 0)),
                elifs: vec!((Node::from(false), output(e.block(PosStr::from("y = true"), 0)))),
                else_block: Some(output(e.block(PosStr::from("z = true"), 0)))
            });
        }

        #[test]
        fn parse_while_stmt() {
            let e = ParserContext::empty();
            check_data("while true:\n x=true", |x| e.statement(x, 0), Stmt::WhileStmt {
                condition: Node::from(true),
                block: Node::from(Block{statements: vec!(Box::new(output(e.assignment_stmt(PosStr::from("x=true")))))})
            });

            simple_check_failed("while true\n x = true", |x| e.statement(x, 0));
        }

        #[test]
        fn parse_for_in_stmt() {
            let e = ParserContext::empty();
            check_data("for x in y:\n a=true", |x| e.statement(x, 0), Stmt::ForInStmt {
                iter_vars: Identifier::from("x"),
                iterator: Node::from("y"),
                block: output(e.block(PosStr::from("a=true"), 0))
            });
        }

        #[test]
        fn parse_simple_statements() {
            let e = ParserContext::empty();
            check_data("pass", |x| e.statement(x, 0), Stmt::PassStmt);
            check_data_and_leftover("pass   \n  ", |x| e.statement(x, 0), Stmt::PassStmt, "\n  ");
            check_data("continue", |x| e.statement(x, 0), Stmt::ContinueStmt);
            check_data_and_leftover("continue   \n  ", |x| e.statement(x, 0), Stmt::ContinueStmt, "\n  ");
            check_data("break", |x| e.statement(x, 0), Stmt::BreakStmt);
            check_data_and_leftover("break   \n  ", |x| e.statement(x, 0), Stmt::BreakStmt, "\n  ");
        }

        #[test]
        fn parse_return_and_yield_stmts() {
            let e = ParserContext::empty();
            check_data("return true", |x| e.statement(x, 0), Stmt::ReturnStmt (Node::from(true)));
            check_data_and_leftover("return 1  \n  ", |x| e.statement(x, 0), Stmt::ReturnStmt(Node::from(1)), "\n  ");

            check_data("yield true", |x| e.statement(x, 0), Stmt::YieldStmt (Node::from(true)));
        }
    }
}

/// All expression parsers.
pub mod expr_parsers {
    use super::*;

    /// Top-level expression and some extras.
    impl ParserContext {
        pub fn expression<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            return alt_complete!(input, m!(self.comparison_expr));
        }

        /// Match a match expression.
        fn match_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {

            let parse_result = tuple!(input,
                delimited!(
                    MATCH,
                    m!(self.expression),
                    tuple!(
                        COLON,
                        between_statement
                    )
                ),
                separated_nonempty_list_complete!(
                    between_statement,
                    separated_pair!(
                        alt!(float_expr | int_expr | string_expr),
                        ARROW,
                        m!(self.expression)
                    )
                )
            );

            return fmap_node(parse_result, |x| Expr::MatchExpr {value: Box::new(x.0), cases: x.1});
        }

        /// Match any unary expression.
        /// Implemented as a single parser because all unary expressions have the same precedence.
        fn unary_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            let parse_result = alt!(input,
                tuple!(
                    map!(alt!(PLUS | MINUS | TILDE | NOT), Some),
                    m!(self.unary_expr)
                ) |
                tuple!(
                    value!(None, tag!("")),
                    m!(self.power_expr)
                )
            );

            let node = fmap_iresult(parse_result, |x|
                match x.0 {
                    Some(y) => Node::from(Expr::UnaryExpr {operator: UnaryOperator::from(y.slice), operand: Box::new(x.1)}),
                    None => x.1

                });
            return node;
        }
    }

    /// Binary expressions
    impl ParserContext {
        /// Match a comparison expression.
        fn comparison_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            let parse_result = tuple!(input,
                alt!(m!(self.match_expr) | m!(self.logical_binary_expr)),
                optc!(tuple!(
                    alt_complete!( DEQUAL | NEQUAL | LEQUAL | GEQUAL | LANGLE  | RANGLE),
                    m!(self.comparison_expr)
                ))
            );

            let map = |x: (Node<Expr>, Option<(PosStr<'a>, Node<Expr>)>)| match x.1 {
                None => x.0,
                Some(y) => {
                    let operator = ComparisonOperator::from(y.0);
                    Node::from(Expr::ComparisonExpr{operator, left: Box::new(x.0), right: Box::new(y.1)})
                }
            };

            let node = fmap_iresult(parse_result, map);
            return node;
        }

        /// Match a list of binary operations
        fn binary_expr<'a>(&self, input: PosStr<'a>, operator_parser: impl Fn(PosStr) -> IResult<PosStr, PosStr>, next_expr: impl Fn(PosStr) -> ExprRes) -> ExprRes<'a> {
            let parse_result = tuple!(input,
                next_expr,
                optc!(tuple!(
                    operator_parser,
                    m!(self.binary_expr, operator_parser, next_expr)
                ))
            );

            return fmap_iresult(parse_result, flatten_binary);
        }

        /// Match logical expressions.
        /// Must be public because it's used by several statements
        pub fn logical_binary_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            return self.binary_expr(input, |x| alt_complete!(x, AND | OR | XOR), |x| self.bitwise_binary_expr(x));
        }

        /// Match bitwise boolean expressions.
        fn bitwise_binary_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            return self.binary_expr(input, |x| alt_complete!(x, BAND | VBAR | BXOR), |x| self.shift_expr(x));
        }

        /// Match bit shift expressions.
        fn shift_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            return self.binary_expr(input, |x| alt_complete!(x, LSHIFT | RSHIFT), |x| self.additive_expr(x));
        }

        /// Match addition and subtraction expressions.
        fn additive_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            return self.binary_expr(input, |x| alt_complete!(x, PLUS | MINUS), |x| self.mult_expr(x));
        }

        /// Match multiplication, division, and modulo expressions.
        fn mult_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            return self.binary_expr(input, |x| alt_complete!(x, STAR | DIV | MOD), |x| self.unary_expr(x));
        }

        /// Match an exponentiation expression.
        fn power_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            return self.binary_expr(input, |x| call!(x, EXP), |x| self.atomic_expr(x));
        }
    }

    /// Atomic expressions
    impl ParserContext {

        fn atomic_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            let node = w_followed!(input, alt_complete!(
                bool_expr |
                float_expr |
                int_expr |
                string_expr |
                delimited!(
                    OPEN_BRACE,
                    alt_complete!(m!(self.map_or_set_comprehension)  | m!(self.map_literal) | m!(self.set_literal)),
                    CLOSE_BRACE
                ) |
                delimited!(
                    OPEN_BRACKET,
                    alt_complete!(m!(self.vector_comprehension) | m!(self.vec_literal)),
                    CLOSE_BRACKET
                ) |
                m!(self.expr_with_trailer)
            ));
            return node;
        }

        /// An expression wrapped in parentheses.
        fn wrapped_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            let node = delimited!(input,
                OPEN_PAREN,
                alt_complete!(m!(self.generator_comprehension) | m!(self.tuple_literal) | m!(self.expression)),
                CLOSE_PAREN
            );

            return node;
        }

        /// Match a list of arguments in a function call.
        fn args_list<'a>(&self, input: PosStr<'a>) -> Res<'a, Vec<Node<Expr>>> {
            let parse_result = separated_nonempty_list_complete!(input,
                COMMA,
                terminated!(
                    m!(self.logical_binary_expr),
                    not!(EQUALS)
                )
            );
            return parse_result;
        }

        /// Match a list of keyword arguments in a function call.
        fn kwargs_list<'a>(&self, input: PosStr<'a>) -> Res<'a, Vec<(Identifier, Node<Expr>)>> {
            let parse_result = separated_list!(input,
                COMMA,
                tuple!(
                    IDENTIFIER,
                    preceded!(
                        EQUALS,
                        m!(self.logical_binary_expr)
                    )
                )
            );
            return parse_result;
        }

        fn struct_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            let result = tuple!(input,
                separated_nonempty_list_complete!(DOT, IDENTIFIER), 
                delimited!(OPEN_BRACE, m!(self.args_list), CLOSE_BRACE)
            );
            let map = |(idents, args): (Vec<Identifier>, Vec<Node<Expr>>)| {
                let mut tree_base = Expr::IdentifierExpr(idents.get(0).unwrap().clone());
                for attribute in idents[1..].iter() {
                    tree_base = Expr::AttributeAccess {base: Box::new(Node::from(tree_base)), 
                    attribute: attribute.clone()};
                };
                return Expr::StructLiteral{base: Box::new(Node::from(tree_base)), fields: args};
            };
            return fmap_node(result, map);
        }

        /// An expression that can be followed by an arbitrary number of function calls or attribute accesses.
        fn expr_with_trailer<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            let ident_as_expr = |x| fmap_iresult(
                IDENTIFIER(x),
                |y: Identifier| Node::from(Expr::IdentifierExpr (y))
            );

            let parse_result = alt_complete!(input, 
                tuple!(
                    m!(self.struct_expr),
                    value!(vec!())
                ) |
                tuple!(
                    alt_complete!(ident_as_expr | m!(self.wrapped_expr)),
                    many0c!(m!(self.trailer))
                )
            );

            // Convert the vector of post identifiers into a single usable expression.
            let map = |x: (Node<Expr>, Vec<PostIdent>)| {
                let mut tree_base = x.0.data;
                for postval in x.1 {
                    match postval {
                        PostIdent::Call{args, kwargs} => {
                            tree_base = Expr::FunctionCall {function: Box::new(Node::from(tree_base)), args: args, kwargs: kwargs};
                        },
                        PostIdent::Access{attribute} => {
                            tree_base = Expr::AttributeAccess {base: Box::new(Node::from(tree_base)), attribute: attribute};
                        }
                        PostIdent::Index{slices} => {
                            tree_base = Expr::Index {base: Box::new(Node::from(tree_base)), slices: slices};
                        }
                    };
                };
                return Node::from(tree_base);
            };

            let node = fmap_iresult(parse_result, map);
            return node;
        }

        /// Parse an expression trailer.
        fn trailer<'a>(&self, input: PosStr<'a>) -> Res<'a, PostIdent> {
            return alt_complete!(input,
                m!(self.post_call) |
                post_access |
                m!(self.post_index)
            );
        }

        /// Match a function call following an expression.
        fn post_call<'a>(&self, input: PosStr<'a>) -> Res<'a, PostIdent> {
            let parse_result = delimited!(input,
                OPEN_PAREN,
                alt_complete!(
                    tuple!(
                        m!(self.args_list),
                        opt!(preceded!(
                            COMMA,
                            m!(self.kwargs_list)
                        ))
                    ) |
                    map!(m!(self.kwargs_list), |x| (vec!(), Some(x))) |
                    map!(peek!(CLOSE_PAREN), |_x| (vec!(), None))
                ),
                CLOSE_PAREN
            );
            return fmap_iresult(parse_result, |(x, y)| PostIdent::Call{
                args: x, 
                kwargs: match y {
                    Some(z) => z,
                    None => vec![]
                }
            });
        }

        /// Match an indexing operation following an expression.
        fn post_index<'a>(&self, input: PosStr<'a>) -> Res<'a, PostIdent> {
            let parse_result = delimited!(input,
                OPEN_BRACKET,
                separated_nonempty_list_complete!(
                    COMMA,
                    alt_complete!(
                        tuple!(
                            map!(m!(self.logical_binary_expr), |x| Some(x)),
                            optc!(tuple!(
                                preceded!(
                                    COLON,
                                    m!(self.logical_binary_expr)
                                ),
                                optc!(preceded!(
                                    COLON,
                                    m!(self.logical_binary_expr)
                                ))
                            ))
                        ) |
                        map!(COLON, |_| (None, None))
                    )
                ),
                CLOSE_BRACKET
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

            return fmap_iresult(parse_result, |x| PostIdent::Index { slices: c![flatten(y), for y in x] });
        }
    }

    /// Collection literals
    impl ParserContext {
        /// Match a vector literal.
        fn vec_literal<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {

            let parse_result = terminated!(input,
                separated_nonempty_list_complete!(
                    COMMA,
                    m!(self.logical_binary_expr)
                ),
                peek!(CLOSE_BRACKET)
            );

            return fmap_node(parse_result, |x| Expr::VecLiteral(x));
        }

        /// Match a set literal.
        fn set_literal<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            let parse_result = 
                separated_nonempty_list_complete!(input,
                    COMMA,
                    m!(self.logical_binary_expr)
                );

            return fmap_node(parse_result, |x| Expr::SetLiteral(x));
        }

        /// Match a map literal.
        fn map_literal<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {

            let parse_result = separated_nonempty_list_complete!(input,
                COMMA,
                separated_pair!(
                    m!(self.logical_binary_expr),
                    COLON,
                    m!(self.logical_binary_expr)
                )
            );

            return fmap_node(parse_result, |x| Expr::MapLiteral (x));
        }

        /// Match a tuple literal
        /// e.g. (), (1, ), (1,2,3), (1,2,3,)
        fn tuple_literal<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {

            let parse_result = alt_complete!(input,
                // Empty input
                map!(peek!(CLOSE_PAREN), |_| vec!()) |
                // Single element tuple.
                map!(
                    terminated!(
                        m!(self.logical_binary_expr),
                        tuple!(
                            COMMA,
                            peek!(CLOSE_PAREN)
                        )
                    ), |x| vec!(x)
                ) |
                terminated!(
                    separated_at_least_m!(2, COMMA, m!(self.logical_binary_expr)),
                    optc!(COMMA)
                )
            );

            return fmap_node(parse_result, |x| Expr::TupleLiteral(x));
        }

    }

    /// Comprehensions
    impl ParserContext {

        /// Match the for part of a comprehension.
        fn comprehension_for<'a>(&self, input: PosStr<'a>) -> Res<'a, ComprehensionIter> {
            let parse_result = tuple!(input,
                delimited!(FOR, variable_unpacking, IN),
                m!(self.logical_binary_expr),
                optc!(preceded!(IF, m!(self.logical_binary_expr)))
            );

            return fmap_iresult(parse_result, |(iter_vars, iterator, if_clause)| ComprehensionIter{
                iter_vars: iter_vars,
                iterator: Box::new(iterator),
                if_clause: if_clause
            });
        }

        /// Match a vector comprehension.
        fn vector_comprehension<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            let parse_result = tuple!(input,
                m!(self.logical_binary_expr),
                many1!(m!(self.comprehension_for))
            );

            return fmap_node(parse_result, |(value, iterators)| {
                return Expr::VecComprehension {
                    value: Box::new(value),
                    iterators: iterators
                };
            });
        }

        /// Match a generator comprehension.
        fn generator_comprehension<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            let parse_result = tuple!(input,
                m!(self.logical_binary_expr),
                many1!(m!(self.comprehension_for))
            );

            return fmap_node(parse_result, |x| Expr::GenComprehension {
                value: Box::new(x.0),
                iterators: x.1
            });
        }

        /// Match a map or a set.
        fn map_or_set_comprehension<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            let parse_result = tuple!(input,
                    m!(self.logical_binary_expr),
                    opt!(complete!(preceded!(
                        COLON,
                        m!(self.logical_binary_expr)
                    ))),
                    many1!(m!(self.comprehension_for))
            );

            return fmap_node(parse_result, |(key_or_value, value, iters)| match value {
                Some(y) => Expr::MapComprehension {
                    key: Box::new(key_or_value),
                    value: Box::new(y),
                    iterators: iters
                },
                None => Expr::SetComprehension {
                    value: Box::new(key_or_value),
                    iterators: iters
                }
            });
        }

    }

    /// A helper Enum for trailers.
    #[derive (Debug, Clone, PartialEq, Eq, Hash)]
    enum PostIdent {
        Call{args: Vec<Node<Expr>>, kwargs: Vec<(Identifier, Node<Expr>)>},
        Index{slices: Vec<(Option<Node<Expr>>, Option<Node<Expr>>, Option<Node<Expr>>)>},
        Access{attribute: Identifier}
    }

    /// Match an access operation following an expression.
    fn post_access<'a>(input: PosStr<'a>) -> IResult<PosStr<'a>, PostIdent> {
        let result = preceded!(input,
            DOT,
            IDENTIFIER
        );
        return fmap_iresult(result, |x| PostIdent::Access{attribute: x});
    }

    // BEGIN SIMPLE LITERALS

    /// Match a boolean literal expression.
    fn bool_expr<'a>(input: PosStr<'a>) -> ExprRes {
        let parse_result = w_followed!(input, 
            alt!(
            terminated!(tag!("true"), peek!(not!(IDENT_CHAR))) |
            terminated!(tag!("false"), peek!(not!(IDENT_CHAR)))
        ));
        return fmap_node(parse_result, |x| Expr::Bool(match from_utf8(x.slice).unwrap() {
            "true" => true,
            "false" => false,
            _ => panic!()
        }));
    }

    /// Match an integer literal expression.
    fn int_expr<'a>(input: PosStr<'a>) -> ExprRes {
        let parse_result = w_followed!(input, 
            recognize!(tuple!(
                optc!(SIGN),
                terminated!(
                    DIGIT,
                    VALID_NUM_FOLLOW
                )
            )
        ));
        return fmap_node(parse_result, |x| Expr::Int(from_utf8(x.slice).unwrap().to_string()));
    }

    /// Match a floating point literal expression.
    fn float_expr<'a>(input: PosStr<'a>) -> ExprRes {
        
        let exponent = |x: PosStr<'a>| preceded!(x, 
            alt!(tag!("e") | tag!("E")),
            tuple!(
                opt!(SIGN),
                DIGIT
            )
        );

        let with_dec = |x: PosStr<'a>| tuple!(x,
            tag!("."),
            DIGIT0,
            opt!(complete!(exponent))
        );

        let parse_result = w_followed!(input, 
            recognize!(tuple!(
                opt!(SIGN),
                DIGIT,
                alt!(
                    value!((), with_dec) |
                    value!((), complete!(exponent))
                ),
                VALID_NUM_FOLLOW
            )
        ));

        return fmap_node(parse_result, |x| Expr::Float(from_utf8(x.slice).unwrap().to_string()));
    }

    /// Match a string literal expression.
    fn string_expr<'a>(input: PosStr<'a>) -> ExprRes {
        let result = w_followed!(input, 
            recognize!(
                tuple!(
                    tag!("\""),
                    many0c!(STRING_CHAR),
                    tag!("\"")
                )
            )
        );
        return fmap_node(result, |x| Expr::String(from_utf8(x.slice).unwrap().to_string()))
    }

    // END SIMPLE LITERALS

    /// Flatten a possible binary expression into a single expression.
    fn flatten_binary<'a>(result: (Node<Expr>, Option<(PosStr<'a>, Node<Expr>)>)) -> Node<Expr> {
        return match result.1 {
            Some(x) => {
                let op = BinaryOperator::from(x.0.slice);
                Node::from(Expr::BinaryExpr {operator: op, left: Box::new(result.0), right: Box::new(x.1)})
            },
            None => result.0
        };
    }

    /// Match a split variable.
    fn variable_unpacking<'a>(input: PosStr<'a>) -> IResult<PosStr<'a>, Vec<Identifier>> {
        return separated_nonempty_list_complete!(input,
            COMMA,
            IDENTIFIER
        );
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn parse_post_ident() {
            let e = ParserContext::empty();
            let expected_args = vec!("a", "b", "c").iter().map(|x| Node::from(*x)).collect();
            check_match("( a ,  b , c ) ", |x| e.trailer(x), PostIdent::Call{args: expected_args, kwargs: vec![]});
            check_match("( a   ,  b  =    true)", |x| e.trailer(x), PostIdent::Call {
                args: vec!(Node::from("a")),
                kwargs: vec!((Identifier::from("b"), Node::from(true)))
            });

            check_match("( a   = true,  b = true ) ", |x| e.trailer(x), PostIdent::Call {
                args: vec![],
                kwargs: vec![(Identifier::from("a"), Node::from(true)), (Identifier::from("b"), Node::from(true))]
            });

            check_match("()", |x| e.trailer(x), PostIdent::Call {
                args: vec!(),
                kwargs: vec!()
            });

            simple_check_failed("(a | b=false)", |x| e.trailer(x));
            simple_check_failed("(a   b=false)", |x| e.trailer(x));
            simple_check_failed("(a,, b=false)", |x| e.trailer(x));
            simple_check_failed("(a,, b)", |x| e.trailer(x));
            simple_check_failed("(a, b =  true, c)", |x| e.trailer(x));


            check_match(".asdf_   ", |x| e.trailer(x), PostIdent::Access{attribute: Identifier::from("asdf_")});

            check_match("[a:b:c, :, d]", |x| e.trailer(x), PostIdent::Index {
                slices: vec!(
                    (Some(Node::from("a")), Some(Node::from("b")), Some(Node::from("c"))),
                    (None, None, None),
                    (Some(Node::from("d")), None, None)
                )
            });
        }

        #[test]
        fn test_parenthetical_expressions() {
            let e = ParserContext::empty();
            let expected = Expr::BinaryExpr {
                operator: BinaryOperator::Or,
                left: Box::new(Node::from(Expr::BinaryExpr {
                    operator:BinaryOperator::And,
                    left: Box::new(Node::from(true)),
                    right: Box::new(Node::from(false))
                })),
                right: Box::new(Node::from(true))
            };
            check_data("(true and false) or true", |x| e.expression(x), expected);

            check_data("(true and false)", |x| e.expression(x), Expr::BinaryExpr {
                
                operator:BinaryOperator::And,
                left: Box::new(Node::from(true)),
                right: Box::new(Node::from(false))
            });
        }

        #[test]
        fn parse_function_call() {
            let e = ParserContext::empty();
            let a = output(e.logical_binary_expr(PosStr::from("true and false")));
            let no_args = Node::from(Expr::FunctionCall{
                function: wrap(Expr::from("func")),
                args: vec!(),
                kwargs: vec!()
            });
            check_match("func()", |x| e.expr_with_trailer(x), no_args);

            let b = output(e.expression(PosStr::from("func()")));
            let expected = Expr::FunctionCall{
                function: wrap(Expr::from("ident")),
                args: vec!(a, b), 
                kwargs: vec!()
            };

            check_data("ident(true and false, func())", |x| e.expression(x), expected);

            check_data("func(a, b, c=true, d=true)", |x| e.expression(x), Expr::FunctionCall {
                function: Box::new(Node::from("func")),
                args: vec!(Node::from("a"), Node::from("b")),
                kwargs: vec!(
                    (Identifier::from("c"), Node::from(true)),
                    (Identifier::from("d"), Node::from(true))
                )
            });

            let expected = Expr::FunctionCall{
                function: Box::new(Node::from(Expr::FunctionCall{
                    function: Box::new(Node::from("func")), 
                    args: vec!(Node::from("a")), 
                    kwargs: vec!()
                })),
                args: vec!(Node::from("b"), Node::from("c")),
                kwargs: vec!()
            };
            check_data("func(a)(b, c)", |x| e.expression(x), expected);

            check_data("(a and b)(true)", |x| e.expression(x), Expr::FunctionCall {
                function: Box::new(output(e.logical_binary_expr(PosStr::from("a and b")))),
                args: vec!(Node::from(true)),
                kwargs: vec!()
            });
        }

        #[test]
        fn parse_comparison_expr() {
            let e = ParserContext::empty();
            let comp_strs = vec![">", "<", ">=", "<=", "==", "!="];
            let comp_ops = vec![ComparisonOperator::Greater, ComparisonOperator::Less, ComparisonOperator::GreaterEqual,
                                ComparisonOperator::LessEqual, ComparisonOperator::Equal, ComparisonOperator::Unequal];
            for (comp_str, comp_op) in comp_strs.iter().zip(comp_ops.iter()) {
                let as_str = format!("true {} false", comp_str);
                let expr = e.expression(PosStr::new(as_str.as_bytes()));
                let expected = Node::from(Expr::ComparisonExpr{ 
                    left: Box::new(Node::from(true)),
                    right: Box::new(Node::from(false)),
                    operator: *comp_op
                });

                assert_eq!(expr.unwrap().1, expected);
            }
        }

        #[test]
        fn parse_binary_expr() {
            let e = ParserContext::empty();

            check_data("1 ** 2", |x| e.expression(x), Expr::BinaryExpr{
                operator: BinaryOperator::Exponent,
                left: Box::new(Node::from(1)),
                right: Box::new(Node::from(2))
            });

            check_data("true and false", |x| e.expression(x), Expr::BinaryExpr{
                operator: BinaryOperator::And,
                left: Box::new(Node::from(true)),
                right: Box::new(Node::from(false))
            });

            check_data("true or false", |x| e.expression(x), Expr::BinaryExpr{
                operator: BinaryOperator::Or,
                left: Box::new(Node::from(true)),
                right: Box::new(Node::from(false))
            });

            check_data("true and false or true", |x| e.expression(x), Expr::BinaryExpr{
                
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
                check_data(input.as_str(), |x| e.expression(x), Expr::BinaryExpr {
                    operator: BinaryOperator::from(op),
                    left: Box::new(Node::from("x")),
                    right: Box::new(Node::from("y")),
                });
            }
        }

        #[test]
        fn parse_unary_expr() {
            let e = ParserContext::empty();
            let ops = vec!["not", "+", "-", "~"];
            for op in ops {
                let input = format!("{} y", op);
                check_data(input.as_str(), |x| e.expression(x), Expr::UnaryExpr {
                    operator: UnaryOperator::from(op),
                    operand: Box::new(Node::from("y")),
                });
            }
            check_data("~+y", |x| e.expression(x), Expr::UnaryExpr {
                operator: UnaryOperator::BitNot,
                operand: Box::new(output(e.expression(PosStr::from("+y")))),
            });
            check_data("not true", |x| e.expression(x), Expr::UnaryExpr {
                operator: UnaryOperator::Not, 
                operand: Box::new(Node::from(true))
            });
        }

        #[test]
        fn parse_comprehensions() {
            let e = ParserContext::empty();
            check_match("{x for x in y}", |x| e.expression(x), Node::from(Expr::SetComprehension {
                
                value: Box::new(Node::from("x")),
                iterators: vec!(ComprehensionIter {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(Node::from("y")),
                    if_clause: None
                })
            }));

            check_match("{x:z for x in y}", |x| e.expression(x), Node::from(Expr::MapComprehension {
                
                key: Box::new(Node::from("x")),
                value: Box::new(Node::from("z")),
                iterators: vec!(ComprehensionIter {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(Node::from("y")),
                    if_clause: None
                })
            }));

            check_match("[x for x in y]", |x| e.expression(x), Node::from(Expr::VecComprehension {
                
                value: Box::new(Node::from("x")),
                iterators: vec!(ComprehensionIter {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(Node::from("y")),
                    if_clause: None
                })
            }));

            check_match("(x for x in y)", |x| e.expression(x), Node::from(Expr::GenComprehension {
                value: Box::new(Node::from("x")),
                iterators: vec!(ComprehensionIter {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(Node::from("y")),
                    if_clause: None
                })
            }));

            check_match("[x for x in y for x in y]", |x| e.expression(x), Node::from(Expr::VecComprehension {
                value: Box::new(Node::from("x")),
                iterators: vec!(ComprehensionIter {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(Node::from("y")),
                    if_clause: None
                }, ComprehensionIter {
                    iter_vars: vec![Identifier::from("x")],
                    iterator: Box::new(Node::from("y")),
                    if_clause: None
                })
            }));

            check_match("for a, b in c if true", |x| e.comprehension_for(x), ComprehensionIter{
                iter_vars: c![Identifier::from(x), for x in vec!("a", "b")],
                iterator: Box::new(Node::from("c")),
                if_clause: Some(Node::from(true))
            });
        }

        #[test]
        fn parse_match_expr() {
            let e = ParserContext::empty();
            check_match("match x:\n5 => 5", |x| e.expression(x), Node::from(Expr::MatchExpr {
                value: Box::new(Node::from("x")),
                cases: vec![(Node::from(5), Node::from(5))]
            }));
        }

        #[test]
        fn parse_identifier_expr() {
            let e = ParserContext::empty();
            let expected: Node<Expr> = Node::from("words");
            check_match("words", |x| e.expression(x), expected);

            let expected = Node::from("abc_123");
            check_match("abc_123", |x| e.expression(x), expected);

            check_failed("(", |x| e.expression(x), ErrorKind::Alt);
        }

        #[test]
        fn parse_literals() {
            let e = ParserContext::empty();
            check_data_and_leftover("2]", |x| e.expression(x), Expr::from(2), "]");

            let int = rand::random::<i64>().abs();
            check_match(&int.to_string(), int_expr, Node::from(int));
            let rand_float = rand::random::<f64>().abs();
            check_match(&rand_float.to_string(), float_expr, Node::from(rand_float));
            let expr = Expr::String("\"asdf\\\"\\\rasdf\"".to_string());
            check_match("\"asdf\\\"\\\rasdf\"", |x| e.expression(x), Node::from(expr));

            check_match("{x : y}", |x| e.expression(x), Node::from(Expr::MapLiteral(vec!((Node::from("x"), Node::from("y"))))));
            check_match("[true, false]", |x| e.expression(x), Node::from(Expr::VecLiteral(vec!(Node::from(true), Node::from(false)))));
            check_match("{true, false}", |x| e.expression(x), Node::from(Expr::SetLiteral(vec!(Node::from(true), Node::from(false)))));
            check_match("(true, false)", |x| e.expression(x), Node::from(Expr::TupleLiteral(vec!(Node::from(true), Node::from(false)))));

            check_failed(".", |x| e.expression(x), ErrorKind::Alt);
        }

        #[test]
        fn parse_collection_literals() {
            let e = ParserContext::empty();
            check_data("[1, 2]", |x| e.expression(x), Expr::VecLiteral(
                vec!(Node::from(1), Node::from(2))
            ));

            check_data("()", |x| e.expression(x), Expr::TupleLiteral(
                vec!()
            ));

            check_data("(  )", |x| e.expression(x), Expr::TupleLiteral(
                vec!()
            ));

            check_data("(1, )", |x| e.expression(x), Expr::TupleLiteral(
                vec!(Node::from(1))
            ));

            check_data("(1, 2)", |x| e.expression(x), Expr::TupleLiteral(
                vec!(Node::from(1), Node::from(2))
            ));

            check_data("(1, 2,)", |x| e.expression(x), Expr::TupleLiteral(
                vec!(Node::from(1), Node::from(2))
            ));


            check_data("{a: 2}", |x| e.expression(x), Expr::MapLiteral(
                vec!((Node::from("a"), Node::from(2)))
            ));
        }

        #[test]
        fn parse_wrapped() {
            let e = ParserContext::empty();
            check_data("2 * (1 + (3))", |x| e.expression(x), Expr::BinaryExpr{
                operator: BinaryOperator::Mult,
                left: Box::new(Node::from(2)),
                right: Box::new(Node::from(Expr::BinaryExpr{
                    operator: BinaryOperator::Add,
                    left: Box::new(Node::from(Expr::from(1))),
                    right: Box::new(Node::from(Expr::from(3)))
                }))
            });
        }

        #[test]
        fn parse_struct_literal() {
            let e = ParserContext::empty();
            check_data("a.b{1,2,3}", |x| e.expression(x), Expr::StructLiteral{
                base: Box::new(Node::from(Expr::AttributeAccess{
                    base: Box::new(Node::from(Expr::IdentifierExpr(Identifier::from("a")))),
                    attribute: Identifier::from("b")
                })), 
                fields: vec!(Node::from(1), Node::from(2), Node::from(3))
            });
        }

        #[cfg(test)]
        mod subparsers {
            use super::*;

            #[test]
            fn parse_atomic() {
                let e = ParserContext::empty();
                check_data_and_leftover("1 ** 2", |x| e.atomic_expr(x), Expr::from(1), "** 2");
                check_data("false", bool_expr, Expr::from(false));
                check_data("false", |x| e.atomic_expr(x), Expr::from(false));
            }

            #[test]
            fn parse_spec_literals() {
                check_match("123", int_expr, Node::from(123));
                check_failed("e10", float_expr, ErrorKind::Digit);
                check_failed(".e10", float_expr, ErrorKind::Digit);
                check_failed(".0", float_expr, ErrorKind::Digit);
            }

            #[test]
            fn parse_expr_with_trailer() {
                let e = ParserContext::empty();
                check_failed("123", |x| e.expr_with_trailer(x), ErrorKind::Alt);
            }
        }
    }
}

/// Type parsers
pub mod type_parser {
    use super::*;
    /// Parse a type.
    pub fn any_type<'a>(input: PosStr<'a>) -> TypeRes {
        return alt_complete!(input, product_type | sum_type);
    }

    pub fn product_type<'a>(input: PosStr<'a>) -> TypeRes {
        let result = delimited!(input,
            OPEN_PAREN,
            separated_list!(
                COMMA,
                alt_complete!(product_type | sum_type)
            ),
            CLOSE_PAREN
        );

        return fmap_iresult(result, |x| Type::Product(x))
    }

    pub fn sum_type<'a>(input: PosStr<'a>) -> TypeRes {
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
                Type::Sum(x.1)
            }
        });
    }

    pub fn parameterized_type<'a>(input: PosStr<'a>) -> TypeRes {
        let result = tuple!(input, 
            IDENTIFIER,
            optc!(delimited!(
                LANGLE,
                separated_nonempty_list!(
                    COMMA,
                    any_type
                ),
                RANGLE
            ))
        );
        return fmap_iresult(result, |x| match x.1 {
            Some(y) => Type::Parameterized(x.0, y),
            None => Type::from(x.0)
        });
    }

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_simple_types() {
            check_match("i32", any_type, Type::i32);
            check_match("i64", any_type, Type::i64);
            check_match("f32", any_type, Type::f32);
            check_match("f64", any_type, Type::f64);
            check_match("ui32", any_type, Type::ui32);
            check_match("ui64", any_type, Type::ui64);
            check_match("boolean", any_type, Type::boolean);
            check_match("string", any_type, Type::string);

            // TODO: Random string test
        }

        #[test]
        fn test_parameterized_types() {
            check_match("Test<i32>", any_type, Type::Parameterized(
                Identifier::from("Test"), 
                vec!(Type::i32)
            ));
        }

        #[test]
        fn test_sum_types() {
            check_match("i32 | i64", any_type, Type::Sum(vec!(
                Type::i32, Type::i64
            )));
        }

        #[test]
        fn test_product_types() {
            check_match("(i32, i64)", any_type, Type::Product(vec!(
                Type::i32, Type::i64
            )));
        }
    }

}

/// Rewrite a for loop as a while loop.
/// 
/// # Arguments
/// 
/// * `loop_var` - The name of the variable that contains the iterator results
/// * `iterator` - The iterator expression
/// * `inner_loop` - The contexts of loop.
fn for_to_while(loop_var: Identifier, iterator: &Node<Expr>, mut inner_loop: StmtSeq) -> (StmtSeq, Stmt) {
   
    // The contents of the loop.

    let mut outer_stmts = vec!();

    // Expression to create the iterator.
    let iter_call = Node::from(Expr::FunctionCall{
        function: wrap(Expr::AttributeAccess{
            base: Box::new(iterator.clone()),
            attribute: Identifier::from("iter")
        }),
        args: vec!(),
        kwargs: vec!()
    });

    // The name of the iterator
    let loop_iter_name = Identifier::from(format!(".{}", get_next_var()));
    // The name of the iterator values.
    let loop_var_name =  Identifier::from(format!(".{}", get_next_var()));

    // Statement to store the iterator
    let loop_iter = Stmt::LetStmt{
        name: loop_iter_name.clone(),
        type_annotation: Some(Type::Undetermined),
        expression: iter_call
    };
    outer_stmts.push(wrap(loop_iter));

    // The next value of the iterator
    let iter_next = Stmt::LetStmt{
        name: loop_var_name.clone(),
        type_annotation: Some(Type::Undetermined),
        expression: Node::from(Expr::FunctionCall{
            function: wrap(Expr::AttributeAccess{
                base: wrap(Expr::from(loop_iter_name)),
                attribute: Identifier::from("next")
            }),
            args: vec!(),
            kwargs: vec!()
        })
    };
    // Add the first iteration outside the loop.
    outer_stmts.push(wrap(iter_next.clone()));
    // Call next at the end of every loop.
    inner_loop.push(wrap(iter_next));

    // The contents of the first value.
    let iter_var = Stmt::LetStmt{
        name: loop_var,
        type_annotation: Some(Type::Undetermined),
        expression: Node::from(Expr::Index{
            base: wrap(Expr::from(loop_var_name.clone())),
            slices: vec!((Some(Node::from(Expr::from(0))), None, None))
        })
    };
    outer_stmts.push(wrap(iter_var));

    let iter_state = Node::from(Expr::Index{
        base: wrap(Expr::from(loop_var_name)),
        slices: vec!((Some(Node::from(Expr::from(1))), None, None))
    });

    let while_loop = Stmt::WhileStmt{
        condition: iter_state, 
        block: Node::from(Block{statements: inner_loop})
    };


    return (outer_stmts, while_loop);
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse_module() {
        let e = ParserContext::empty();
        let module_str = "fn a():\n return 0\n\nfn b():\n return 1";
        check_match(module_str, module, Node::from(Module{
            declarations: vec!(
                Box::new(output(e.statement(PosStr::from("fn a():\n return 0"), 0))),
                Box::new(output(e.statement(PosStr::from("fn b():\n return 1"), 0)))
            ),
            imports: vec!()
        }))
    }

    #[test]
    fn parse_module_with_import() {
        let e = ParserContext::empty();
        let module_str = "import foo\nfn a():\n return 0\n\nfn b():\n return 1";
        check_match(module_str, module, Node::from(Module{
            declarations: vec!(
                Box::new(output(e.statement(PosStr::from("fn a():\n return 0"), 0))),
                Box::new(output(e.statement(PosStr::from("fn b():\n return 1"), 0)))
            ),
            imports: vec!(Box::new(Import{id: 0, path: vec!(Identifier::from("foo")), alias: None, values: vec!()})),
        }))
    }        
    #[test]
    fn parse_imports() {
        check_match("import foo.bar.baz", import, Import{
            id: 0,
            path: vec!(
                Identifier::from("foo"), 
                Identifier::from("bar"), 
                Identifier::from("baz")), 
            alias: None,
            values: vec!()
        });
    }

    #[test]
    fn parse_block() {
        let e = ParserContext::empty();
        let exp_block = Block {
            statements: vec![
                Box::new(output(e.statement(PosStr::from("x=0\n"), 0))),
                Box::new(output(e.statement(PosStr::from("y=true"), 0)))
            ]
        };

        check_match(" x=0\n y=true\n\n  \n", |x| e.block(x, 1), Node::from(exp_block));
    }

    #[cfg(test)]
    mod from_failures {
        use super::*;

        #[test]
        fn parse_solitary_expression() {
            let e = ParserContext::empty();
            simple_check_failed("1 + 3", module);
            simple_check_failed("1 + 3", |x| e.block(x, 0));
        }
    }
}
