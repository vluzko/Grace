use std::str::from_utf8;
use std::collections::HashMap;

extern crate nom;
use self::nom::*;


use expression::*;
use position_tracker::PosStr;
use parser_utils::*;
use parser_utils::iresult_helpers::*;
use parser_utils::tokens::*;

use typing::{Type, Refinement};
use general_utils::{
    get_next_id,
    get_next_var,
    join
};

use self::stmt_parsers::struct_declaration_stmt;

type StmtNode = Node<Stmt>;
type ExprNode = Node<Expr>;
type IO<'a> = IResult<PosStr<'a>, PosStr<'a>>;
type Res<'a, T> = IResult<PosStr<'a>, T>;
type StmtSeq = Vec<Box<Node<Stmt>>>;
type ExprU = (ExprNode, StmtSeq);
type StmtU = (StmtNode, StmtSeq);
type StmtRes<'a> = IResult<PosStr<'a>, StmtU>;
type ExprRes<'a> = IResult<PosStr<'a>, ExprU>;
type TypeRes<'a> = IResult<PosStr<'a>, Type>;


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
        return output(e.statement(PosStr::from(input), 0)).0;
    }
}

impl Parseable for Node<Expr> {
    fn parse<'a>(input: PosStr<'a>) -> Node<Expr> {
        let e = ParserContext::empty();
        return output(e.expression(PosStr::from(input))).0;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserContext {
    imported: HashMap<Identifier, Import>
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

        return fmap_node(parse_result, |x| {
            let mut statements = vec!();
            // Add all the updates to the block. Updates always go before the statement that created them.
            for (stmt, mut update) in x {
                statements.append(&mut update);
                statements.push(Box::new(stmt));
            }
            Block{statements}
        });
    }

    pub fn empty() -> ParserContext {
        return ParserContext{
            imported: HashMap::new()
        };
    }
}

/// Match a module.
pub fn module<'a>(input: PosStr<'a>) -> IResult<PosStr<'a>, Node<Module>>{
    let mut context = ParserContext::empty();

    let just_imports = preceded!(input,
        opt!(between_statement),
        many0c!(
            terminated!(import, between_statement)
        )
    );

    let (remaining, imports) = match &just_imports {
        Ok((i, parsed_imports)) => {
            for import in parsed_imports {
                match &import.alias {
                    Some(ref alias) => context.imported.insert(alias.clone(), import.clone()),
                    None => context.imported.insert(import.path.get(0).unwrap().clone(), import.clone())
                };
            }
            (i, parsed_imports)
        }, 
        _ => panic!()
    };

    let statements = terminated!(remaining,
        many1c!(
            terminated!(
                alt_complete!(m!(context.function_declaration_stmt, 0) | call!(struct_declaration_stmt, 0)),
                
                between_statement
            )
        ),
        alt_complete!(eof!() | EMPTY)
    );

    return fmap_node(statements, |x| Module{
        imports: imports.into_iter().map(|z| Box::new(z.clone())).collect(), 
        declarations: x.into_iter().map(|y| Box::new(y)).collect()
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
                map!(m!(self.function_declaration_stmt, indent), |x| (x, vec!())) |
                m!(self.return_stmt) |
                m!(self.yield_stmt) |
                break_stmt |
                pass_stmt |
                continue_stmt 
            );

            return node;
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

            return fmap_nodeu(parse_result, |(name, type_annotation, (expression, u))| 
                (Stmt::LetStmt {name, type_annotation, expression}, u)
            );
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

            return fmap_nodeu(parse_result, |(name, assn, (expr, u))| (Stmt::AssignmentStmt{
                name: name.clone(), 
                expression: match assn.slice {
                    b"=" => expr,
                    _ => {
                        let subop = &assn.slice[0..assn.slice.len()-1];
                        Node::from(Expr::BinaryExpr{
                            operator: BinaryOperator::from(subop),
                            left: Box::new(Node::from(Expr::IdentifierExpr(name))),
                            right: Box::new(expr)
                        })
                    }
                }
            }, u));
        }

        /// Parse a function declaration.
        pub fn function_declaration_stmt<'a>(&self, input: PosStr<'a>, indent: usize) -> Res<'a, StmtNode> {
            let arg_parser = |i: PosStr<'a>| tuple!(i,
                IDENTIFIER,
                preceded!(OPEN_PAREN, fn_dec_args),
                terminated!(m!(self.keyword_args), CLOSE_PAREN),
                optc!(preceded!(
                    TARROW,
                    type_parser::any_type
                ))
            );

            let parse_result = line_and_block!(input, self, preceded!(FN, arg_parser), indent);

            return fmap_node(parse_result, |((name, args, keyword_args, return_type), body)| {
                let mut res_kwargs = vec!();
                for (ident, t, (expr, u)) in keyword_args {
                    // No comprehensions or match expressions in keywords.
                    assert_eq!(u.len(), 0);
                    res_kwargs.push((ident, t, expr));
                }
                
                let stmt = Stmt::FunctionDecStmt{
                    name: name,
                    args: args,
                    kwargs: res_kwargs,
                    block: body,
                    return_type: match return_type {
                        Some(x) => x,
                        None => Type::empty
                    }
                };

                return stmt;
            });
        }

        /// Match an if statement.
        fn if_stmt<'a>(&self, input: PosStr<'a>, indent: usize) -> StmtRes<'a> {
            let parse_result = tuple!(input,
                line_and_block!(self, preceded!(IF, m!(self.expression)), indent),
                many0c!(indented!(line_and_block!(self, preceded!(ELIF, m!(self.expression)), indent), indent)),
                opt!(complete!(indented!(keyword_and_block!(self, ELSE, indent), indent)))
            );

            return fmap_nodeu(parse_result, |(((cond, mut cond_u), block), elifs, mut else_block)| {
                let mut just_elifs = Vec::with_capacity(elifs.len());

                for ((c, mut c_u), b) in elifs.into_iter() {
                    cond_u.append(&mut c_u);
                    just_elifs.push((c, b));
                }

                for (elif_cond, elif_block) in just_elifs.into_iter().rev() {
                    let sub_if = wrap(Stmt::IfStmt{condition: elif_cond, block: elif_block, else_block: else_block});
                    else_block = Some(Node::from(Block{statements: vec!(sub_if)}));
                }

                let stmt = Stmt::IfStmt{condition: cond, block: block, else_block};
                return (stmt, cond_u);
            });
        }

        /// Parse a while loop.
        fn while_stmt<'a>(&self, input: PosStr<'a>, indent: usize) -> StmtRes<'a> {
            let parse_result = line_and_block!(input, self, preceded!(WHILE, m!(self.expression)), indent);
            return fmap_nodeu(parse_result, |((cond, cu), block)| (Stmt::WhileStmt {condition: cond, block: block}, cu));
        }

        /// Parse a for in loop.
        fn for_in<'a>(&self, input: PosStr<'a>, indent: usize) -> StmtRes<'a> {
            let parse_result = line_and_block!(input, self, tuple!(
                preceded!(
                    FOR,
                    IDENTIFIER
                ),
                preceded!(
                    IN,
                    m!(self.expression)
                )
            ), indent);

            return fmap_nodeu(parse_result, |((iter_var, (iterator, iu)), block)| {
                let (stmt, update) = for_to_while(iter_var, &iterator, block.data.statements);
                (stmt, join(iu, update))
            });
        }

        /// Match a return statement.
        fn return_stmt<'a>(&self, input: PosStr<'a>) -> StmtRes<'a> {
            let parse_result = preceded!(input, RETURN, m!(self.expression));
            return fmap_pass(parse_result,|x| Stmt::ReturnStmt (x));
        }

        /// Match a yield statement.
        fn yield_stmt<'a>(&self, input: PosStr<'a>) -> StmtRes<'a> {
            let parse_result = preceded!(input, YIELD, m!(self.expression));
            return fmap_pass(parse_result, |x| Stmt::YieldStmt(x))
        }

        /// Match all keyword arguments in a function declaration.
        fn keyword_args<'a>(&self, input: PosStr<'a>) -> Res<'a, Vec<(Identifier, Type, ExprU)>> {
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

    pub fn struct_declaration_stmt<'a>(input: PosStr<'a>, indent: usize) -> Res<'a, StmtNode> {
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

        return fmap_nodeu(parse_result, |_| (Stmt::BreakStmt, vec!()));
    }

    /// Match a pass statement.
    pub fn pass_stmt<'a>(input: PosStr<'a>) -> StmtRes {
        let parse_result = terminated!(input,
            PASS,
            peek!(alt_complete!(
                eof!() | NEWLINE | EMPTY
            ))
        );

        return fmap_nodeu(parse_result, |_| (Stmt::PassStmt, vec!()));
    }

    /// Match a continue statement.
    pub fn continue_stmt<'a>(input: PosStr<'a>) -> StmtRes {
        let parse_result = terminated!(input,
            CONTINUE,
            peek!(alt_complete!(
                eof!() | NEWLINE | EMPTY
            ))
        );

        return fmap_nodeu(parse_result, |_| (Stmt::ContinueStmt, vec!()));
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
                   (Identifier::from("c"), Type::i32, (Node::from(5), vec!())),
                   (Identifier::from("d"), Type::i32, (Node::from(7), vec!()))
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
                    (Identifier::from("c"), Type::i32, output(e.expression(PosStr::from("5"))).0),
                    (Identifier::from("d"), Type::i32, output(e.expression(PosStr::from("7"))).0)
                ),
                block: output(e.block(PosStr::from("let val =  5\n"), 0)),
                return_type: Type::empty
            });

            check_data("fn wkwargs(a: i32, c: i32=5):\n let val = 5", |x| e.statement(x, 0), Stmt::FunctionDecStmt {
                name: Identifier::from("wkwargs"),
                args: vec![(Identifier::from("a"), Type::i32)],
                kwargs: vec!(
                    (Identifier::from("c"), Type::i32, output(e.expression(PosStr::from("5"))).0),
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
            check_match(input, |x| struct_declaration_stmt(x, 1), Node::from(Stmt::StructDec{
                name: Identifier::from("A"),
                fields: vec!(
                    (Identifier::from("x"), Type::i32),
                    (Identifier::from("y"), Type::i32)
                )
            }));
        }
        
        #[test]
        fn parse_if_stmt() {
            let e = ParserContext::empty();
            let good_input = "if (a and b):\n x = true";

            let good_output = Stmt::IfStmt{
                condition: output(e.expression(PosStr::from("a and b"))).0,
                block: Node::from(Block{statements: vec!(Box::new(output(e.assignment_stmt(PosStr::from("x = true"))).0))}),
                else_block: None
            };

            check_data(good_input, |x| e.statement(x, 0), good_output);

            check_failed("ifa and b:\n x = true", |x| e.statement(x, 0), ErrorKind::Alt);

            check_data("if    true   :     \n\n\n  x = true\n elif    false   :   \n\n\n  y = true\n else     :  \n  z = true", |x| e.if_stmt(x, 1), Stmt::IfStmt {
                condition: Node::from(true),
                block: output(e.block(PosStr::from("x = true"), 0)),
                // elifs: vec!((Node::from(false), output(e.block(PosStr::from("y = true"), 0)))),
                else_block: Some(Node::from(Block{
                    statements: vec!(wrap(Stmt::IfStmt{
                        condition: Node::from(false),
                        block: output(e.block(PosStr::from("y = true"), 0)),
                        else_block:Some(output(e.block(PosStr::from("z = true"), 0)))
                    }))
                }))
            });
        }

        #[test]
        fn parse_while_stmt() {
            let e = ParserContext::empty();
            check_data("while true:\n x=true", |x| e.statement(x, 0), Stmt::WhileStmt {
                condition: Node::from(true),
                block: Node::from(Block{statements: vec!(Box::new(output(e.assignment_stmt(PosStr::from("x=true"))).0))})
            });

            simple_check_failed("while true\n x = true", |x| e.statement(x, 0));
        }

        #[test]
        fn parse_for_in_stmt() {
            // let e = ParserContext::empty();
            // check_data("for x in y:\n a=true", |x| e.statement(x, 0), Stmt::ForInStmt {
            //     iter_vars: Identifier::from("x"),
            //     iterator: Node::from("y"),
            //     block: output(e.block(PosStr::from("a=true"), 0))
            // });
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

            // return fmap_node(parse_result, |x| Expr::MatchExpr {value: Box::new(x.0), cases: x.1});
            panic!("{:?}", parse_result)
        }

        /// Match any unary expression.
        /// Implemented as a single parser because all unary expressions have the same precedence.
        fn unary_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {

            let parse_result = alt!(input,
                tuple!(
                    map!(alt_complete!(PLUS | NEG | TILDE | NOT), Some),
                    m!(self.unary_expr)
                ) |
                tuple!(
                    value!(None, tag!("")),
                    m!(self.power_expr)
                )
            );

            let node = fmap_iresult(parse_result, |(maybe_op, (expr, u))|
                match maybe_op {
                    Some(op_str) => (Node::from(Expr::UnaryExpr {operator: UnaryOperator::from(op_str), operand: Box::new(expr)}), u),
                    None => (expr, u)

                });
            return node;
        }
    }

    /// Binary expressions
    impl ParserContext {
        /// Match a comparison expression.
        fn comparison_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            let parse_result = tuple!(input,
                m!(self.logical_binary_expr),
                optc!(tuple!(
                    alt_complete!( DEQUAL | NEQUAL | LEQUAL | GEQUAL | LANGLE  | RANGLE),
                    m!(self.comparison_expr)
                ))
            );

            let map = |x: (ExprU, Option<(PosStr<'a>, ExprU)>)| match x.1 {
                None => x.0,
                Some((o, (right, mut u))) => {
                    let operator = ComparisonOperator::from(o.slice);
                    let (left, mut update) = x.0;
                    update.append(&mut u);
                //     (Node::from(Expr::BinaryExpr {operator: op, left: Box::new(left), right: Box::new(right)}), update)
                // },
                    // let operator = ComparisonOperator::from(y.0);
                    (Node::from(Expr::ComparisonExpr{operator, left: Box::new(left), right: Box::new(right)}), update)
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
        fn args_list<'a>(&self, input: PosStr<'a>) -> Res<'a, Vec<ExprU>> {
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
        fn kwargs_list<'a>(&self, input: PosStr<'a>) -> Res<'a, Vec<(Identifier, ExprU)>> {
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
            let map = |(idents, au): (Vec<Identifier>, Vec<ExprU>)| {
                let mut tree_base = Expr::IdentifierExpr(idents.get(0).unwrap().clone());
                for attribute in idents[1..idents.len()-1].iter() {
                    tree_base = Expr::AttributeAccess {base: Box::new(Node::from(tree_base)), 
                    attribute: attribute.clone()};
                };

                let rewritten = self.rewrite_access(tree_base, idents.get(idents.len()-1).unwrap().clone());

                let mut args = vec!();
                let mut update = vec!();
                for (val, mut u) in au {
                    args.push(val);
                    update.append(&mut u);
                }
                return (Expr::StructLiteral{base: Box::new(Node::from(rewritten)), fields: args}, update);
            };
            return fmap_nodeu(result, map);
        }

        /// An expression that can be followed by an arbitrary number of function calls, attribute accesses, or indices.
        fn expr_with_trailer<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            let ident_as_expr = |x| fmap_nodeu(
                IDENTIFIER(x),
                |y: Identifier| (Expr::IdentifierExpr(y), vec!())
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
            let map = |((base, mut update), post): (ExprU, Vec<(PostIdent, StmtSeq)>)| {
                let mut tree_base = base.data;
                for (postval, mut u) in post {
                    update.append(&mut u);
                    tree_base = match postval {
                        PostIdent::Call{args, kwargs} => Expr::FunctionCall {function: wrap(tree_base), args: args, kwargs: kwargs},
                        PostIdent::Index{slices} => Expr::Index {base: wrap(tree_base), slices: slices},
                        PostIdent::Access{attribute} => self.rewrite_access(tree_base, attribute)
                    };
                };
                return (tree_base, update);
            };

            let node = fmap_nodeu(parse_result, map);
            return node;
        }

        /// Parse an expression trailer.
        fn trailer<'a>(&self, input: PosStr<'a>) -> Res<'a, (PostIdent, StmtSeq)> {
            
            return alt_complete!(input,
                m!(self.post_call) |
                post_access |
                m!(self.post_index)
            );
        }

        /// Match a function call following an expression.
        fn post_call<'a>(&self, input: PosStr<'a>) -> Res<'a, (PostIdent, StmtSeq)> {
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
            return fmap_iresult(parse_result, |(args, kwargs)| {
                let mut just_args = vec!();
                let mut update = vec!();
                for (arg, mut u) in args {
                    just_args.push(arg);
                    update.append(&mut u);
                }

                let just_kwargs = match kwargs {
                    Some(x) => {
                        let mut t = vec!();
                        for (ident, (expr, mut u)) in x {
                            t.push((ident, expr));
                            update.append(&mut u);
                        }
                        t
                    },
                    None => vec!()
                };
                
                return (PostIdent::Call{args: just_args, kwargs: just_kwargs}, update);
            });
        }

        /// Match an indexing operation following an expression.
        fn post_index<'a>(&self, input: PosStr<'a>) -> Res<'a, (PostIdent, StmtSeq)> {
            let parse_result = delimited!(input,
                OPEN_BRACKET,
                separated_nonempty_list_complete!(
                    COMMA,
                    tuple!(
                        optc!(m!(self.logical_binary_expr)),
                        map!(optc!(preceded!(
                            COLON,
                            optc!(m!(self.logical_binary_expr))
                        )), |x| match x {
                            Some(y) => y,
                            None => None
                        }),
                        map!(optc!(preceded!(
                            COLON,
                            optc!(m!(self.logical_binary_expr))
                        )), |x| match x {
                            Some(y) => y,
                            None => None
                        })
                    )
                ),
                CLOSE_BRACKET
            );

            return fmap_iresult(parse_result, |x| {
                let mut indices = vec!();
                let mut update = vec!();

                for (opt1, opt2, opt3) in x {
                    let i1 = match opt1 {
                        Some((x, mut u)) => {
                            update.append(&mut u);
                            Some(x)
                        },
                        None => None
                    };

                    let i2 = match opt2 {
                        Some((x, mut u)) => {
                            update.append(&mut u);
                            Some(x)
                        },
                        None => None
                    };

                    let i3 = match opt3 {
                        Some((x, mut u)) => {
                            update.append(&mut u);
                            Some(x)
                        },
                        None => None
                    };

                    indices.push((i1, i2, i3));
                }

                return (PostIdent::Index{slices: indices}, update);
            });
        }

        /// Rewrite an AttributeAccess as a ModuleAccess if necessary
        /// Will rewrite if the base expression is an identifier in the imports set, or if it's a ModuleExpression.
        fn rewrite_access(&self, base: Expr, attribute: Identifier) -> Expr {
            
            return match base {
                Expr::ModuleAccess(id, mut v) => {
                    v.push(attribute);
                    Expr::ModuleAccess(id, v)
                },
                Expr::IdentifierExpr(name) => {
                    match self.imported.get(&name) {
                        Some(import) => Expr::ModuleAccess(import.id, vec!(name, attribute)),
                        None => Expr::AttributeAccess{base: wrap(Expr::IdentifierExpr(name)), attribute: attribute}
                    }
                },
                x => Expr::AttributeAccess {base: wrap(x), attribute: attribute}
            };
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

            return fmap_nodeu(parse_result, |x| {
                let mut exprs = vec!();
                let mut updates = vec!(); 
                for (expr, mut update) in x {
                    exprs.push(expr);
                    updates.append(&mut update);
                }
                return (Expr::VecLiteral(exprs), updates);
            });
        }

        /// Match a set literal.
        fn set_literal<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            let parse_result = 
                separated_nonempty_list_complete!(input,
                    COMMA,
                    m!(self.logical_binary_expr)
                );

            return fmap_nodeu(parse_result, |x| {
                let mut exprs = vec!();
                let mut updates = vec!(); 
                for (expr, mut update) in x {
                    exprs.push(expr);
                    updates.append(&mut update);
                }  
                return (Expr::SetLiteral(exprs), updates);
            });
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

            return fmap_nodeu(parse_result, |x| {
                let mut mappings = vec!();
                let mut updates = vec!();

                for ((key, mut ku), (value, mut vu)) in x {
                    mappings.push((key, value));
                    updates.append(&mut ku);
                    updates.append(&mut vu);
                }

                return (Expr::MapLiteral(mappings), updates);
            });
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

            return fmap_nodeu(parse_result, |x| {
                let mut exprs = vec!();
                let mut updates = vec!();
                for (expr, mut update) in x {
                    exprs.push(expr);
                    updates.append(&mut update);
                }
                return (Expr::TupleLiteral(exprs), updates);
            });
        }

    }

    /// Rewrite a comprehension into an expression and a for loop.
    /// 
    /// # Arguments
    /// * `iterators` - The iterators in each for clause of the comprehension (in order)
    /// * `coll_create` - The statement to create the collection.
    /// * `add_value` - The statement to add a value to the collection.
    fn rewrite_comprehension(iterators: Vec<(Vec<Identifier>, ExprU, Option<ExprU>)>, coll_create: Stmt, add_value: Stmt) -> ExprU {
        let coll_name = next_hidden();
        // The statement to create the vector.

        let mut outer_stmts = vec!();
        outer_stmts.push(wrap(add_value));

        for (iter_vars, (iterator, mut iter_u), if_clause) in iterators.into_iter().rev() {

            // The contents of the loop.
            let (inner_stmts, new_outer) = match if_clause {
                None => (outer_stmts, vec!()),
                Some((cond, u)) => {

                    let if_stmt = Stmt::simple_if(cond, Block{statements: outer_stmts});
                    let new_stmts = vec!(wrap(if_stmt));
                    (new_stmts, u)
                }
            };
                
            outer_stmts = new_outer;

            outer_stmts.append(&mut iter_u);

            let mut rewritten = for_to_while(
                iter_vars.get(0).unwrap().clone(), 
                &iterator, 
                inner_stmts
            );
            
            outer_stmts.append(&mut rewritten.1);
            outer_stmts.push(wrap(rewritten.0));
        }

        outer_stmts.insert(0, wrap(coll_create));
        let new_expr = Expr::IdentifierExpr(coll_name.clone());

        return (Node::from(new_expr), outer_stmts);
    }

    /// Comprehensions
    impl ParserContext {
        /// Match the for part of a comprehension.
        fn comprehension_for<'a>(&self, input: PosStr<'a>) -> Res<'a, (Vec<Identifier>, ExprU, Option<ExprU>)> {
            let parse_result = tuple!(input,
                delimited!(FOR, variable_unpacking, IN),
                m!(self.logical_binary_expr),
                optc!(preceded!(IF, m!(self.logical_binary_expr)))
            );

            return parse_result;
        }

        /// Match a vector comprehension.
        fn vector_comprehension<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {

            let parse_result = tuple!(input,
                m!(self.logical_binary_expr),
                many1!(m!(self.comprehension_for))
            );

            return fmap_iresult(parse_result, |((value, mut v_update), iterators)| {
                // The internal name for the collection.
                let coll_name = next_hidden();
                // The statement to create the vector.
                let coll_create = coll_name.simple_let(Expr::from("vec").call());
                
                // The statement to push the next element onto the vector.
                let push = coll_name.assn(coll_name.as_expr().access(&"push").callw(vec!(value)));
                let (ref_expr, mut rewritten) = rewrite_comprehension(iterators, coll_create, push);
                v_update.append(&mut rewritten);

                return (ref_expr, v_update);
            });
            
        }

        /// Match a generator comprehension.
        fn generator_comprehension<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
            let parse_result = tuple!(input,
                m!(self.logical_binary_expr),
                many1!(m!(self.comprehension_for))
            );

            return fmap_iresult(parse_result, |((value, mut v_update), iterators)| {
                // The internal name for the collection.
                let coll_name = next_hidden();
                // The statement to create the vector.
                let coll_create = coll_name.simple_let(Expr::from("gen").call());
                // The statement to push the next element onto the vector.
                let push = coll_name.assn(coll_name.as_expr().access(&"push").callw(vec!(value)));
                let (ref_expr, mut rewritten) = rewrite_comprehension(iterators, coll_create, push);
                v_update.append(&mut rewritten);

                return (ref_expr, v_update);
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
            
            return fmap_iresult(parse_result, |((key_or_value, mut kv_update), opt_value, iterators)| {
                
                let coll_name = next_hidden();
                let (create, add) = match opt_value {
                    Some((value, mut v_update)) => {
                        let create = coll_name.simple_let(Expr::from("map").call());
                        let add = coll_name.assn(coll_name.as_expr().access(&"add").callw(vec!(key_or_value, value)));
                        kv_update.append(&mut v_update);
                        (create, add)
                    },
                    None => {
                        let create = coll_name.simple_let(Expr::from("set").call());
                        let add = coll_name.assn(coll_name.as_expr().access(&"add").callw(vec!(key_or_value)));
                        (create, add)
                    }
                };

                let (ref_expr, mut rewritten) = rewrite_comprehension(iterators, create, add);
                kv_update.append(&mut rewritten);

                return (ref_expr, kv_update);
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
    fn post_access<'a>(input: PosStr<'a>) -> IResult<PosStr<'a>, (PostIdent, StmtSeq)> {
        let result = preceded!(input,
            DOT,
            IDENTIFIER
        );
        return fmap_iresult(result, |x| (PostIdent::Access{attribute: x}, vec!()));
    }

    // BEGIN SIMPLE LITERALS

    /// Match a boolean literal expression.
    fn bool_expr<'a>(input: PosStr<'a>) -> ExprRes {
        let parse_result = w_followed!(input, 
            alt!(
            terminated!(tag!("true"), peek!(not!(IDENT_CHAR))) |
            terminated!(tag!("false"), peek!(not!(IDENT_CHAR)))
        ));
        return fmap_nodeu(parse_result, |x| (Expr::Bool(match from_utf8(x.slice).unwrap() {
            "true" => true,
            "false" => false,
            _ => panic!()
        }), vec!()));
    }

    /// Match an integer literal expression.
    fn int_expr<'a>(input: PosStr<'a>) -> ExprRes {
        let parse_result = just_int(input);
        return fmap_nodeu(parse_result, |x| (Expr::Int(from_utf8(x.slice).unwrap().to_string()), vec!()));
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

        return fmap_nodeu(parse_result, |x| (Expr::Float(from_utf8(x.slice).unwrap().to_string()), vec!()));
    }

    /// Match a string literal expression.
    fn string_expr<'a>(input: PosStr<'a>) -> ExprRes {
        let result = w_followed!(input, 
                delimited!(
                    tag!("\""),
                    recognize!(many0c!(STRING_CHAR)),
                    tag!("\"")
                )
        );
        return fmap_nodeu(result, |x| (Expr::String(from_utf8(x.slice).unwrap().to_string()), vec!()))
    }

    // END SIMPLE LITERALS

    /// Flatten a possible binary expression into a single expression.
    fn flatten_binary<'a>(result: (ExprU, Option<(PosStr<'a>, ExprU)>)) -> ExprU {
        return match result.1 {
            Some((o, (right, mut u))) => {
                let op = BinaryOperator::from(o.slice);
                let (left, mut update) = result.0;
                update.append(&mut u);
                (Node::from(Expr::BinaryExpr {operator: op, left: Box::new(left), right: Box::new(right)}), update)
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
        use std::path::Path;
        use std::fs::File;
        use std::io::Read;

        #[cfg(test)]
        mod subparsers {
            use super::*;

            #[test]
            fn parse_post_index() {
                let e = ParserContext::empty();
                
                check_match_no_update("[a:b:c, :, d]", |x| e.post_index(x), PostIdent::Index{slices: vec!(
                    (Some(Node::from("a")), Some(Node::from("b")), Some(Node::from("c"))),
                    (None, None, None),
                    (Some(Node::from("d")), None, None)
                )});
                simple_check_failed("[a", |x| e.post_index(x));
                simple_check_failed("[a:", |x| e.post_index(x));
            }

            #[test]
            fn parse_atomic() {
                let e = ParserContext::empty();
                check_data_and_leftover("1 ** 2", |x| e.atomic_expr(x), Expr::from(1), "** 2");
                check_data("false", bool_expr, Expr::from(false));
                check_data("false", |x| e.atomic_expr(x), Expr::from(false));
            }

            #[test]
            fn parse_spec_literals() {
                check_match_no_update("123", int_expr, Node::from(123));
                check_failed("e10", float_expr, ErrorKind::Digit);
                check_failed(".e10", float_expr, ErrorKind::Digit);
                check_failed(".0", float_expr, ErrorKind::Digit);
            }

        }

        #[cfg(test)]
        mod failures {
            use super::*;

            /// Failed because NOT wasn't was returning a partial match again "n"
            #[test]
            #[ignore]
            fn failure_2020_02_07() {
                let e = ParserContext::empty();
                check_data("n", |x| e.expression(x), Expr::from("n"))
            }

            /// Failed because + wasn't in VALID_NUM_FOLLOW. Added all binary operations to VALID_NUM_FOLLOW.
            #[test]
            #[ignore]
            fn failure_2019_12_14_1() {
                let input = "0+true    ";
                let e = ParserContext::empty();
                let result = e.additive_expr(PosStr::from(input));
                result.unwrap();
            }

            #[test]
            #[ignore]
            fn failure_2019_12_14_2() {
                let input = "\"\\\"\\\"\"+-10446305       ";
                let e = ParserContext::empty();
                let result = e.expression(PosStr::from(input));
                result.unwrap();
            }

            /// Resolved by preventing -{NUM_START} from being interpreted as a unary operator.
            #[test]
            #[ignore]
            fn failure_2019_12_14_3() {
                let input = "-3       ";
                let e = ParserContext::empty();
                check_data(input, |x| e.expression(x), Expr::Int("-3".to_string()));
            }

            /// Resolved by allowing DIGIT0 to recognize the empty string.
            /// Before "9." would break when checking if a digit sequence occured after the decimal.
            #[test]
            #[ignore]
            fn failure_2019_12_14_4() {
                let input = "0<9.";
                let e = ParserContext::empty();
                check_data(input, |x| e.expression(x), Expr::ComparisonExpr{
                    operator: ComparisonOperator::Less,
                    left: wrap(Expr::from(0)),
                    right: wrap(Expr::Float("9.".to_string()))
                });
            }

            /// Resolved by making a new token to recognize the negative unary operator specifically.
            /// It checks that it's not followed by a digit.
            #[test]
            #[ignore]
            fn failure_2019_12_14_5() {
                let input = "- 0   ";
                let e = ParserContext::empty();
                check_data(input, |x| e.expression(x), Expr::UnaryExpr{
                    operator: UnaryOperator::Negative,
                    operand: wrap(Expr::from(0))
                });
            }
        }

        #[test]
        fn parse_post_ident() {
            let e = ParserContext::empty();
            let expected_args = vec!("a", "b", "c").iter().map(|x| Node::from(*x)).collect();
            check_match_no_update("( a ,  b , c ) ", |x| e.trailer(x), PostIdent::Call{args: expected_args, kwargs: vec![]});
            check_match_no_update("( a   ,  b  =    true)", |x| e.trailer(x), PostIdent::Call {
                args: vec!(Node::from("a")),
                kwargs: vec!((Identifier::from("b"), Node::from(true)))
            });

            check_match_no_update("( a   = true,  b = true ) ", |x| e.trailer(x), PostIdent::Call {
                args: vec![],
                kwargs: vec![(Identifier::from("a"), Node::from(true)), (Identifier::from("b"), Node::from(true))]
            });

            check_match_no_update("()", |x| e.trailer(x), PostIdent::Call {
                args: vec!(),
                kwargs: vec!()
            });

            simple_check_failed("(a | b=false)", |x| e.trailer(x));
            simple_check_failed("(a   b=false)", |x| e.trailer(x));
            simple_check_failed("(a,, b=false)", |x| e.trailer(x));
            simple_check_failed("(a,, b)", |x| e.trailer(x));
            simple_check_failed("(a, b =  true, c)", |x| e.trailer(x));


            check_match_no_update(".asdf_   ", |x| e.trailer(x), PostIdent::Access{attribute: Identifier::from("asdf_")});

            check_match_no_update("[a:b:c, :, d]", |x| e.trailer(x), PostIdent::Index {
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
            let a = output(e.logical_binary_expr(PosStr::from("true and false"))).0;
            let no_args = Node::from(Expr::FunctionCall{
                function: wrap(Expr::from("func")),
                args: vec!(),
                kwargs: vec!()
            });
            check_match_no_update("func()", |x| e.expr_with_trailer(x), no_args);

            let b = output(e.expression(PosStr::from("func()"))).0;
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
                function: Box::new(output(e.logical_binary_expr(PosStr::from("a and b"))).0),
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

                assert_eq!(output(expr).0, expected);
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
                operand: Box::new(output(e.expression(PosStr::from("+y"))).0),
            });
            check_data("not true", |x| e.expression(x), Expr::UnaryExpr {
                operator: UnaryOperator::Not, 
                operand: Box::new(Node::from(true))
            });
        }

        #[test]
        fn parse_comprehensions() {
            let e = ParserContext::empty();

            let set = e.expression(PosStr::from("{x for x in y}"));
            match output(set).0.data {
                Expr::IdentifierExpr(_) => {},
                _ => panic!()
            };
            
            let map = e.expression(PosStr::from("{x:1 for x in y}"));
            match output(map).0.data {
                Expr::IdentifierExpr(_) => {},
                _ => panic!()
            };

            let vec_c = e.expression(PosStr::from("[x for x in y]"));
            match output(vec_c).0.data {
                Expr::IdentifierExpr(_) => {},
                _ => panic!()
            };

            let gen = e.expression(PosStr::from("(x for x in y)"));
            match output(gen).0.data {
                Expr::IdentifierExpr(_) => {},
                _ => panic!()
            };

            let vec_c = e.expression(PosStr::from("[x for x in y for x in y]]"));
            match output(vec_c).0.data {
                Expr::IdentifierExpr(_) => {},
                _ => panic!()
            };

            check_match("for a, b in c if true", |x| e.comprehension_for(x), (
                vec!(Identifier::from("a"), Identifier::from("b")),
                (Node::from("c"), vec!()),
                Some((Node::from(true), vec!()))
            ));
        }

        // #[test]
        // fn parse_match_expr() {
        //     let e = ParserContext::empty();
        //     check_match_no_update("match x:\n5 => 5", |x| e.expression(x), Node::from(Expr::MatchExpr {
        //         value: Box::new(Node::from("x")),
        //         cases: vec![(Node::from(5), Node::from(5))]
        //     }));
        // }

        #[test]
        fn parse_identifier_expr() {
            let e = ParserContext::empty();
            let expected: Node<Expr> = Node::from("words");
            check_match_no_update("words", |x| e.expression(x), expected);

            let expected = Node::from("abc_123");
            check_match_no_update("abc_123", |x| e.expression(x), expected);

            check_failed("(", |x| e.expression(x), ErrorKind::Alt);
        }

        #[test]
        fn parse_literals() {
            let e = ParserContext::empty();
            check_data_and_leftover("2]", |x| e.expression(x), Expr::from(2), "]");

            let int = rand::random::<i64>().abs();
            check_match_no_update(&int.to_string(), int_expr, Node::from(int));
            let rand_float = rand::random::<f64>().abs();
            check_match_no_update(&rand_float.to_string(), float_expr, Node::from(rand_float));
            let expr = Expr::String("asdf\\\"\\\ra\'sdf".to_string());
            check_match_no_update("\"asdf\\\"\\\ra\'sdf\"", |x| e.expression(x), Node::from(expr));

            check_match_no_update("{x : y}", |x| e.expression(x), Node::from(Expr::MapLiteral(vec!((Node::from("x"), Node::from("y"))))));
            check_match_no_update("[true, false]", |x| e.expression(x), Node::from(Expr::VecLiteral(vec!(Node::from(true), Node::from(false)))));
            check_match_no_update("{true, false}", |x| e.expression(x), Node::from(Expr::SetLiteral(vec!(Node::from(true), Node::from(false)))));
            check_match_no_update("(true, false)", |x| e.expression(x), Node::from(Expr::TupleLiteral(vec!(Node::from(true), Node::from(false)))));

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

        #[test]
        fn parse_expr_with_trailer() {
            let e = ParserContext::empty();

            check_data("a()", |x| e.expr_with_trailer(x), Expr::FunctionCall{
                function: wrap(Expr::from("a")),
                args: vec!(),
                kwargs: vec!()
            });

            check_failed("123", |x| e.expr_with_trailer(x), ErrorKind::Alt);
        }

        #[test]
        fn parse_special_chars() {
            let f_path = Path::new("./test_data/special_chars.txt");
            let mut f = File::open(f_path).expect("File not found");
            let mut file_contents = String::new();
            f.read_to_string(&mut file_contents).unwrap();
            check_data(file_contents.as_str(), string_expr, Expr::String("\\\"\\n\'\\\\\\\'".to_string()));
        }

        #[test]
        fn parse_module_access() {
            let mut e = ParserContext::empty();
            let import = Import {
                id: 0,
                path: vec!(Identifier::from("file_2")),
                alias: None,
                values: vec!()
            };
            e.imported.insert(Identifier::from("file_2"), import);
            check_data("file_2.foo", |x| e.expression(x), Expr::ModuleAccess(0, vec!(Identifier::from("file_2"), Identifier::from("foo"))));
        }
    }
}

/// Recognize an integer. No post-processing.
fn just_int<'a>(input: PosStr<'a>) -> IO<'a> {
    return w_followed!(input, 
        recognize!(tuple!(
            optc!(SIGN),
            terminated!(
                DIGIT,
                VALID_NUM_FOLLOW
            )
        )
    ));
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

    fn refinement<'a>(input: PosStr<'a>) {

        panic!()
    }

    fn comparison_refinement<'a>(input: PosStr<'a>) {
        let parse_result = tuple!(input,
            operator_refinement,
            optc!(tuple!(
                alt_complete!( DEQUAL | NEQUAL | LEQUAL | GEQUAL | LANGLE  | RANGLE),
                operator_refinement
            ))
        );

        let map = |x: (ExprU, Option<(PosStr<'a>, ExprU)>)| match x.1 {
            None => x.0,
            Some((o, (right, mut u))) => {
                let operator = ComparisonOperator::from(o.slice);
                (Node::from(Expr::ComparisonExpr{operator, left: Box::new(left), right: Box::new(right)}), update)
            }
        };

        // let node = fmap_iresult(parse_result, map);
        panic!()
        // return node;
    }

    fn operator_refinement<'a>(input: PosStr<'a>) -> IResult<PosStr<'a>, Refinement> {
        let parse_result = tuple!(input,
            refinement_atom,
            optc!(tuple!(
                alt_complete!(PLUS | MINUS | STAR),
                operator_refinement
            ))
        );
        return fmap_iresult(parse_result, |(x, y)| => match y {
            Some((op, t)) => {
                match op {
                    "+" => Refinement::Plus(x, t),
                    _ => panic!()
                }
            },
        });
    }

    fn refinement_atom<'a>(input: PosStr<'a>) -> IResult<PosStr<'a>, Refinement> {
        return just_int(input);
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

/// Get the next hidden variable.
fn next_hidden() -> Identifier {
    return Identifier::from(format!(".{}", get_next_var()));
}

/// Rewrite a for loop as a while loop.
/// 
/// # Arguments
/// 
/// * `loop_var` - The name of the variable that contains the iterator results
/// * `iterator` - The iterator expression
/// * `inner_loop` - The contexts of loop.
fn for_to_while(loop_var: Identifier, iterator: &Node<Expr>, mut inner_loop: StmtSeq) -> (Stmt, StmtSeq) {
   
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


    return (while_loop, outer_stmts);
}

#[cfg(test)]
mod property_based_tests {
    use super::*;
    use proptest::prelude::*;
    use proptest_utils::strategies;

    // Check that literal expressions can parse at all.
    proptest! {
        #[test]
        fn lit_props(v in strategies::literal_strategy()) {
            let expr_string = v.inverse_parse();
            let e = ParserContext::empty();
            let result = e.expression(PosStr::from(expr_string.as_bytes()));
            result.unwrap();
        }
    }

    // Check that exprs can parse at all.
    proptest! {
        #![proptest_config(ProptestConfig {
            cases: 50, .. ProptestConfig::default()
        })]
        #[test]
        fn prop_expr_parse_at_all(v in strategies::expr_strategy()) {
            let expr_string = v.inverse_parse();
            let e = ParserContext::empty();
            let result = e.expression(PosStr::from(expr_string.as_bytes()));
            result.unwrap();
        }
    }

    // Check that turning an expression into a string and back again is the identity.
    proptest! {
        #![proptest_config(ProptestConfig {
            cases: 10, .. ProptestConfig::default()
        })]
        #[test]
        fn prop_expr_identity(v in strategies::expr_strategy()) {
            let expr_string = v.inverse_parse();
            let e = ParserContext::empty();
            check_data(expr_string.as_str(), |x| e.expression(x), v);
        }
    }

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
                Box::new(output(e.statement(PosStr::from("fn a():\n return 0"), 0)).0),
                Box::new(output(e.statement(PosStr::from("fn b():\n return 1"), 0)).0)
            ),
            imports: vec!()
        }))
    }

    #[test]
    fn parse_module_with_import() {
        let e = ParserContext::empty();
        let module_str = "import foo\nfn a() -> i64:\n return 0\n\nfn b() -> i64:\n return 1";
        check_match(module_str, module, Node::from(Module{
            declarations: vec!(
                Box::new(output(e.statement(PosStr::from("fn a() -> i64:\n return 0"), 0)).0),
                Box::new(output(e.statement(PosStr::from("fn b() -> i64:\n return 1"), 0)).0)
            ),
            imports: vec!(Box::new(Import{id: 0, path: vec!(Identifier::from("foo")), alias: None, values: vec!()})),
        }));
        
        // let e = ParserContext::empty();
        let module_str = "import file_2\nfn a() -> i64:\n return file_2.foo()\n";
        let parsed = module(PosStr::from(module_str)).unwrap();
        let import_id = parsed.1.data.imports[0].id;
        assert_eq!(parsed.1, Node::from(Module{
            declarations: vec!(
                wrap(Stmt::FunctionDecStmt{
                    name: Identifier::from("a"), 
                    args: vec!(),
                    kwargs: vec!(),
                    block: Node::from(Block{
                        statements: vec!(wrap(Stmt::ReturnStmt(
                            Node::from(Expr::FunctionCall{
                                function: wrap(Expr::ModuleAccess(import_id, vec!(Identifier::from("file_2"), Identifier::from("foo")))),
                                args: vec!(),
                                kwargs: vec!(),
                            })
                        )))
                    }), 
                    return_type: Type::i64
                })
            ),
            imports: vec!(Box::new(Import{id: 0, path: vec!(Identifier::from("file_2")), alias: None, values: vec!()})),
        }));
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
                wrap(Stmt::assn("x", 0)),
                wrap(Stmt::assn("y", true))
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
