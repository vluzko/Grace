/// All statement parsers.
pub mod stmt_parsers {
    use super::*;

    impl ParserContext {
        /// Match any statement.
        pub fn statement<'a>(&self, input: PosStr<'a>, indent: usize) -> StmtRes<'a> {
            let node = alt_complete!(
                input,
                m!(self.let_stmt)
                    | m!(self.assignment_stmt)
                    | m!(self.while_stmt, indent)
                    | m!(self.for_in, indent)
                    | m!(self.if_stmt, indent)
                    | map!(m!(self.function_declaration_stmt, indent), |x| (x, vec!()))
                    | m!(self.return_stmt)
                    | m!(self.yield_stmt)
                    | break_stmt
                    | pass_stmt
                    | continue_stmt
            );

            return node;
        }

        /// Match a let statement.
        fn let_stmt<'a>(&self, input: PosStr<'a>) -> StmtRes<'a> {
            let parse_result = tuple!(
                input,
                preceded!(LET, IDENTIFIER),
                optc!(preceded!(COLON, type_parser::any_type)),
                preceded!(EQUALS, m!(self.expression))
            );

            return fmap_nodeu(parse_result, |(name, type_annotation, (expression, u))| {
                (
                    Stmt::LetStmt {
                        name,
                        type_annotation,
                        expression,
                    },
                    u,
                )
            });
        }

        /// Match an assignment statement.
        fn assignment_stmt<'a>(&self, input: PosStr<'a>) -> StmtRes<'a> {
            /// Match an assignment operator.
            fn assignments<'a>(input: PosStr<'a>) -> IO<'a> {
                return alt_complete!(
                    input,
                    EQUALS
                        | ADDASN
                        | SUBASN
                        | MULASN
                        | DIVASN
                        | MODASN
                        | EXPASN
                        | RSHASN
                        | LSHASN
                        | BORASN
                        | BANDASN
                        | BXORASN
                );
            }

            let parse_result = tuple!(input, IDENTIFIER, assignments, m!(self.expression));

            return fmap_nodeu(parse_result, |(name, assn, (expr, u))| {
                (
                    Stmt::AssignmentStmt {
                        name: name.clone(),
                        expression: match assn.slice {
                            b"=" => expr,
                            _ => {
                                let subop = &assn.slice[0..assn.slice.len() - 1];
                                Node::from(Expr::BinaryExpr {
                                    operator: BinaryOperator::from(subop),
                                    left: Box::new(Node::from(Expr::IdentifierExpr(name))),
                                    right: Box::new(expr),
                                })
                            }
                        },
                    },
                    u,
                )
            });
        }

        /// Parse a function declaration.
        pub fn function_declaration_stmt<'a>(
            &self,
            input: PosStr<'a>,
            indent: usize,
        ) -> Res<'a, StmtNode> {
            let arg_parser = |i: PosStr<'a>| {
                tuple!(
                    i,
                    IDENTIFIER,
                    preceded!(OPEN_PAREN, m!(self.simple_args)),
                    terminated!(m!(self.keyword_args), CLOSE_PAREN),
                    optc!(preceded!(TARROW, type_parser::any_type))
                )
            };

            let parse_result = line_and_block!(input, self, preceded!(FN, arg_parser), indent);

            return fmap_node(
                parse_result,
                |((name, args, keyword_args, return_type), body)| {
                    let mut res_kwargs = vec![];
                    for (ident, t, (expr, u)) in keyword_args {
                        // No comprehensions or match expressions in keywords.
                        assert_eq!(u.len(), 0);
                        res_kwargs.push((ident, t, expr));
                    }

                    let stmt = Stmt::FunctionDecStmt {
                        name: name,
                        args: args,
                        kwargs: res_kwargs,
                        block: body,
                        return_type: match return_type {
                            Some(x) => x,
                            None => Type::empty,
                        },
                    };

                    return stmt;
                },
            );
        }

        /// Match an if statement.
        fn if_stmt<'a>(&self, input: PosStr<'a>, indent: usize) -> StmtRes<'a> {
            let parse_result = tuple!(
                input,
                line_and_block!(self, preceded!(IF, m!(self.expression)), indent),
                many0c!(indented!(
                    line_and_block!(self, preceded!(ELIF, m!(self.expression)), indent),
                    indent
                )),
                opt!(complete!(indented!(
                    keyword_and_block!(self, ELSE, indent),
                    indent
                )))
            );

            return fmap_nodeu(
                parse_result,
                |(((cond, mut cond_u), block), elifs, mut else_block)| {
                    let mut just_elifs = Vec::with_capacity(elifs.len());

                    for ((c, mut c_u), b) in elifs.into_iter() {
                        cond_u.append(&mut c_u);
                        just_elifs.push((c, b));
                    }

                    for (elif_cond, elif_block) in just_elifs.into_iter().rev() {
                        let sub_if = wrap(Stmt::IfStmt {
                            condition: elif_cond,
                            block: elif_block,
                            else_block: else_block,
                        });
                        else_block = Some(Node::from(Block {
                            statements: vec![sub_if],
                        }));
                    }

                    let stmt = Stmt::IfStmt {
                        condition: cond,
                        block: block,
                        else_block,
                    };
                    return (stmt, cond_u);
                },
            );
        }

        /// Parse a while loop.
        fn while_stmt<'a>(&self, input: PosStr<'a>, indent: usize) -> StmtRes<'a> {
            let parse_result =
                line_and_block!(input, self, preceded!(WHILE, m!(self.expression)), indent);
            return fmap_nodeu(parse_result, |((cond, cu), block)| {
                (
                    Stmt::WhileStmt {
                        condition: cond,
                        block: block,
                    },
                    cu,
                )
            });
        }

        /// Parse a for in loop.
        fn for_in<'a>(&self, input: PosStr<'a>, indent: usize) -> StmtRes<'a> {
            let parse_result = line_and_block!(
                input,
                self,
                tuple!(
                    preceded!(FOR, IDENTIFIER),
                    preceded!(IN, m!(self.expression))
                ),
                indent
            );

            return fmap_nodeu(parse_result, |((iter_var, (iterator, iu)), block)| {
                let (stmt, update) = for_to_while(iter_var, &iterator, block.data.statements);
                (stmt, join(iu, update))
            });
        }

        /// Match a return statement.
        fn return_stmt<'a>(&self, input: PosStr<'a>) -> StmtRes<'a> {
            let parse_result = preceded!(input, RETURN, m!(self.expression));
            return fmap_pass(parse_result, |x| Stmt::ReturnStmt(x));
        }

        /// Match a yield statement.
        fn yield_stmt<'a>(&self, input: PosStr<'a>) -> StmtRes<'a> {
            let parse_result = preceded!(input, YIELD, m!(self.expression));
            return fmap_pass(parse_result, |x| Stmt::YieldStmt(x));
        }

        /// Match all keyword arguments in a function declaration.
        fn keyword_args<'a>(&self, input: PosStr<'a>) -> Res<'a, Vec<(Identifier, Type, ExprU)>> {
            let parse_result = optc!(
                input,
                preceded!(
                    COMMA,
                    separated_list_complete!(
                        COMMA,
                        tuple!(
                            IDENTIFIER,
                            preceded!(COLON, type_parser::any_type),
                            preceded!(EQUALS, m!(self.expression))
                        )
                    )
                )
            );

            return fmap_iresult(parse_result, |x| match x {
                Some(y) => y,
                None => vec![],
            });
        }

        /// Match the standard arguments in a function declaration.
        pub fn simple_args<'a>(&self, input: PosStr<'a>) -> Res<'a, Vec<(Identifier, Type)>> {
            let result = separated_list_complete!(
                input,
                COMMA,
                tuple!(
                    IDENTIFIER,
                    preceded!(
                        COLON,
                        terminated!(m!(self.parse_type), not!(complete!(EQUALS)))
                    )
                )
            );
            return result;
        }
    }

    /// Match a break statement.
    pub fn break_stmt<'a>(input: PosStr<'a>) -> StmtRes {
        let parse_result = BREAK(input);

        return fmap_nodeu(parse_result, |_| (Stmt::BreakStmt, vec![]));
    }

    /// Match a pass statement.
    pub fn pass_stmt<'a>(input: PosStr<'a>) -> StmtRes {
        let parse_result = PASS(input);

        return fmap_nodeu(parse_result, |_| (Stmt::PassStmt, vec![]));
    }

    /// Match a continue statement.
    pub fn continue_stmt<'a>(input: PosStr<'a>) -> StmtRes {
        let parse_result = CONTINUE(input);

        return fmap_nodeu(parse_result, |_| (Stmt::ContinueStmt, vec![]));
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn parse_let_stmt() {
            let e = ParserContext::empty();
            check_data(
                "let x = 3.0",
                |x| e.statement(x, 0),
                Stmt::LetStmt {
                    name: Identifier::from("x"),
                    type_annotation: None,
                    expression: Node::from(Expr::Float("3.0".to_string())),
                },
            );

            check_data(
                "let x: f32 = 3.0",
                |x| e.statement(x, 0),
                Stmt::LetStmt {
                    name: Identifier::from("x"),
                    type_annotation: Some(Type::f32),
                    expression: Node::from(Expr::Float("3.0".to_string())),
                },
            );
        }

        #[test]
        fn parse_assignment_stmt() {
            let e = ParserContext::empty();
            check_data(
                "foo = true",
                |x| e.assignment_stmt(x),
                Stmt::AssignmentStmt {
                    name: Identifier::from("foo"),
                    expression: Node::from(true),
                },
            );

            check_data_and_leftover(
                "x = 0\n",
                |x| e.assignment_stmt(x),
                Stmt::AssignmentStmt {
                    name: Identifier::from("x"),
                    expression: Node::from(0),
                },
                "\n",
            );

            let all_ops = vec![
                "&=", "|=", "^=", "+=", "-=", "*=", "/=", "%=", ">>=", "<<=", "**=",
            ];
            for op in all_ops {
                let input = format!("x {} y", op);
                check_data(
                    input.as_str(),
                    |x| e.assignment_stmt(x),
                    Stmt::AssignmentStmt {
                        name: Identifier::from("x"),
                        expression: Node::from(Expr::BinaryExpr {
                            operator: BinaryOperator::from(&op[0..op.len() - 1]),
                            left: Box::new(Node::from(Expr::from("x"))),
                            right: Box::new(Node::from(Expr::from("y"))),
                        }),
                    },
                );
            }
        }

        #[test]
        fn parse_func_dec_parts() {
            // Args
            let e = ParserContext::empty();
            let actual = output(e.simple_args(PosStr::from("a: i32)")));
            assert_eq!(vec!((Identifier::from("a"), Type::i32)), actual);
            check_match(
                "a: i32, b: i64",
                |x| e.simple_args(x),
                vec![
                    (Identifier::from("a"), Type::i32),
                    (Identifier::from("b"), Type::i64),
                ],
            );

            // Kwargs.
            let expected = vec![
                (Identifier::from("c"), Type::i32, (Node::from(5), vec![])),
                (Identifier::from("d"), Type::i32, (Node::from(7), vec![])),
            ];

            check_match(", c: i32=5, d: i32=7", |x| e.keyword_args(x), expected);
        }

        #[test]
        fn parse_func_dec() {
            let e = ParserContext::empty();
            check_data(
                "fn wvars(a: i32, b: i32, c: i32=5, d: i32 = 7):\n let val = 5",
                |x| e.statement(x, 0),
                Stmt::FunctionDecStmt {
                    name: Identifier::from("wvars"),
                    args: vec![
                        (Identifier::from("a"), Type::i32),
                        (Identifier::from("b"), Type::i32),
                    ],
                    kwargs: vec![
                        (
                            Identifier::from("c"),
                            Type::i32,
                            output(e.expression(PosStr::from("5"))).0,
                        ),
                        (
                            Identifier::from("d"),
                            Type::i32,
                            output(e.expression(PosStr::from("7"))).0,
                        ),
                    ],
                    block: output(e.block(PosStr::from("let val =  5\n"), 0)),
                    return_type: Type::empty,
                },
            );

            check_data(
                "fn wkwargs(a: i32, c: i32=5):\n let val = 5",
                |x| e.statement(x, 0),
                Stmt::FunctionDecStmt {
                    name: Identifier::from("wkwargs"),
                    args: vec![(Identifier::from("a"), Type::i32)],
                    kwargs: vec![(
                        Identifier::from("c"),
                        Type::i32,
                        output(e.expression(PosStr::from("5"))).0,
                    )],
                    block: output(e.block(PosStr::from("let val=5\n"), 0)),
                    return_type: Type::empty,
                },
            );

            check_data(
                "fn a(b: i32) -> i32:\n let x = 5 + 6\n return x\n",
                |x| e.statement(x, 0),
                Stmt::FunctionDecStmt {
                    name: Identifier::from("a"),
                    args: vec![(Identifier::from("b"), Type::i32)],
                    kwargs: vec![],
                    block: output(e.block(PosStr::from("let x = 5 + 6\nreturn x"), 0)),
                    return_type: Type::i32,
                },
            );
        }

        #[test]
        fn parse_if_stmt() {
            let e = ParserContext::empty();
            let good_input = "if (a and b):\n x = true";

            let good_output = Stmt::IfStmt {
                condition: output(e.expression(PosStr::from("a and b"))).0,
                block: Node::from(Block {
                    statements: vec![Box::new(
                        output(e.assignment_stmt(PosStr::from("x = true"))).0,
                    )],
                }),
                else_block: None,
            };

            check_data(good_input, |x| e.statement(x, 0), good_output);

            check_failed(
                "ifa and b:\n x = true",
                |x| e.statement(x, 0),
                ErrorKind::Alt,
            );

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
            check_data(
                "while true:\n x=true",
                |x| e.statement(x, 0),
                Stmt::WhileStmt {
                    condition: Node::from(true),
                    block: Node::from(Block {
                        statements: vec![Box::new(
                            output(e.assignment_stmt(PosStr::from("x=true"))).0,
                        )],
                    }),
                },
            );

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
            check_data_and_leftover(
                "pass  //comment \n  ",
                |x| e.statement(x, 0),
                Stmt::PassStmt,
                "//comment \n  ",
            );
            check_data("continue", |x| e.statement(x, 0), Stmt::ContinueStmt);
            check_data_and_leftover(
                "continue   \n  ",
                |x| e.statement(x, 0),
                Stmt::ContinueStmt,
                "\n  ",
            );
            check_data_and_leftover(
                "continue  //comment  \n  ",
                |x| e.statement(x, 0),
                Stmt::ContinueStmt,
                "//comment  \n  ",
            );
            check_data("break", |x| e.statement(x, 0), Stmt::BreakStmt);
            check_data_and_leftover(
                "break   \n  ",
                |x| e.statement(x, 0),
                Stmt::BreakStmt,
                "\n  ",
            );
            check_data_and_leftover(
                "break  // comment \n  ",
                |x| e.statement(x, 0),
                Stmt::BreakStmt,
                "// comment \n  ",
            );
        }

        #[test]
        fn parse_return_and_yield_stmts() {
            let e = ParserContext::empty();
            check_data(
                "return true",
                |x| e.statement(x, 0),
                Stmt::ReturnStmt(Node::from(true)),
            );
            check_data_and_leftover(
                "return 1  \n  ",
                |x| e.statement(x, 0),
                Stmt::ReturnStmt(Node::from(1)),
                "\n  ",
            );

            check_data(
                "yield true",
                |x| e.statement(x, 0),
                Stmt::YieldStmt(Node::from(true)),
            );
        }
    }
}