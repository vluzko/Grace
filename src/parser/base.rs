//! The parser.
use std::collections::HashMap;

extern crate nom;
use self::nom::*;
use nom::bytes::complete::tag;
use nom::combinator::{map, opt, recognize};
use nom::multi::{many_m_n, separated_list1};
use nom::sequence::{delimited, preceded, terminated};

use crate::expression::*;
use crate::parser::module_parser::module;
use crate::parser::parser_utils::iresult_helpers::*;
use crate::parser::parser_utils::tokens::*;
use crate::parser::parser_utils::*;
use crate::parser::position_tracker::PosStr;

use crate::general_utils::get_next_var;
use crate::type_checking::types::{Trait, Type};

use super::type_parser::any_type;

pub(in crate::parser) type StmtNode = Node<Stmt>;
pub(in crate::parser) type ExprNode = Node<Expr>;
pub(in crate::parser) type IO<'a> = IResult<PosStr<'a>, PosStr<'a>>;
pub(in crate::parser) type Res<'a, T> = IResult<PosStr<'a>, T>;
pub(in crate::parser) type StmtSeq = Vec<Box<Node<Stmt>>>;
pub(in crate::parser) type ExprU = (ExprNode, StmtSeq);
pub(in crate::parser) type StmtU = (StmtNode, StmtSeq);
pub(in crate::parser) type StmtRes<'a> = IResult<PosStr<'a>, StmtU>;
pub(in crate::parser) type ExprRes<'a> = IResult<PosStr<'a>, ExprU>;
pub(in crate::parser) type TypeRes<'a> = IResult<PosStr<'a>, Type>;

pub trait Parseable {
    fn parse(input: PosStr) -> Self;
}

impl Parseable for Node<Module> {
    fn parse(input: PosStr) -> Node<Module> {
        return output(module(input));
    }
}

impl Parseable for Node<Block> {
    fn parse(input: PosStr) -> Node<Block> {
        let e = ParserContext::empty();
        output(e.block(input, 0))
    }
}

impl Parseable for Node<Stmt> {
    fn parse(input: PosStr) -> Node<Stmt> {
        let e = ParserContext::empty();
        output(e.statement(input, 0)).0
    }
}

impl Parseable for Node<Expr> {
    fn parse(input: PosStr) -> Node<Expr> {
        let e = ParserContext::empty();
        output(e.expression(input)).0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserContext {
    /// The imports in the current module
    pub(in crate::parser) imported: HashMap<Identifier, Import>,
    /// Whether or not the current context can use the self variable.
    pub(in crate::parser) can_use_self: bool,
}

impl ParserContext {
    pub fn empty() -> ParserContext {
        ParserContext {
            imported: HashMap::new(),
            can_use_self: false,
        }
    }
}

/// Parser for trait implementations.
impl ParserContext {
    /// Parse a trait declaration.
    /// trait NameOfTrait:
    ///     fn method_name: (arg1: type1, ...) -> return_type
    ///     ...
    pub(in crate::parser) fn trait_parser<'a>(&self, input: PosStr<'a>) -> Res<'a, Trait> {
        let header = delimited(TRAIT, IDENTIFIER, (COLON, between_statement)).parse(input);

        let body_parser = |i: PosStr<'a>| {
            let (i, indent) = map(|i| many1c!(i, inline_whitespace_char), |x| x.len()).parse(i)?;
            let (i, statements) = separated_list1(
                (between_statement, many_m_n(indent, indent, tag(" "))),
                |i| self.trait_method(i),
            )
            .parse(i)?;
            Ok((i, statements))
        };

        let full_res = chain(header, body_parser);

        let trait_val = fmap_iresult(full_res, |(name, signatures)| {
            let mut m = HashMap::new();

            for (k, v) in signatures {
                m.insert(k, v);
            }
            Trait { name, functions: m }
        });

        trait_val
    }

    /// Parse a single function description in a trait.
    /// fn method_name: (arg1: type1, ... -> return_type)
    fn trait_method<'a>(&self, input: PosStr<'a>) -> Res<'a, (Identifier, Type)> {
        let parse_result = (
            delimited(FN, IDENTIFIER, COLON),
            delimited(OPEN_PAREN, |i| self.simple_args(i), CLOSE_PAREN),
            preceded(TARROW, any_type),
        )
            .parse(input);

        fmap_iresult(parse_result, |(name, args, ret)| {
            (name, Type::Function(args, vec![], Box::new(ret)))
        })
    }

    pub(in crate::parser) fn trait_impl<'a>(
        &self,
        input: PosStr<'a>,
    ) -> Res<'a, (Identifier, Identifier, Vec<Node<Stmt>>)> {
        let header = (
            preceded(IMPL, IDENTIFIER),
            delimited(FOR, IDENTIFIER, terminated(COLON, between_statement)),
        )
            .parse(input);
        let body_parser = |i: PosStr<'a>| {
            let (i, indent) = map(|i| many1c!(i, inline_whitespace_char), |x| x.len()).parse(i)?;
            let (i, declarations) = separated_list1(
                (between_statement, many_m_n(indent, indent, tag(" "))),
                |i| self.function_declaration_stmt(i, indent),
            )
            .parse(i)?;
            Ok((i, declarations))
        };

        let full_res = chain(header, body_parser);
        fmap_iresult(full_res, |((trait_name, struct_name), declarations)| {
            (trait_name, struct_name, declarations)
        })
    }
}

/// Parser for struct declarations.
impl ParserContext {
    /// Parse a struct declaration.
    pub fn struct_declaration_stmt<'a>(&self, input: PosStr<'a>) -> Res<'a, StmtNode> {
        let header =
            delimited(STRUCT, IDENTIFIER, terminated(COLON, between_statement)).parse(input);

        let field_parser =
            |i: PosStr<'a>| (IDENTIFIER, preceded(COLON, |i| self.parse_type(i))).parse(i);

        let body_parser = |i: PosStr<'a>| {
            let (i, indent) = map(|i| many1c!(i, inline_whitespace_char), |x| x.len()).parse(i)?;
            let (i, fields) = separated_list1(
                (between_statement, many_m_n(indent, indent, tag(" "))),
                field_parser,
            )
            .parse(i)?;
            Ok((i, fields))
        };

        let full_res = chain(header, body_parser);
        let struct_dec = fmap_node(
            full_res,
            |(name, fields)| Stmt::StructDec { name, fields },
            &(input.line, input.column),
        );

        struct_dec
    }
}

/// Recognize an integer. No post-processing.
pub(in crate::parser) fn just_int(input: PosStr) -> IO {
    w_followed!(
        input,
        recognize((opt(SIGN), terminated(DIGIT, VALID_NUM_FOLLOW)))
    )
}

// /// Type parsers

/// Get the next hidden variable.
pub(in crate::parser) fn next_hidden() -> Identifier {
    Identifier::from(format!(".{}", get_next_var()))
}

/// Rewrite a for loop as a while loop.
///
/// # Arguments
///
/// * `loop_var` - The name of the variable that contains the iterator results
/// * `iterator` - The iterator expression
/// * `inner_loop` - The contexts of loop.
pub(in crate::parser) fn for_to_while(
    loop_var: Identifier,
    iterator: &Node<Expr>,
    mut inner_loop: StmtSeq,
) -> (Stmt, StmtSeq) {
    // The contents of the loop.

    let mut outer_stmts = vec![];

    // Expression to create the iterator.
    let iter_call = Node::from(Expr::FunctionCall {
        function: wrap(Expr::AttributeAccess {
            base: Box::new(iterator.clone()),
            attribute: Identifier::from("iter"),
        }),
        args: vec![],
        kwargs: vec![],
    });

    // The name of the iterator
    let loop_iter_name = Identifier::from(format!(".{}", get_next_var()));
    // The name of the iterator values.
    let loop_var_name = Identifier::from(format!(".{}", get_next_var()));

    // Statement to store the iterator
    let loop_iter = Stmt::LetStmt {
        name: loop_iter_name.clone(),
        type_annotation: Some(Type::Undetermined),
        expression: iter_call,
    };
    outer_stmts.push(wrap(loop_iter));

    // The next value of the iterator
    let iter_next = Stmt::LetStmt {
        name: loop_var_name.clone(),
        type_annotation: Some(Type::Undetermined),
        expression: Node::from(Expr::FunctionCall {
            function: wrap(Expr::AttributeAccess {
                base: wrap(Expr::from(loop_iter_name)),
                attribute: Identifier::from("next"),
            }),
            args: vec![],
            kwargs: vec![],
        }),
    };
    // Add the first iteration outside the loop.
    outer_stmts.push(wrap(iter_next.clone()));
    // Call next at the end of every loop.
    inner_loop.push(wrap(iter_next));

    // The contents of the first value.
    let iter_var = Stmt::LetStmt {
        name: loop_var,
        type_annotation: Some(Type::Undetermined),
        expression: Node::from(Expr::Index {
            base: wrap(Expr::from(loop_var_name.clone())),
            slices: vec![(Some(Node::from(Expr::from(0))), None, None)],
        }),
    };
    outer_stmts.push(wrap(iter_var));

    let iter_state = Node::from(Expr::Index {
        base: wrap(Expr::from(loop_var_name)),
        slices: vec![(Some(Node::from(Expr::from(1))), None, None)],
    });

    let while_loop = Stmt::WhileStmt {
        condition: iter_state,
        block: Node::from(Block {
            statements: inner_loop,
        }),
    };

    (while_loop, outer_stmts)
}

#[cfg(test)]
mod property_based_tests {
    use super::*;
    use crate::testing::proptest_utils::strategies;
    use proptest::prelude::*;

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
        check_match(
            module_str,
            module,
            Node::from(Module {
                functions: vec![
                    Box::new(output(e.statement(PosStr::from("fn a():\n return 0"), 0)).0),
                    Box::new(output(e.statement(PosStr::from("fn b():\n return 1"), 0)).0),
                ],
                structs: vec![],
                imports: vec![],
                traits: HashMap::new(),
                trait_implementations: vec![],
            }),
        )
    }

    #[test]
    fn parse_module_with_import() {
        let e = ParserContext::empty();
        let module_str = "import foo\nfn a() -> i64:\n return 0\n\nfn b() -> i64:\n return 1";
        check_match(
            module_str,
            module,
            Node::from(Module {
                functions: vec![
                    Box::new(output(e.statement(PosStr::from("fn a() -> i64:\n return 0"), 0)).0),
                    Box::new(output(e.statement(PosStr::from("fn b() -> i64:\n return 1"), 0)).0),
                ],
                structs: vec![],
                imports: vec![Box::new(Import {
                    id: 0,
                    path: vec![Identifier::from("foo")],
                    alias: None,
                    values: vec![],
                })],
                traits: HashMap::new(),
                trait_implementations: vec![],
            }),
        );

        // let e = ParserContext::empty();
        let module_str = "import file_2\nfn a() -> i64:\n return file_2.foo()\n";
        let parsed = module(PosStr::from(module_str)).unwrap();
        let import_id = parsed.1.data.imports[0].id;
        assert_eq!(
            parsed.1,
            Node::from(Module {
                functions: vec!(wrap(Stmt::FunctionDecStmt {
                    name: Identifier::from("a"),
                    args: vec!(),
                    kwargs: vec!(),
                    block: Node::from(Block {
                        statements: vec!(wrap(Stmt::ReturnStmt(Node::from(Expr::FunctionCall {
                            function: wrap(Expr::ModuleAccess(
                                import_id,
                                vec!(Identifier::from("file_2"), Identifier::from("foo"))
                            )),
                            args: vec!(),
                            kwargs: vec!(),
                        }))))
                    }),
                    return_type: Type::i64
                })),
                structs: vec!(),
                imports: vec!(Box::new(Import {
                    id: 0,
                    path: vec!(Identifier::from("file_2")),
                    alias: None,
                    values: vec!()
                })),
                traits: HashMap::new(),
                trait_implementations: vec!()
            })
        );
    }

    #[test]
    fn parse_block() {
        let e = ParserContext::empty();
        let exp_block = Block {
            statements: vec![wrap(Stmt::assn("x", 0)), wrap(Stmt::assn("y", true))],
        };

        check_match(
            " x=0\n y=true\n\n  \n",
            |x| e.block(x, 1),
            Node::from(exp_block),
        );
    }

    #[test]
    fn parse_struct_dec() {
        let input = "struct A:\n  a: i64\n  b: i32";
        let e = ParserContext::empty();
        let expected = Stmt::StructDec {
            name: Identifier::from("A"),
            fields: vec![
                (Identifier::from("a"), Type::i64),
                (Identifier::from("b"), Type::i32),
            ],
        };
        check_data_no_update(input, |x| e.struct_declaration_stmt(x), expected);

        // Test 2
        let input = "struct A:  \n   \n\n  x: i32\n  y: i32";
        let e = ParserContext::empty();
        check_match(
            input,
            |x| e.struct_declaration_stmt(x),
            Node::from(Stmt::StructDec {
                name: Identifier::from("A"),
                fields: vec![
                    (Identifier::from("x"), Type::i32),
                    (Identifier::from("y"), Type::i32),
                ],
            }),
        );
    }

    #[test]
    fn parse_trait_dec() {
        let mut m = HashMap::new();

        m.insert(
            Identifier::from("ident1"),
            Type::Function(
                vec![(Identifier::from("a"), Type::i32)],
                vec![],
                Box::new(Type::i32),
            ),
        );
        m.insert(
            Identifier::from("ident2"),
            Type::Function(
                vec![(Identifier::from("b"), Type::i64)],
                vec![],
                Box::new(Type::i64),
            ),
        );
        let e = ParserContext::empty();
        check_match(
            "trait Trait:\n    fn ident1: (a: i32) -> i32\n    fn ident2: (b: i64) ->i64",
            |x| e.trait_parser(x),
            Trait {
                name: Identifier::from("Trait"),
                functions: m,
            },
        )
    }

    #[test]
    fn parse_trait_impl() {
        let mut m = HashMap::new();

        m.insert(Identifier::from("ident1"), Type::i32);
        m.insert(Identifier::from("ident2"), Type::i32);
        m.insert(Identifier::from("ident3"), Type::i32);
        let e = ParserContext::empty();

        let tiny_function = Node::<Stmt>::parse(PosStr::from("fn x() -> i32:\n    return 0"));

        check_match(
            "impl Foo for Baz:\n    fn x() -> i32:\n        return 0",
            |x| e.trait_impl(x),
            (
                Identifier::from("Foo"),
                Identifier::from("Baz"),
                vec![tiny_function],
            ),
        )
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
