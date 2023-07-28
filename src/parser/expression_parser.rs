//! All expression parsers.
extern crate nom;
use self::nom::*;
use expression::*;
use parser::base::{
    for_to_while, just_int, next_hidden, ExprNode, ExprRes, ExprU, ParserContext, Res, StmtSeq,
};
use parser::parser_utils::iresult_helpers::*;
use parser::parser_utils::tokens::*;
use parser::parser_utils::*;
use parser::position_tracker::PosStr;
use std::str::from_utf8;

/// Top-level expression and some extras.
impl ParserContext {
    pub fn expression<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        alt_complete!(input, m!(self.comparison_expr))
    }

    /// Match any unary expression.
    /// Implemented as a single parser because all unary expressions have the same precedence.
    fn unary_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        let (i, (maybe_op, (expr, u))) = alt!(
            input,
            tuple!(
                map!(alt_complete!(PLUS | NEG | TILDE | NOT), Some),
                m!(self.unary_expr)
            ) | tuple!(value!(None, tag!("")), m!(self.power_expr))
        )?;

        let node_update = match maybe_op {
            Some(op_str) => (
                Node::from((
                    Expr::UnaryExpr {
                        operator: UnaryOperator::from(op_str),
                        operand: Box::new(expr),
                    },
                    input.line,
                    input.column,
                    i.line,
                    i.column,
                )),
                u,
            ),
            None => (expr, u),
        };

        Ok((i, node_update))
    }
}

/// Binary expressions
impl ParserContext {
    /// Match a comparison expression.
    fn comparison_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        let parse_result = tuple!(
            input,
            m!(self.logical_binary_expr),
            optc!(tuple!(
                alt_complete!(DEQUAL | NEQUAL | LEQUAL | GEQUAL | LANGLE | RANGLE),
                m!(self.logical_binary_expr)
            ))
        );

        let node = fmap_iresult(parse_result, flatten_binary);
        node
    }

    #[allow(clippy::only_used_in_recursion)]
    /// Match a list of binary operations
    fn binary_expr<'a>(
        &self,
        input: PosStr<'a>,
        operator_parser: impl Fn(PosStr) -> IResult<PosStr, PosStr>,
        next_expr: impl Fn(PosStr) -> ExprRes,
    ) -> ExprRes<'a> {
        let parse_result = tuple!(
            input,
            next_expr,
            optc!(tuple!(
                operator_parser,
                m!(self.binary_expr, operator_parser, next_expr)
            ))
        );

        fmap_iresult(parse_result, flatten_binary)
    }

    /// Match logical expressions.
    /// Must be public because it's used by several statements
    pub fn logical_binary_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        self.binary_expr(
            input,
            |x| alt_complete!(x, AND | OR | XOR),
            |x| self.bitwise_binary_expr(x),
        )
    }

    /// Match bitwise boolean expressions.
    fn bitwise_binary_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        self.binary_expr(
            input,
            |x| alt_complete!(x, BAND | VBAR | BXOR),
            |x| self.shift_expr(x),
        )
    }

    /// Match bit shift expressions.
    fn shift_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        self.binary_expr(
            input,
            |x| alt_complete!(x, LSHIFT | RSHIFT),
            |x| self.additive_expr(x),
        )
    }

    /// Match addition and subtraction expressions.
    fn additive_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        self.binary_expr(
            input,
            |x| alt_complete!(x, PLUS | MINUS),
            |x| self.mult_expr(x),
        )
    }

    /// Match multiplication, division, and modulo expressions.
    fn mult_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        self.binary_expr(
            input,
            |x| alt_complete!(x, STAR | DIV | MOD),
            |x| self.unary_expr(x),
        )
    }

    /// Match an exponentiation expression.
    fn power_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        self.binary_expr(input, |x| call!(x, EXP), |x| self.atomic_expr(x))
    }
}

/// Atomic expressions
impl ParserContext {
    fn atomic_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        w_followed!(
            input,
            alt_complete!(
                tuple!(bool_expr, value!(vec!()))
                    | tuple!(float_expr, value!(vec!()))
                    | tuple!(int_expr, value!(vec!()))
                    | tuple!(string_expr, value!(vec!()))
                    | delimited!(
                        OPEN_BRACE,
                        alt_complete!(
                            m!(self.map_or_set_comprehension)
                                | m!(self.map_literal)
                                | m!(self.set_literal)
                        ),
                        CLOSE_BRACE
                    )
                    | delimited!(
                        OPEN_BRACKET,
                        alt_complete!(m!(self.vector_comprehension) | m!(self.vec_literal)),
                        CLOSE_BRACKET
                    )
                    | m!(self.expr_with_trailer)
            )
        )
    }

    /// An expression wrapped in parentheses.
    fn wrapped_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        delimited!(
            input,
            OPEN_PAREN,
            alt_complete!(
                m!(self.generator_comprehension) | m!(self.tuple_literal) | m!(self.expression)
            ),
            CLOSE_PAREN
        )
    }

    /// Match a list of arguments in a function call.
    fn args_list<'a>(&self, input: PosStr<'a>) -> Res<'a, Vec<ExprU>> {
        separated_nonempty_list_complete!(
            input,
            COMMA,
            terminated!(m!(self.logical_binary_expr), not!(EQUALS))
        )
    }

    /// Match a list of keyword arguments in a function call.
    fn kwargs_list<'a>(&self, input: PosStr<'a>) -> Res<'a, Vec<(Identifier, ExprU)>> {
        separated_list!(
            input,
            COMMA,
            tuple!(IDENTIFIER, preceded!(EQUALS, m!(self.logical_binary_expr)))
        )
    }

    /// A struct literal
    fn struct_expr<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        let (i, o) = tuple!(
            input,
            separated_nonempty_list_complete!(DOT, IDENTIFIER),
            delimited!(OPEN_BRACE, m!(self.args_list), CLOSE_BRACE)
        )?;

        let map = |(idents, au): (Vec<Identifier>, Vec<ExprU>)| {
            let mut tree_base = Expr::IdentifierExpr(idents.get(0).unwrap().clone());
            let rewritten = if idents.len() > 1 {
                for attribute in idents[1..idents.len() - 1].iter() {
                    tree_base = Expr::AttributeAccess {
                        base: Box::new(Node::from((
                            tree_base,
                            input.line,
                            input.column,
                            i.line,
                            i.column,
                        ))),
                        attribute: attribute.clone(),
                    };
                }
                self.rewrite_access(tree_base, idents.last().unwrap().clone())
            } else {
                tree_base
            };

            let mut args = vec![];
            let mut update = vec![];
            for (val, mut u) in au {
                args.push(val);
                update.append(&mut u);
            }
            (
                Expr::StructLiteral {
                    base: Box::new(Node::from((
                        rewritten,
                        input.line,
                        input.column,
                        i.line,
                        i.column,
                    ))),
                    fields: args,
                },
                update,
            )
        };
        fmap_nodeu(Ok((i, o)), map, &(input.line, input.column))
    }

    /// An expression that can be followed by an arbitrary number of function calls, attribute accesses, or indices.
    fn expr_with_trailer<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        let ident_as_expr = |x| {
            fmap_nodeu(
                IDENTIFIER(x),
                |y: Identifier| (Expr::IdentifierExpr(y), vec![]),
                &(input.line, input.column),
            )
        };

        let parse_result = alt_complete!(
            input,
            tuple!(m!(self.struct_expr), value!(vec!()))
                | tuple!(
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
                    PostIdent::Call { args, kwargs } => Expr::FunctionCall {
                        function: wrap(tree_base),
                        args,
                        kwargs,
                    },
                    PostIdent::Index { slices } => Expr::Index {
                        base: wrap(tree_base),
                        slices,
                    },
                    PostIdent::Access { attribute } => self.rewrite_access(tree_base, attribute),
                };
            }
            (tree_base, update)
        };

        fmap_nodeu(parse_result, map, &(input.line, input.column))
    }

    /// Parse an expression trailer.
    fn trailer<'a>(&self, input: PosStr<'a>) -> Res<'a, (PostIdent, StmtSeq)> {
        alt_complete!(
            input,
            m!(self.post_call) | post_access | m!(self.post_index)
        )
    }

    /// Match a function call following an expression.
    fn post_call<'a>(&self, input: PosStr<'a>) -> Res<'a, (PostIdent, StmtSeq)> {
        let parse_result = delimited!(
            input,
            OPEN_PAREN,
            alt_complete!(
                tuple!(
                    m!(self.args_list),
                    opt!(preceded!(COMMA, m!(self.kwargs_list)))
                ) | map!(m!(self.kwargs_list), |x| (vec!(), Some(x)))
                    | map!(peek!(CLOSE_PAREN), |_x| (vec!(), None))
            ),
            CLOSE_PAREN
        );
        fmap_iresult(parse_result, |(args, kwargs)| {
            let mut just_args = vec![];
            let mut update = vec![];
            for (arg, mut u) in args {
                just_args.push(arg);
                update.append(&mut u);
            }

            let just_kwargs = match kwargs {
                Some(x) => {
                    let mut t = vec![];
                    for (ident, (expr, mut u)) in x {
                        t.push((ident, expr));
                        update.append(&mut u);
                    }
                    t
                }
                None => vec![],
            };

            (
                PostIdent::Call {
                    args: just_args,
                    kwargs: just_kwargs,
                },
                update,
            )
        })
    }

    /// Match an indexing operation following an expression.
    fn post_index<'a>(&self, input: PosStr<'a>) -> Res<'a, (PostIdent, StmtSeq)> {
        let parse_result = delimited!(
            input,
            OPEN_BRACKET,
            separated_nonempty_list_complete!(
                COMMA,
                tuple!(
                    optc!(m!(self.logical_binary_expr)),
                    map!(
                        optc!(preceded!(COLON, optc!(m!(self.logical_binary_expr)))),
                        |x| match x {
                            Some(y) => y,
                            None => None,
                        }
                    ),
                    map!(
                        optc!(preceded!(COLON, optc!(m!(self.logical_binary_expr)))),
                        |x| match x {
                            Some(y) => y,
                            None => None,
                        }
                    )
                )
            ),
            CLOSE_BRACKET
        );

        fmap_iresult(parse_result, |x| {
            let mut indices = vec![];
            let mut update = vec![];

            for (opt1, opt2, opt3) in x {
                let i1 = match opt1 {
                    Some((x, mut u)) => {
                        update.append(&mut u);
                        Some(x)
                    }
                    None => None,
                };

                let i2 = match opt2 {
                    Some((x, mut u)) => {
                        update.append(&mut u);
                        Some(x)
                    }
                    None => None,
                };

                let i3 = match opt3 {
                    Some((x, mut u)) => {
                        update.append(&mut u);
                        Some(x)
                    }
                    None => None,
                };

                indices.push((i1, i2, i3));
            }

            (PostIdent::Index { slices: indices }, update)
        })
    }

    /// Rewrite an AttributeAccess as a ModuleAccess if necessary
    /// Will rewrite if the base expression is an identifier in the imports set, or if it's a ModuleExpression.
    fn rewrite_access(&self, base: Expr, attribute: Identifier) -> Expr {
        return match base {
            Expr::ModuleAccess(id, mut v) => {
                v.push(attribute);
                Expr::ModuleAccess(id, v)
            }
            Expr::IdentifierExpr(name) => match self.imported.get(&name) {
                Some(import) => Expr::ModuleAccess(import.id, vec![name, attribute]),
                None => Expr::AttributeAccess {
                    base: wrap(Expr::IdentifierExpr(name)),
                    attribute,
                },
            },
            x => Expr::AttributeAccess {
                base: wrap(x),
                attribute,
            },
        };
    }
}

/// Collection literals
impl ParserContext {
    /// Match a vector literal.
    fn vec_literal<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        let parse_result = terminated!(
            input,
            separated_nonempty_list_complete!(COMMA, m!(self.logical_binary_expr)),
            peek!(CLOSE_BRACKET)
        );

        return fmap_nodeu(
            parse_result,
            |x| {
                let mut exprs = vec![];
                let mut updates = vec![];
                for (expr, mut update) in x {
                    exprs.push(expr);
                    updates.append(&mut update);
                }
                (Expr::VecLiteral(exprs), updates)
            },
            &(input.line, input.column),
        );
    }

    /// Match a set literal.
    fn set_literal<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        let parse_result =
            separated_nonempty_list_complete!(input, COMMA, m!(self.logical_binary_expr));

        fmap_nodeu(
            parse_result,
            |x| {
                let mut exprs = vec![];
                let mut updates = vec![];
                for (expr, mut update) in x {
                    exprs.push(expr);
                    updates.append(&mut update);
                }
                panic!("Not implemented")
                // (Expr::SetLiteral(exprs), updates)
            },
            &(input.line, input.column),
        )
    }

    /// Match a map literal.
    fn map_literal<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        let parse_result = separated_nonempty_list_complete!(
            input,
            COMMA,
            separated_pair!(
                m!(self.logical_binary_expr),
                COLON,
                m!(self.logical_binary_expr)
            )
        );

        fmap_nodeu(
            parse_result,
            |x| {
                let mut mappings = vec![];
                let mut updates = vec![];

                for ((key, mut ku), (value, mut vu)) in x {
                    mappings.push((key, value));
                    updates.append(&mut ku);
                    updates.append(&mut vu);
                }
                panic!("Not implemented")
                // (Expr::MapLiteral(mappings), updates)
            },
            &(input.line, input.column),
        )
    }

    /// Match a tuple literal
    /// e.g. (), (1, ), (1,2,3), (1,2,3,)
    fn tuple_literal<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        let parse_result = alt_complete!(
            input,
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

        fmap_nodeu(
            parse_result,
            |x| {
                let mut exprs = vec![];
                let mut updates = vec![];
                for (expr, mut update) in x {
                    exprs.push(expr);
                    updates.append(&mut update);
                }
                (Expr::TupleLiteral(exprs), updates)
            },
            &(input.line, input.column),
        )
    }
}

/// Rewrite a comprehension into an expression and a for loop.
///
/// # Arguments
/// * `inputs`      - The input that created the comprehension.
/// * `leftover`    - The leftover input after the comprehension match.
/// * `iterators`   - The iterators in each for clause of the comprehension (in order)
/// * `coll_create` - The statement to create the collection.
/// * `add_value`   - The statement to add a value to the collection.
fn rewrite_comprehension(
    input: &PosStr,
    leftover: &PosStr,
    iterators: Vec<(Vec<Identifier>, ExprU, Option<ExprU>)>,
    coll_create: Stmt,
    add_value: Stmt,
) -> ExprU {
    let coll_name = next_hidden();
    // The statement to create the vector.

    let mut outer_stmts = vec![wrap(add_value)];

    for (iter_vars, (iterator, mut iter_u), if_clause) in iterators.into_iter().rev() {
        // The contents of the loop.
        let (inner_stmts, new_outer) = match if_clause {
            None => (outer_stmts, vec![]),
            Some((cond, u)) => {
                let if_stmt = Stmt::simple_if(
                    cond,
                    Block {
                        statements: outer_stmts,
                    },
                );
                let new_stmts = vec![wrap(if_stmt)];
                (new_stmts, u)
            }
        };

        outer_stmts = new_outer;

        outer_stmts.append(&mut iter_u);

        let mut rewritten = for_to_while(iter_vars.get(0).unwrap().clone(), &iterator, inner_stmts);

        outer_stmts.append(&mut rewritten.1);
        outer_stmts.push(wrap(rewritten.0));
    }

    outer_stmts.insert(0, wrap(coll_create));
    let new_expr = Expr::IdentifierExpr(coll_name);

    (
        Node::from((
            new_expr,
            input.line,
            input.column,
            leftover.line,
            leftover.column,
        )),
        outer_stmts,
    )
}

/// Comprehensions
impl ParserContext {
    /// Match the for part of a comprehension.
    fn comprehension_for<'a>(
        &self,
        input: PosStr<'a>,
    ) -> Res<'a, (Vec<Identifier>, ExprU, Option<ExprU>)> {
        let parse_result = tuple!(
            input,
            delimited!(FOR, variable_unpacking, IN),
            m!(self.logical_binary_expr),
            optc!(preceded!(IF, m!(self.logical_binary_expr)))
        );

        parse_result
    }

    /// Match a vector comprehension.
    fn vector_comprehension<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        let (i, o) = tuple!(
            input,
            m!(self.logical_binary_expr),
            many1!(m!(self.comprehension_for))
        )?;

        return fmap_iresult(Ok((i, o)), |((value, mut v_update), iterators)| {
            // The internal name for the collection.
            let coll_name = next_hidden();
            // The statement to create the vector.
            let coll_create = coll_name.simple_let(Expr::from("vec").call());

            // The statement to push the next element onto the vector.
            let push = coll_name.assn(coll_name.as_expr().access(&"push").callw(vec![value]));
            let (ref_expr, mut rewritten) =
                rewrite_comprehension(&input, &i, iterators, coll_create, push);
            v_update.append(&mut rewritten);

            (ref_expr, v_update)
        });
    }

    /// Match a generator comprehension.
    fn generator_comprehension<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        let (i, o) = tuple!(
            input,
            m!(self.logical_binary_expr),
            many1!(m!(self.comprehension_for))
        )?;

        return fmap_iresult(Ok((i, o)), |((value, mut v_update), iterators)| {
            // The internal name for the collection.
            let coll_name = next_hidden();
            // The statement to create the vector.
            let coll_create = coll_name.simple_let(Expr::from("gen").call());
            // The statement to push the next element onto the vector.
            let push = coll_name.assn(coll_name.as_expr().access(&"push").callw(vec![value]));
            let (ref_expr, mut rewritten) =
                rewrite_comprehension(&input, &i, iterators, coll_create, push);
            v_update.append(&mut rewritten);

            (ref_expr, v_update)
        });
    }

    /// Match a map or a set.
    fn map_or_set_comprehension<'a>(&self, input: PosStr<'a>) -> ExprRes<'a> {
        let (i, o) = tuple!(
            input,
            m!(self.logical_binary_expr),
            opt!(complete!(preceded!(COLON, m!(self.logical_binary_expr)))),
            many1!(m!(self.comprehension_for))
        )?;

        return fmap_iresult(
            Ok((i, o)),
            |((key_or_value, mut kv_update), opt_value, iterators)| {
                let coll_name = next_hidden();
                let (create, add) = match opt_value {
                    Some((value, mut v_update)) => {
                        let create = coll_name.simple_let(Expr::from("map").call());
                        let add = coll_name.assn(
                            coll_name
                                .as_expr()
                                .access(&"add")
                                .callw(vec![key_or_value, value]),
                        );
                        kv_update.append(&mut v_update);
                        (create, add)
                    }
                    None => {
                        let create = coll_name.simple_let(Expr::from("set").call());
                        let add = coll_name
                            .assn(coll_name.as_expr().access(&"add").callw(vec![key_or_value]));
                        (create, add)
                    }
                };

                let (ref_expr, mut rewritten) =
                    rewrite_comprehension(&input, &i, iterators, create, add);
                kv_update.append(&mut rewritten);

                (ref_expr, kv_update)
            },
        );
    }
}

/// A helper Enum for trailers.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PostIdent {
    Call {
        args: Vec<Node<Expr>>,
        kwargs: Vec<(Identifier, Node<Expr>)>,
    },
    Index {
        #[allow(clippy::type_complexity)]
        slices: Vec<(Option<Node<Expr>>, Option<Node<Expr>>, Option<Node<Expr>>)>,
    },
    Access {
        attribute: Identifier,
    },
}

/// Match an access operation following an expression.
fn post_access(input: PosStr) -> IResult<PosStr, (PostIdent, StmtSeq)> {
    let result = preceded!(input, DOT, IDENTIFIER);
    fmap_iresult(result, |x| (PostIdent::Access { attribute: x }, vec![]))
}

// BEGIN SIMPLE LITERALS

pub(super) fn bool_expr(input: PosStr) -> IResult<PosStr, ExprNode> {
    let parse_result = w_followed!(
        input,
        alt!(
            terminated!(tag!("true"), peek!(not!(IDENT_CHAR)))
                | terminated!(tag!("false"), peek!(not!(IDENT_CHAR)))
        )
    );
    fmap_node(
        parse_result,
        |x| {
            Expr::Bool(match from_utf8(x.slice).unwrap() {
                "true" => true,
                "false" => false,
                _ => panic!(),
            })
        },
        &(input.line, input.column),
    )
}

/// Match an integer literal expression.
pub(super) fn int_expr(input: PosStr) -> IResult<PosStr, ExprNode> {
    let parse_result = just_int(input);
    fmap_node(
        parse_result,
        |x| Expr::Int(from_utf8(x.slice).unwrap().to_string()),
        &(input.line, input.column),
    )
}

/// Match a floating point literal expression.
pub(super) fn float_expr<'a>(input: PosStr<'a>) -> IResult<PosStr<'a>, ExprNode> {
    let exponent =
        |x: PosStr<'a>| preceded!(x, alt!(tag!("e") | tag!("E")), tuple!(opt!(SIGN), DIGIT));

    let with_dec = |x: PosStr<'a>| tuple!(x, tag!("."), DIGIT0, opt!(complete!(exponent)));

    let parse_result = w_followed!(
        input,
        recognize!(tuple!(
            opt!(SIGN),
            DIGIT,
            alt!(value!((), with_dec) | value!((), complete!(exponent))),
            VALID_NUM_FOLLOW
        ))
    );

    return fmap_node(
        parse_result,
        |x| Expr::Float(from_utf8(x.slice).unwrap().to_string()),
        &(input.line, input.column),
    );
}

/// Match a string literal expression.
fn string_expr(input: PosStr) -> IResult<PosStr, ExprNode> {
    let result = w_followed!(
        input,
        delimited!(tag!("\""), recognize!(many0c!(STRING_CHAR)), tag!("\""))
    );
    return fmap_node(
        result,
        |x| Expr::String(from_utf8(x.slice).unwrap().to_string()),
        &(input.line, input.column),
    );
}

// END SIMPLE LITERALS

/// Flatten a possible binary expression into a single expression.
fn flatten_binary(result: (ExprU, Option<(PosStr, ExprU)>)) -> ExprU {
    match result.1 {
        Some((o, (right, mut u))) => {
            let op = BinaryOperator::from(o.slice);
            let (left, mut update) = result.0;
            let (left_line, left_col) = (left.start_line, left.start_col);
            let (right_line, right_col) = (right.start_line, right.start_col);
            update.append(&mut u);
            (
                Node::from((
                    Expr::BinaryExpr {
                        operator: op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    left_line,
                    left_col,
                    right_line,
                    right_col,
                )),
                update,
            )
        }
        None => result.0,
    }
}

/// Match a split variable.
fn variable_unpacking(input: PosStr) -> IResult<PosStr, Vec<Identifier>> {
    separated_nonempty_list_complete!(input, COMMA, IDENTIFIER)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::Read;
    use std::path::Path;

    #[cfg(test)]
    mod subparsers {
        use super::*;

        #[test]
        fn parse_post_index() {
            let e = ParserContext::empty();

            check_match_no_update(
                "[a:b:c, :, d]",
                |x| e.post_index(x),
                PostIdent::Index {
                    slices: vec![
                        (
                            Some(Node::from("a")),
                            Some(Node::from("b")),
                            Some(Node::from("c")),
                        ),
                        (None, None, None),
                        (Some(Node::from("d")), None, None),
                    ],
                },
            );
            simple_check_failed("[a", |x| e.post_index(x));
            simple_check_failed("[a:", |x| e.post_index(x));
        }

        #[test]
        fn parse_atomic() {
            let e = ParserContext::empty();
            check_data_and_leftover("1 ** 2", |x| e.atomic_expr(x), Expr::from(1), "** 2");
            check_data("false", |x| e.atomic_expr(x), Expr::from(false));
        }

        #[test]
        fn parse_spec_literals() {
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
            check_data(
                input,
                |x| e.expression(x),
                Expr::BinaryExpr {
                    operator: BinaryOperator::Less,
                    left: wrap(Expr::from(0)),
                    right: wrap(Expr::Float("9.".to_string())),
                },
            );
        }

        /// Resolved by making a new token to recognize the negative unary operator specifically.
        /// It checks that it's not followed by a digit.
        #[test]
        #[ignore]
        fn failure_2019_12_14_5() {
            let input = "- 0   ";
            let e = ParserContext::empty();
            check_data(
                input,
                |x| e.expression(x),
                Expr::UnaryExpr {
                    operator: UnaryOperator::Negative,
                    operand: wrap(Expr::from(0)),
                },
            );
        }
    }

    #[test]
    fn parse_post_ident() {
        let e = ParserContext::empty();
        let expected_args = vec!["a", "b", "c"].iter().map(|x| Node::from(*x)).collect();
        check_match_no_update(
            "( a ,  b , c ) ",
            |x| e.trailer(x),
            PostIdent::Call {
                args: expected_args,
                kwargs: vec![],
            },
        );
        check_match_no_update(
            "( a   ,  b  =    true)",
            |x| e.trailer(x),
            PostIdent::Call {
                args: vec![Node::from("a")],
                kwargs: vec![(Identifier::from("b"), Node::from(true))],
            },
        );

        check_match_no_update(
            "( a   = true,  b = true ) ",
            |x| e.trailer(x),
            PostIdent::Call {
                args: vec![],
                kwargs: vec![
                    (Identifier::from("a"), Node::from(true)),
                    (Identifier::from("b"), Node::from(true)),
                ],
            },
        );

        check_match_no_update(
            "()",
            |x| e.trailer(x),
            PostIdent::Call {
                args: vec![],
                kwargs: vec![],
            },
        );

        simple_check_failed("(a | b=false)", |x| e.trailer(x));
        simple_check_failed("(a   b=false)", |x| e.trailer(x));
        simple_check_failed("(a,, b=false)", |x| e.trailer(x));
        simple_check_failed("(a,, b)", |x| e.trailer(x));
        simple_check_failed("(a, b =  true, c)", |x| e.trailer(x));

        check_match_no_update(
            ".asdf_   ",
            |x| e.trailer(x),
            PostIdent::Access {
                attribute: Identifier::from("asdf_"),
            },
        );

        check_match_no_update(
            "[a:b:c, :, d]",
            |x| e.trailer(x),
            PostIdent::Index {
                slices: vec![
                    (
                        Some(Node::from("a")),
                        Some(Node::from("b")),
                        Some(Node::from("c")),
                    ),
                    (None, None, None),
                    (Some(Node::from("d")), None, None),
                ],
            },
        );
    }

    #[test]
    fn test_parenthetical_expressions() {
        let e = ParserContext::empty();
        let expected = Expr::BinaryExpr {
            operator: BinaryOperator::Or,
            left: Box::new(Node::from(Expr::BinaryExpr {
                operator: BinaryOperator::And,
                left: Box::new(Node::from(true)),
                right: Box::new(Node::from(false)),
            })),
            right: Box::new(Node::from(true)),
        };
        check_data("(true and false) or true", |x| e.expression(x), expected);

        check_data(
            "(true and false)",
            |x| e.expression(x),
            Expr::BinaryExpr {
                operator: BinaryOperator::And,
                left: Box::new(Node::from(true)),
                right: Box::new(Node::from(false)),
            },
        );
    }

    #[test]
    fn parse_function_call() {
        let e = ParserContext::empty();
        let a = output(e.logical_binary_expr(PosStr::from("true and false"))).0;
        let no_args = Node::from(Expr::FunctionCall {
            function: wrap(Expr::from("func")),
            args: vec![],
            kwargs: vec![],
        });
        check_match_no_update("func()", |x| e.expr_with_trailer(x), no_args);

        let b = output(e.expression(PosStr::from("func()"))).0;
        let expected = Expr::FunctionCall {
            function: wrap(Expr::from("ident")),
            args: vec![a, b],
            kwargs: vec![],
        };

        check_data(
            "ident(true and false, func())",
            |x| e.expression(x),
            expected,
        );

        check_data(
            "func(a, b, c=true, d=true)",
            |x| e.expression(x),
            Expr::FunctionCall {
                function: Box::new(Node::from("func")),
                args: vec![Node::from("a"), Node::from("b")],
                kwargs: vec![
                    (Identifier::from("c"), Node::from(true)),
                    (Identifier::from("d"), Node::from(true)),
                ],
            },
        );

        let expected = Expr::FunctionCall {
            function: Box::new(Node::from(Expr::FunctionCall {
                function: Box::new(Node::from("func")),
                args: vec![Node::from("a")],
                kwargs: vec![],
            })),
            args: vec![Node::from("b"), Node::from("c")],
            kwargs: vec![],
        };
        check_data("func(a)(b, c)", |x| e.expression(x), expected);

        check_data(
            "(a and b)(true)",
            |x| e.expression(x),
            Expr::FunctionCall {
                function: Box::new(output(e.logical_binary_expr(PosStr::from("a and b"))).0),
                args: vec![Node::from(true)],
                kwargs: vec![],
            },
        );
    }

    #[test]
    fn parse_comparison_expr() {
        let e = ParserContext::empty();
        let comp_strs = vec![">", "<", ">=", "<=", "==", "!="];
        let comp_ops = vec![
            BinaryOperator::Greater,
            BinaryOperator::Less,
            BinaryOperator::GreaterEqual,
            BinaryOperator::LessEqual,
            BinaryOperator::Equal,
            BinaryOperator::Unequal,
        ];
        for (comp_str, comp_op) in comp_strs.iter().zip(comp_ops.iter()) {
            let as_str = format!("true {} false", comp_str);
            let expr = e.expression(PosStr::new(as_str.as_bytes()));
            let expected = Node::from(Expr::BinaryExpr {
                left: Box::new(Node::from(true)),
                right: Box::new(Node::from(false)),
                operator: *comp_op,
            });

            assert_eq!(output(expr).0, expected);
        }
    }

    #[test]
    fn parse_binary_expr() {
        let e = ParserContext::empty();

        check_data(
            "1 ** 2",
            |x| e.expression(x),
            Expr::BinaryExpr {
                operator: BinaryOperator::Exponent,
                left: Box::new(Node::from(1)),
                right: Box::new(Node::from(2)),
            },
        );

        check_data(
            "true and false",
            |x| e.expression(x),
            Expr::BinaryExpr {
                operator: BinaryOperator::And,
                left: Box::new(Node::from(true)),
                right: Box::new(Node::from(false)),
            },
        );

        check_data(
            "true or false",
            |x| e.expression(x),
            Expr::BinaryExpr {
                operator: BinaryOperator::Or,
                left: Box::new(Node::from(true)),
                right: Box::new(Node::from(false)),
            },
        );

        check_data(
            "true and false or true",
            |x| e.expression(x),
            Expr::BinaryExpr {
                operator: BinaryOperator::And,
                left: Box::new(Node::from(true)),
                right: Box::new(Node::from(Expr::BinaryExpr {
                    operator: BinaryOperator::Or,
                    left: Box::new(Node::from(false)),
                    right: Box::new(Node::from(true)),
                })),
            },
        );

        let all_ops = vec![
            "and", "or", "xor", "&", "|", "^", "+", "-", "*", "/", "%", ">>", "<<", "**",
        ];
        for op in all_ops {
            let input = format!("x {} y", op);
            check_data(
                input.as_str(),
                |x| e.expression(x),
                Expr::BinaryExpr {
                    operator: BinaryOperator::from(op),
                    left: Box::new(Node::from("x")),
                    right: Box::new(Node::from("y")),
                },
            );
        }
    }

    #[test]
    fn parse_unary_expr() {
        let e = ParserContext::empty();
        let ops = vec!["not", "+", "-", "~"];
        for op in ops {
            let input = format!("{} y", op);
            check_data(
                input.as_str(),
                |x| e.expression(x),
                Expr::UnaryExpr {
                    operator: UnaryOperator::from(op),
                    operand: Box::new(Node::from("y")),
                },
            );
        }
        check_data(
            "~+y",
            |x| e.expression(x),
            Expr::UnaryExpr {
                operator: UnaryOperator::BitNot,
                operand: Box::new(output(e.expression(PosStr::from("+y"))).0),
            },
        );
        check_data(
            "not true",
            |x| e.expression(x),
            Expr::UnaryExpr {
                operator: UnaryOperator::Not,
                operand: Box::new(Node::from(true)),
            },
        );
    }

    #[test]
    fn parse_comprehensions() {
        let e = ParserContext::empty();

        let set = e.expression(PosStr::from("{x for x in y}"));
        match output(set).0.data {
            Expr::IdentifierExpr(_) => {}
            _ => panic!(),
        };

        let map = e.expression(PosStr::from("{x:1 for x in y}"));
        match output(map).0.data {
            Expr::IdentifierExpr(_) => {}
            _ => panic!(),
        };

        let vec_c = e.expression(PosStr::from("[x for x in y]"));
        match output(vec_c).0.data {
            Expr::IdentifierExpr(_) => {}
            _ => panic!(),
        };

        let gen = e.expression(PosStr::from("(x for x in y)"));
        match output(gen).0.data {
            Expr::IdentifierExpr(_) => {}
            _ => panic!(),
        };

        let vec_c = e.expression(PosStr::from("[x for x in y for x in y]]"));
        match output(vec_c).0.data {
            Expr::IdentifierExpr(_) => {}
            _ => panic!(),
        };

        check_match(
            "for a, b in c if true",
            |x| e.comprehension_for(x),
            (
                vec![Identifier::from("a"), Identifier::from("b")],
                (Node::from("c"), vec![]),
                Some((Node::from(true), vec![])),
            ),
        );
    }

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
        check_match_no_update(&int.to_string(), |x| e.expression(x), Node::from(int));
        let rand_float = rand::random::<f64>().abs();
        check_match_no_update(
            &rand_float.to_string(),
            |x| e.expression(x),
            Node::from(rand_float),
        );
        let expr = Expr::String("asdf\\\"\\\ra\'sdf".to_string());
        check_match_no_update(
            "\"asdf\\\"\\\ra\'sdf\"",
            |x| e.expression(x),
            Node::from(expr),
        );

        check_match_no_update(
            "[true, false]",
            |x| e.expression(x),
            Node::from(Expr::VecLiteral(vec![Node::from(true), Node::from(false)])),
        );
        check_match_no_update(
            "(true, false)",
            |x| e.expression(x),
            Node::from(Expr::TupleLiteral(vec![
                Node::from(true),
                Node::from(false),
            ])),
        );

        check_failed(".", |x| e.expression(x), ErrorKind::Alt);
    }

    #[test]
    fn parse_collection_literals() {
        let e = ParserContext::empty();
        check_data(
            "[1, 2]",
            |x| e.expression(x),
            Expr::VecLiteral(vec![Node::from(1), Node::from(2)]),
        );

        check_data("()", |x| e.expression(x), Expr::TupleLiteral(vec![]));

        check_data("(  )", |x| e.expression(x), Expr::TupleLiteral(vec![]));

        check_data(
            "(1, )",
            |x| e.expression(x),
            Expr::TupleLiteral(vec![Node::from(1)]),
        );

        check_data(
            "(1, 2)",
            |x| e.expression(x),
            Expr::TupleLiteral(vec![Node::from(1), Node::from(2)]),
        );

        check_data(
            "(1, 2,)",
            |x| e.expression(x),
            Expr::TupleLiteral(vec![Node::from(1), Node::from(2)]),
        );
    }

    #[test]
    fn parse_wrapped() {
        let e = ParserContext::empty();
        check_data(
            "2 * (1 + (3))",
            |x| e.expression(x),
            Expr::BinaryExpr {
                operator: BinaryOperator::Mult,
                left: Box::new(Node::from(2)),
                right: Box::new(Node::from(Expr::BinaryExpr {
                    operator: BinaryOperator::Add,
                    left: Box::new(Node::from(Expr::from(1))),
                    right: Box::new(Node::from(Expr::from(3))),
                })),
            },
        );
    }

    #[test]
    fn parse_struct_literal() {
        let e = ParserContext::empty();
        check_data(
            "a.b{1,2,3}",
            |x| e.expression(x),
            Expr::StructLiteral {
                base: Box::new(Node::from(Expr::AttributeAccess {
                    base: Box::new(Node::from(Expr::IdentifierExpr(Identifier::from("a")))),
                    attribute: Identifier::from("b"),
                })),
                fields: vec![Node::from(1), Node::from(2), Node::from(3)],
            },
        );
    }

    #[test]
    fn parse_expr_with_trailer() {
        let e = ParserContext::empty();

        check_data(
            "a()",
            |x| e.expr_with_trailer(x),
            Expr::FunctionCall {
                function: wrap(Expr::from("a")),
                args: vec![],
                kwargs: vec![],
            },
        );

        check_failed("123", |x| e.expr_with_trailer(x), ErrorKind::Alt);
    }

    #[test]
    fn parse_special_chars() {
        let f_path = Path::new("tests/test_data/special_chars.txt");
        let mut f = File::open(f_path).expect("File not found");
        let mut file_contents = String::new();
        f.read_to_string(&mut file_contents).unwrap();
        check_data_no_update(
            file_contents.as_str().trim(),
            string_expr,
            Expr::String("\\\"\\n\'\\\\\\\'".to_string()),
        );
    }

    #[test]
    fn parse_module_access() {
        let mut e = ParserContext::empty();
        let import = Import {
            id: 0,
            path: vec![Identifier::from("file_2")],
            alias: None,
            values: vec![],
        };
        e.imported.insert(Identifier::from("file_2"), import);
        check_data(
            "file_2.foo",
            |x| e.expression(x),
            Expr::ModuleAccess(0, vec![Identifier::from("file_2"), Identifier::from("foo")]),
        );
    }
}
