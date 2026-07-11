extern crate nom;
use self::nom::*;
use crate::expression::*;
use crate::parser::base::{ExprNode, ParserContext, TypeRes};
use crate::parser::expression_parser::{bool_expr, float_expr, int_expr};
use crate::parser::parser_utils::iresult_helpers::*;
use crate::parser::parser_utils::tokens::*;
use crate::parser::parser_utils::*;
use crate::parser::position_tracker::PosStr;
use crate::type_checking::types::{Refinement, Type};
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, opt, value};
use nom::multi::{many0, separated_list0, separated_list1};
use nom::sequence::{delimited, preceded};

type JustExpr<'a> = IResult<PosStr<'a>, ExprNode>;

impl ParserContext {
    pub fn parse_type<'a>(&self, input: PosStr<'a>) -> TypeRes<'a> {
        if self.can_use_self {
            with_self(input)
        } else {
            any_type(input)
        }
    }
}

pub fn with_self(input: PosStr) -> TypeRes {
    alt((map(SELF, |_x| Type::SelfT), any_type)).parse(input)
}

/// Parse a type.
pub fn any_type(input: PosStr) -> TypeRes {
    alt((product_type, sum_type)).parse(input)
}

pub fn product_type(input: PosStr) -> TypeRes {
    let result = delimited(
        OPEN_PAREN,
        separated_list0(COMMA, alt((product_type, sum_type))),
        CLOSE_PAREN,
    )
    .parse(input);

    fmap_iresult(result, Type::Product)
}

pub fn sum_type(input: PosStr) -> TypeRes {
    let result = (
        parameterized_type,
        many0(preceded(VBAR, parameterized_type)),
    )
        .parse(input);

    fmap_iresult(result, |mut x| match x.1.len() {
        0 => x.0,
        _ => {
            x.1.insert(0, x.0);
            Type::Sum(x.1)
        }
    })
}

pub fn parameterized_type(input: PosStr) -> TypeRes {
    let result = (
        IDENTIFIER,
        opt(delimited(LANGLE, separated_list1(COMMA, any_type), RANGLE)),
        opt(refinement),
    )
        .parse(input);
    fmap_iresult(result, |(base, param, refine)| {
        let b = match param {
            Some(y) => Type::Parameterized(base, y),
            None => Type::from(base),
        };
        match refine {
            Some(r) => Type::Refinement(Box::new(b), r),
            None => b,
        }
    })
}

fn refinement(input: PosStr) -> IResult<PosStr, Vec<Refinement>> {
    delimited(
        OPEN_BRACKET,
        separated_list1(COMMA, single_refinement),
        CLOSE_BRACKET,
    )
    .parse(input)
}

fn single_refinement(input: PosStr) -> IResult<PosStr, Refinement> {
    let parse_result = (
        logical_binary_expr,
        alt((DEQUAL, NEQUAL, LEQUAL, GEQUAL, LANGLE, RANGLE)),
        logical_binary_expr,
    )
        .parse(input);

    fmap_iresult(parse_result, |(l, o, r)| Refinement {
        operator: ComparisonOperator::from(o),
        left: Box::new(l),
        right: Box::new(r),
    })
}

fn flatten_binary(result: (Node<Expr>, Option<(PosStr, Node<Expr>)>)) -> Node<Expr> {
    match result.1 {
        Some((o, right)) => {
            let op = BinaryOperator::from(o.slice);
            Node::from(Expr::BinaryExpr {
                operator: op,
                left: Box::new(result.0),
                right: Box::new(right),
            })
        }
        None => result.0,
    }
}

/// Match a list of binary operations
fn binary_expr<'a>(
    input: PosStr<'a>,
    operator_parser: impl Fn(PosStr<'a>) -> IResult<PosStr<'a>, PosStr<'a>> + Copy,
    next_expr: impl Fn(PosStr<'a>) -> JustExpr<'a> + Copy,
) -> JustExpr<'a> {
    let parse_result = (
        next_expr,
        opt((operator_parser, move |i| {
            binary_expr(i, operator_parser, next_expr)
        })),
    )
        .parse(input);

    fmap_iresult(parse_result, flatten_binary)
}

/// Match logical expressions.
/// Must be public because it's used by several statements
pub fn logical_binary_expr(input: PosStr<'_>) -> JustExpr<'_> {
    binary_expr(input, |x| alt((AND, OR, XOR)).parse(x), additive_expr)
}

/// Match addition and subtraction expressions.
fn additive_expr(input: PosStr<'_>) -> JustExpr<'_> {
    binary_expr(input, |x| alt((PLUS, MINUS)).parse(x), mult_expr)
}

/// Match multiplication, division, and modulo expressions.
fn mult_expr(input: PosStr<'_>) -> JustExpr<'_> {
    binary_expr(input, |x| alt((STAR, DIV, MOD)).parse(x), unary_expr)
}

/// Match an exponentiation expression.
fn power_expr(input: PosStr<'_>) -> JustExpr<'_> {
    binary_expr(input, EXP, atomic_expr)
}

fn unary_expr(input: PosStr<'_>) -> JustExpr<'_> {
    let parse_result = alt((
        (map(alt((PLUS, NEG, TILDE, NOT)), Some), unary_expr),
        (value(None, tag("")), power_expr),
    ))
    .parse(input);

    let node = fmap_iresult(parse_result, |(maybe_op, expr)| match maybe_op {
        Some(op_str) => Node::from(Expr::UnaryExpr {
            operator: UnaryOperator::from(op_str),
            operand: Box::new(expr),
        }),
        None => expr,
    });
    node
}

fn atomic_expr(input: PosStr<'_>) -> JustExpr<'_> {
    w_followed!(
        input,
        alt((
            bool_expr,
            float_expr,
            int_expr,
            map(IDENTIFIER, |x| Node::from(Expr::IdentifierExpr(x))),
            map(
                |i| w_followed!(i, tag("$ret")),
                |x| Node::from(Expr::from(x))
            ),
        ))
    )
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
        check_match(
            "Test<i32>",
            any_type,
            Type::Parameterized(Identifier::from("Test"), vec![Type::i32]),
        );
    }

    #[test]
    fn test_sum_types() {
        check_match("i32 | i64", any_type, Type::Sum(vec![Type::i32, Type::i64]));
    }

    #[test]
    fn test_product_types() {
        check_match(
            "(i32, i64)",
            any_type,
            Type::Product(vec![Type::i32, Type::i64]),
        );
    }

    #[test]
    fn test_refinements() {
        check_match(
            "i32 [x > 0]",
            any_type,
            Type::Refinement(
                Box::from(Type::i32),
                vec![Refinement {
                    operator: ComparisonOperator::Greater,
                    left: wrap(Expr::from("x")),
                    right: wrap(Expr::from(0)),
                }],
            ),
        );
    }
}
