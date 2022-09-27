extern crate nom;
use self::nom::*;
use expression::*;
use parser::base::{ExprNode, ParserContext, TypeRes};
use parser::expression_parser::{bool_expr, float_expr, int_expr};
use parser::parser_utils::iresult_helpers::*;
use parser::parser_utils::tokens::*;
use parser::parser_utils::*;
use parser::position_tracker::PosStr;
use type_checking::types::{Refinement, Type};

type JustExpr<'a> = IResult<PosStr<'a>, ExprNode>;

impl ParserContext {
    pub fn parse_type<'a>(&self, input: PosStr<'a>) -> TypeRes<'a> {
        if self.can_use_self {
            return with_self(input);
        } else {
            return any_type(input.clone());
        }
    }
}

pub fn with_self<'a>(input: PosStr<'a>) -> TypeRes {
    return alt_complete!(
        input,
        map!(SELF, |_x| Type::self_type(Box::new(Type::Undetermined))) | any_type
    );
}

/// Parse a type.
pub fn any_type<'a>(input: PosStr<'a>) -> TypeRes {
    return alt_complete!(input, product_type | sum_type);
}

pub fn product_type<'a>(input: PosStr<'a>) -> TypeRes {
    let result = delimited!(
        input,
        OPEN_PAREN,
        separated_list!(COMMA, alt_complete!(product_type | sum_type)),
        CLOSE_PAREN
    );

    return fmap_iresult(result, |x| Type::Product(x));
}

pub fn sum_type<'a>(input: PosStr<'a>) -> TypeRes {
    let result = tuple!(
        input,
        parameterized_type,
        many0c!(preceded!(VBAR, parameterized_type))
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
    let result = tuple!(
        input,
        IDENTIFIER,
        optc!(delimited!(
            LANGLE,
            separated_nonempty_list!(COMMA, any_type),
            RANGLE
        )),
        optc!(refinement)
    );
    return fmap_iresult(result, |(base, param, refine)| {
        let b = match param {
            Some(y) => Type::Parameterized(base, y),
            None => Type::from(base),
        };
        match refine {
            Some(r) => Type::Refinement(Box::new(b), r),
            None => b,
        }
    });
}

fn refinement<'a>(input: PosStr<'a>) -> IResult<PosStr<'a>, Vec<Refinement>> {
    return delimited!(
        input,
        OPEN_BRACKET,
        separated_nonempty_list_complete!(COMMA, single_refinement),
        CLOSE_BRACKET
    );
}

fn single_refinement<'a>(input: PosStr<'a>) -> IResult<PosStr<'a>, Refinement> {
    let parse_result = tuple!(
        input,
        logical_binary_expr,
        alt_complete!(DEQUAL | NEQUAL | LEQUAL | GEQUAL | LANGLE | RANGLE),
        logical_binary_expr
    );

    return fmap_iresult(parse_result, |(l, o, r)| Refinement {
        operator: ComparisonOperator::from(o),
        left: Box::new(l),
        right: Box::new(r),
    });
}

fn flatten_binary<'a>(result: (Node<Expr>, Option<(PosStr<'a>, Node<Expr>)>)) -> Node<Expr> {
    return match result.1 {
        Some((o, right)) => {
            let op = BinaryOperator::from(o.slice);
            Node::from(Expr::BinaryExpr {
                operator: op,
                left: Box::new(result.0),
                right: Box::new(right),
            })
        }
        None => result.0,
    };
}

/// Match a list of binary operations
fn binary_expr<'a>(
    input: PosStr<'a>,
    operator_parser: impl Fn(PosStr) -> IResult<PosStr, PosStr>,
    next_expr: impl Fn(PosStr) -> JustExpr,
) -> JustExpr<'a> {
    let parse_result = tuple!(
        input,
        next_expr,
        optc!(tuple!(
            operator_parser,
            call!(binary_expr, operator_parser, next_expr)
        ))
    );

    return fmap_iresult(parse_result, flatten_binary);
}

/// Match logical expressions.
/// Must be public because it's used by several statements
pub fn logical_binary_expr<'a>(input: PosStr<'a>) -> JustExpr<'a> {
    return binary_expr(
        input,
        |x| alt_complete!(x, AND | OR | XOR),
        |x| additive_expr(x),
    );
}

/// Match addition and subtraction expressions.
fn additive_expr<'a>(input: PosStr<'a>) -> JustExpr<'a> {
    return binary_expr(input, |x| alt_complete!(x, PLUS | MINUS), |x| mult_expr(x));
}

/// Match multiplication, division, and modulo expressions.
fn mult_expr<'a>(input: PosStr<'a>) -> JustExpr<'a> {
    return binary_expr(
        input,
        |x| alt_complete!(x, STAR | DIV | MOD),
        |x| unary_expr(x),
    );
}

/// Match an exponentiation expression.
fn power_expr<'a>(input: PosStr<'a>) -> JustExpr<'a> {
    return binary_expr(input, |x| call!(x, EXP), |x| atomic_expr(x));
}

fn unary_expr<'a>(input: PosStr<'a>) -> JustExpr<'a> {
    let parse_result = alt!(
        input,
        tuple!(
            map!(alt_complete!(PLUS | NEG | TILDE | NOT), Some),
            unary_expr
        ) | tuple!(value!(None, tag!("")), power_expr)
    );

    let node = fmap_iresult(parse_result, |(maybe_op, expr)| match maybe_op {
        Some(op_str) => Node::from(Expr::UnaryExpr {
            operator: UnaryOperator::from(op_str),
            operand: Box::new(expr),
        }),
        None => expr,
    });
    return node;
}

fn atomic_expr<'a>(input: PosStr<'a>) -> JustExpr<'a> {
    let node = w_followed!(
        input,
        alt_complete!(
            bool_expr
                | float_expr
                | int_expr
                | map!(IDENTIFIER, |x| Node::from(Expr::IdentifierExpr(x)))
                | map!(w_followed!(tag!("$ret")), |x| Node::from(Expr::from(x)))
        )
    );
    return node;
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
