/// Helpers for writing property based tests.
use expression::*;

/// Strategies for use in property-based testing.
#[allow(unused)]
pub(crate) mod strategies {
    use super::*;
    use proptest::prelude::*;
    use proptest::collection;
    extern crate rand;
    use rand::Rng;

    use proptest::string::{
        string_regex,
        RegexGeneratorStrategy
    };
    use proptest::strategy::{
        Map
    };
    use proptest::arbitrary::{
        StrategyFor
    };

    /// Generate a random binary operator.
    fn binary_operator_strat() -> impl Strategy<Value = BinaryOperator> {
        prop_oneof![
            Just(BinaryOperator::Add),
            Just(BinaryOperator::Sub),
            Just(BinaryOperator::Div),
            Just(BinaryOperator::Mult),
            Just(BinaryOperator::Mod),
            Just(BinaryOperator::And),
            Just(BinaryOperator::Or),
            Just(BinaryOperator::Xor),
            Just(BinaryOperator::BitAnd),
            Just(BinaryOperator::BitOr),
            Just(BinaryOperator::BitXor),
            Just(BinaryOperator::BitShiftL),
            Just(BinaryOperator::BitShiftR),
            Just(BinaryOperator::Exponent)
        ]
    }

    /// Generate a random comparison operator.
    fn comparison_operator_strat() -> impl Strategy<Value = ComparisonOperator> {
        prop_oneof![
            Just(ComparisonOperator::Greater),
            Just(ComparisonOperator::Less),
            Just(ComparisonOperator::Equal),
            Just(ComparisonOperator::Unequal),
            Just(ComparisonOperator::GreaterEqual),
            Just(ComparisonOperator::LessEqual)
        ]
    }

    /// Generate a random unary operator.
    fn unary_operator_strat() -> impl Strategy<Value = UnaryOperator> {
        prop_oneof![
            Just(UnaryOperator::Positive),
            Just(UnaryOperator::Negative),
            Just(UnaryOperator::Not),
            Just(UnaryOperator::BitNot),
        ]
    }

    fn ident_expr_strat(in_scope: &'static Vec<String>) -> impl Strategy<Value = Expr> {
        return (0..in_scope.len(), Just(in_scope)).prop_map(|(x, y)| {
            Expr::IdentifierExpr(Identifier::from(y[x].clone()))
        });
    }

    /// Generate a recursive expression.
    fn complex_expression_strat() -> impl Strategy<Value = Expr> {
        let leaf = literal_strategy();
        // Minimum 1 level deep, aim for 6 levels deep, usually each level contains 2 branches.
        leaf.prop_recursive(1, 3, 2, |inner| prop_oneof![
            (inner.clone(), inner.clone(), binary_operator_strat()).prop_map(
                |(left, right, operator)| Expr::BinaryExpr{operator, left: wrap(left), right: wrap(right)}
            ),
            (inner.clone(), inner.clone(), comparison_operator_strat()).prop_map(
                |(left, right, operator)| Expr::ComparisonExpr{operator, left: wrap(left), right: wrap(right)}
            ),
            (inner.clone(), unary_operator_strat()).prop_map(
                |(operand, operator)| Expr::UnaryExpr{operator, operand: wrap(operand)}
            )
        ])
    }

    /// Generate a random literal expression.
    pub fn literal_strategy() -> impl Strategy<Value = Expr> {
        prop_oneof![
            // i64 Strategy
            any::<i64>().prop_map(Expr::from),
            // f64 Strategy
            any::<f64>().prop_map(|x| {
                if x.fract() == 0.0 {
                    Expr::Float(format!("{}.", x))
                } else {
                    Expr::Float(format!("{}", x))
                }
            }),
            // Boolean strategy
            any::<bool>().prop_map(Expr::from),
            // ASCII string strategy
            string_regex(r#"[[ !#-\[\]-~]]*"#).unwrap().prop_map(|x| Expr::String(x)),
        ]
    }

    fn identifier_strategy() -> impl Strategy<Value = Identifier> {
        return string_regex(r"[_a-zA-Z][_a-zA-Z0-9]*").unwrap().prop_map(|x| Identifier::from(x));
    }

    /// Generate a random expression.
    pub fn expr_strategy() -> impl Strategy<Value = Expr> {
        prop_oneof![
            // Any literal
            literal_strategy(),
            // Identifier expression
            identifier_strategy().prop_map(|x| Expr::IdentifierExpr(x)),
            // Binary expression
            complex_expression_strat()
        ]
    }

    fn let_strategy() -> impl Strategy<Value = Stmt> {
        return (identifier_strategy(), expr_strategy()).prop_map(|(n, v)| Stmt::LetStmt{
            name: n, expression: Node::from(v), type_annotation: None
        });
    }

    fn assignment_strategy() -> impl Strategy<Value = Stmt> {
        return (identifier_strategy(), expr_strategy()).prop_map(|(n, v)| Stmt::AssignmentStmt{name: n, expression: Node::from(v)});
    }


    pub fn stmt_strategy() -> impl Strategy<Value = Stmt> {
        return prop_oneof![
            let_strategy(),
            assignment_strategy()
        ];
    }

    pub fn block_strategy() -> impl Strategy<Value = Block> {
        return collection::vec(stmt_strategy(), 5).prop_map(|x| Block{statements: x.into_iter().map(wrap).collect()});
    }

    impl Expr {
        /// Turn an expression into a PosStr that should parse to that expression.
        /// Whitespace is inserted randomly. Technically this breaks one of the assumptions of proptest,
        /// so if there are issues with parsing whitespace proptest will not be able to replicate failing tests,
        /// or at least won't be able to shrink test cases properly.
        pub fn inverse_parse(&self) -> String {
            let mut rng = rand::thread_rng();
            let post_space: usize = rng.gen_range(0, 10);
            return match self {
                Expr::BinaryExpr{ref operator, ref left, ref right} => format!(
                    "{}{}{}", left.data.inverse_parse(), operator.to_string(), right.data.inverse_parse()
                ),
                Expr::ComparisonExpr{ref operator, ref left, ref right} => format!(
                    "{}{}{}", left.data.inverse_parse(), operator.to_string(), right.data.inverse_parse()
                ),
                Expr::UnaryExpr{ref operator, ref operand} => {
                    match (operator, &operand.data) {
                        (UnaryOperator::Negative, &Expr::Float(_)) | (UnaryOperator::Negative, &Expr::Int(_))=> format!("{} {}", 
                            operator.to_string(), operand.data.inverse_parse()
                        ),
                        _ => format!("{}{}", operator.to_string(), operand.data.inverse_parse())
                    }
                    
                }
                Expr::String(v) => format!("\"{}\"{}", v, " ".repeat(post_space)),
                Expr::Int(v) | Expr::Float(v) => format!("{}{}", v, " ".repeat(post_space)),
                // Yes the code is exactly the same, but the type of `v` is different.
                Expr::Bool(v) => format!("{}{}", v, " ".repeat(post_space)),
                Expr::IdentifierExpr(v) => v.name.clone(),
                x => panic!("{:?}", x)
            };
        }
    }

}