// use expression::*;
/*
#[test]
fn test_to_string() {
    let true_and_false: BinaryExpression = BinaryExpression {
        operator: BinaryOperator::And,
        left: &Boolean::True,
        right: &Boolean::False
    };
    assert_eq!("true and false", true_and_false.to_string());

	let true_or_false: BinaryExpression = BinaryExpression {
		operator: BinaryOperator::Or,
		left: &Boolean::True, right: &Boolean::False};
    assert_eq!("true or false", true_or_false.to_string());

    let true_xor_false: BinaryExpression = BinaryExpression {
        operator: BinaryOperator::Xor,
        left: &Boolean::True, right: &Boolean::False};
    assert_eq!("true xor false", true_xor_false.to_string());

    let not_true: UnaryExpression = UnaryExpression {
        operator: UnaryOperator::Not,
        operand: &Boolean::True};
    assert_eq!("not true", not_true.to_string());
}

#[test]
fn test_recursion() {
    let true_or_false: BinaryExpression = BinaryExpression {
        operator: BinaryOperator::Or,
        left: &Boolean::True,
        right: &Boolean::False
    };
    let true_and_or: BinaryExpression = BinaryExpression {
        operator: BinaryOperator::And,
        left: &Boolean::True,
        right: &true_or_false,
    };
    assert_eq!("true and true or false", true_and_or.to_string());
    let not_and: UnaryExpression = UnaryExpression {
        operator: UnaryOperator::Not,
        operand: &true_and_or,
    };
    assert_eq!("not true and true or false", not_and.to_string());
    //TODO: handle parentheses when printing operators of different precedence

}*/