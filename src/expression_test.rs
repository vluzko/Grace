use expression::*;

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

}

//#[test]
//fn test_or() {
//	let simple_truth = BinaryExpression {
//		operator: BinaryOperator::value,
//		left: None, right: None,
//		single_value: Some(true)};
//
//	let simply_false = BinaryExpression {
//	    operator: BinaryOperator::value,
//	    left: None, right: None,
//	    single_value: Some(false)};
//
//    let true_or_false = BinaryExpression {
//    	operator: BinaryOperator::or,
//    	left: Some(Box::new(simple_truth)), right: Some(Box::new(simply_false)),
//    	single_value: None};
//    assert_eq!(evaluate(true_or_false), Some(true));
//}

//#[test]
//fn test_xor() {
//	let simple_truth = BinaryExpression {
//		operator: BinaryOperator::value,
//		left: None, right: None,
//		single_value: Some(true)};
//
//	let simply_false = BinaryExpression {
//	    operator: BinaryOperator::value,
//	    left: None, right: None,
//	    single_value: Some(false)};
//
//    let true_xor_false = BinaryExpression {
//    	operator: BinaryOperator::xor,
//    	left: Some(Box::new(simple_truth)), right: Some(Box::new(simply_false)),
//    	single_value: None};
//    assert_eq!(evaluate(true_xor_false), Some(true));
//}

//#[test]
//fn test_tree() {  // two layers of nesting
//    let simple_truth = BinaryExpression {
//    	operator: BinaryOperator::value,
//    	left: None, right: None,
//    	single_value: Some(true)};
//
//    let simply_false = BinaryExpression {
//    	operator: BinaryOperator::value,
//    	left: None, right: None,
//    	single_value: Some(false)};
//
//    let not_false = BinaryExpression {
//    	operator: BinaryOperator::not,
//    	left: Some(Box::new(simply_false)), right: None,
//    	single_value: None};
//
//    let true_and_true = BinaryExpression {
//    	operator: BinaryOperator::and,
//    	left: Some(Box::new(simple_truth)), right: Some(Box::new(not_false)),
//    	single_value: None};
//    assert_eq!(evaluate(true_and_true), Some(true));
//}

//#[test]
//fn test_bad_value_operator_left_expr_set() {
//    let simply_false = BinaryExpression {
//    	operator: BinaryOperator::value,
//    	left: None, right: None,
//    	single_value: Some(false)};
//
//    let wrong = BinaryExpression {
//    	operator: BinaryOperator::value,
//    	left: Some(Box::new(simply_false)), right: None,
//    	single_value: None};
//    assert_eq!(evaluate(wrong), None);
//}

//#[test]
//fn test_bad_not_operator_right_expr_set() {
//    let simply_false = BinaryExpression {
//        operator: BinaryOperator::value,
//        left: None, right: None,
//        single_value: Some(false)};
//    let also_false = BinaryExpression {
//        operator: BinaryOperator::value,
//        left: None, right: None,
//        single_value: Some(false)};
//
//    let wrong = BinaryExpression {
//        operator: BinaryOperator::not,
//        left: Some(Box::new(simply_false)), right: Some(Box::new(also_false)),
//        single_value: None};
//    assert_eq!(evaluate(wrong), None);
//}

//#[test]
//fn test_bad_and_operator_value_set() {
//    let simply_false = BinaryExpression {
//        operator: BinaryOperator::value,
//        left: None, right: None,
//        single_value: Some(false)};
//    let also_false = BinaryExpression {
//        operator: BinaryOperator::value,
//        left: None, right: None,
//        single_value: Some(false)};
//
//    let wrong = BinaryExpression {
//        operator: BinaryOperator::and,
//        left: Some(Box::new(simply_false)), right: Some(Box::new(also_false)),
//        single_value: Some(true)};
//    assert_eq!(evaluate(wrong), None);
//}

//#[test]
//fn test_bad_and_operator_no_right() {
//    let simply_false = BinaryExpression {
//        operator: BinaryOperator::value,
//        left: None, right: None,
//        single_value: Some(false)};
//    let also_false = BinaryExpression {
//        operator: BinaryOperator::value,
//        left: None, right: None,
//        single_value: Some(false)};
//
//    let wrong = BinaryExpression {
//        operator: BinaryOperator::and,
//        left: Some(Box::new(simply_false)), right: None,
//        single_value: None};
//    assert_eq!(evaluate(wrong), None);
//}
