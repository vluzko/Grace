use expression::*;

//#[test]
//fn test_single_value() {
//    let simple_truth = BinaryExpression {
//    operator: BinaryOperator::Value,
//    left: None, right: None,
//    single_value: Some(true)};
//	assert_eq!(evaluate(simple_truth), Some(true));
//}

//#[test]
//fn test_not() {
//    let simply_false = BinaryExpression {
//    	operator: BinaryOperator::Value,
//    	left: None, right: None,
//    	single_value: Some(false)};
//
//    let not_false = BinaryExpression {
//    	operator: BinaryOperator::not,
//    	left: Some(Box::new(simply_false)), right: None,
//    	single_value: None};
//    assert_eq!(evaluate(not_false), Some(true));
//}

#[test]
fn test_and() {

    let true_and_false: BinaryExpression = BinaryExpression {
    	operator: BinaryOperator::And,
    	left: &Boolean::True, right: &Boolean::False};
	println!("{}", true_and_false.to_string())
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
