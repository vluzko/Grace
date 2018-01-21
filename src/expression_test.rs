use expression::*;
use expression::Expression;
use expression::evaluate;
use expression::Operator;

#[test]
fn test_single_value() {
    let simple_truth = Expression{
    operator: Operator::value, 
    left: None, right: None, 
    single_value: Some(true)};
	assert_eq!(evaluate(simple_truth), Some(true));
}

#[test]
fn test_not() {
    let simply_false = Expression{
    	operator: Operator::value, 
    	left: None, right: None, 
    	single_value: Some(false)};

    let not_false = Expression{
    	operator: Operator::not, 
    	left: Some(Box::new(simply_false)), right: None, 
    	single_value: None};
    assert_eq!(evaluate(not_false), Some(true));
}

#[test]
fn test_and() {
	let simple_truth = Expression{
		operator: Operator::value, 
		left: None, right: None, 
		single_value: Some(true)};  

	let simply_false = Expression{
	    operator: Operator::value, 
	    left: None, right: None, 
	    single_value: Some(false)};

    let true_and_false = Expression{
    	operator: Operator::and,
    	left: Some(Box::new(simple_truth)), right: Some(Box::new(simply_false)),
    	single_value: None};
    assert_eq!(evaluate(true_and_false), Some(false));
}

#[test]
fn test_or() {
	let simple_truth = Expression{
		operator: Operator::value, 
		left: None, right: None, 
		single_value: Some(true)};  

	let simply_false = Expression{
	    operator: Operator::value, 
	    left: None, right: None, 
	    single_value: Some(false)};

    let true_or_false = Expression{
    	operator: Operator::or,
    	left: Some(Box::new(simple_truth)), right: Some(Box::new(simply_false)),
    	single_value: None};
    assert_eq!(evaluate(true_or_false), Some(true));
}

#[test]
fn test_xor() {
	let simple_truth = Expression{
		operator: Operator::value, 
		left: None, right: None, 
		single_value: Some(true)};  

	let simply_false = Expression{
	    operator: Operator::value, 
	    left: None, right: None, 
	    single_value: Some(false)};

    let true_xor_false = Expression{
    	operator: Operator::xor,
    	left: Some(Box::new(simple_truth)), right: Some(Box::new(simply_false)),
    	single_value: None};
    assert_eq!(evaluate(true_xor_false), Some(true));
}

#[test]
fn test_tree() {  // two layers of nesting
    let simple_truth = Expression{
    	operator: Operator::value, 
    	left: None, right: None, 
    	single_value: Some(true)};

    let simply_false = Expression{
    	operator: Operator::value, 
    	left: None, right: None, 
    	single_value: Some(false)};

    let not_false = Expression{
    	operator: Operator::not, 
    	left: Some(Box::new(simply_false)), right: None, 
    	single_value: None};

    let true_and_true = Expression{
    	operator: Operator::and,
    	left: Some(Box::new(simple_truth)), right: Some(Box::new(not_false)),
    	single_value: None};
    assert_eq!(evaluate(true_and_true), Some(true));
}

#[test]
fn test_bad_value_operator_left_expr_set() {
    let simply_false = Expression{
    	operator: Operator::value, 
    	left: None, right: None, 
    	single_value: Some(false)};

    let wrong = Expression{
    	operator: Operator::value, 
    	left: Some(Box::new(simply_false)), right: None, 
    	single_value: None};
    assert_eq!(evaluate(wrong), None);
}

#[test]
fn test_bad_not_operator_right_expr_set() {
    let simply_false = Expression{
        operator: Operator::value, 
        left: None, right: None, 
        single_value: Some(false)};
    let also_false = Expression{
        operator: Operator::value, 
        left: None, right: None, 
        single_value: Some(false)};

    let wrong = Expression{
        operator: Operator::not, 
        left: Some(Box::new(simply_false)), right: Some(Box::new(also_false)), 
        single_value: None};
    assert_eq!(evaluate(wrong), None);
}

#[test]
fn test_bad_and_operator_value_set() {
    let simply_false = Expression{
        operator: Operator::value, 
        left: None, right: None, 
        single_value: Some(false)};
    let also_false = Expression{
        operator: Operator::value, 
        left: None, right: None, 
        single_value: Some(false)};

    let wrong = Expression{
        operator: Operator::and, 
        left: Some(Box::new(simply_false)), right: Some(Box::new(also_false)), 
        single_value: Some(true)};
    assert_eq!(evaluate(wrong), None);
}

#[test]
fn test_bad_and_operator_no_right() {
    let simply_false = Expression{
        operator: Operator::value, 
        left: None, right: None, 
        single_value: Some(false)};
    let also_false = Expression{
        operator: Operator::value, 
        left: None, right: None, 
        single_value: Some(false)};

    let wrong = Expression{
        operator: Operator::and, 
        left: Some(Box::new(simply_false)), right: None, 
        single_value: None};
    assert_eq!(evaluate(wrong), None);
}
