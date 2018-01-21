enum Operator {
	or,
	and,
	xor,
	not,
	value
}

// Currently this only handles boolean expressions 
// because they have a fixed size.
// It should be expanded to other types.

// Also currently there can exist invalid
// expressions like "not(true, true)"
// or "value(true, false)" or "and(true)".
// Might want to add some validation here
// to say that value expressions have a 
// single_value, not expressions have a
// left but no right, and other expressions 
// have a left and a right.
struct Expression {
	operator: Operator,
	left: Option<Box<Expression>>,
	right: Option<Box<Expression>>,
	single_value: Option<bool>
}

fn evaluate (expr: Expression) -> Option<bool> {
	if expr.single_value.is_some() {
		return expr.single_value
	}
	let result = match expr.operator {
		Operator::value => expr.single_value,
		Operator::or => Some(evaluate(*(expr.left.expect("no"))).expect("no") || 
						evaluate(*(expr.right.expect("no"))).expect("no")),
		Operator::and => Some(evaluate(*(expr.left.expect("no"))).expect("no") && 
						evaluate(*(expr.right.expect("no"))).expect("no")),
		Operator::xor => Some(xor(evaluate(*(expr.left.expect("no"))).expect("no"), 
							      evaluate(*(expr.right.expect("no"))).expect("no"))),
		Operator::not => Some(!evaluate(*(expr.left.expect("invalid expression"))).expect("no")),
	};
	result
}

fn xor (left: bool, right: bool) -> bool {
	let a = left && !right;
	let b = right && !left;
	a || b
}

//TODO make it real
fn validate (expr: Expression) -> bool {
	true
}

#[test]
fn test_single_value() {
    let simple_truth = Expression{
    operator: Operator::value, 
    left: None, right: None, 
    single_value: Some(true)};
    let value_of_truth = evaluate(simple_truth);
	assert_eq!(value_of_truth, Some(true));
}

#[test]
fn test_tree() {
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
    evaluate(true_and_true);
}

// "value" operator should not be used with left/right
#[test]
fn test_bad_value_operator_left() {
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
