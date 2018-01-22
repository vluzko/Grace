#[derive(Copy, Clone)]
pub enum Operator {
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
pub struct Expression {
	pub operator: Operator,
	pub left: Option<Box<Expression>>,
	pub right: Option<Box<Expression>>,
	pub single_value: Option<bool>
}

pub fn evaluate (expr: Expression) -> Option<bool> {
	if !is_valid_expr(expr.operator, expr.single_value.is_some(),
				 expr.left.is_some(), expr.right.is_some()) {
		return None
	}
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

fn is_valid_expr(operator: Operator, value_set: bool, left_set: bool, right_set: bool) -> bool{
	match operator {
		Operator::value => value_set && !left_set && !right_set,
		Operator::or => !value_set && left_set && right_set,
		Operator::and => {
			if value_set || !left_set || !right_set {
				false
			} else {
				true
			}
		},
		Operator::xor => {
			if value_set || !left_set || !right_set {
				false
			} else {
				true
			}
		},
		Operator::not => {
			if value_set || !left_set || right_set {
				false
			} else {
				true
			}
		},
	}
}