
pub trait ASTNode {}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOperator {
	Or,
	And,
	Xor,
}

#[derive(Debug, Copy, Clone)]
pub enum Boolean {
    True,
    False
}

impl ASTNode for Boolean {}

//pub enum Value {
//    Boolean(Boolean),
//}
//
//impl ASTNode for Value {}

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
pub struct BinaryExpression {
	pub operator: BinaryOperator,
	pub left: Option<Box<BinaryExpression>>,
	pub right: Option<Box<BinaryExpression>>,
	pub single_value: Option<bool>
}

pub fn evaluate (expr: BinaryExpression) -> Option<bool> {
	if !is_valid_expr(expr.operator, expr.single_value.is_some(),
				 expr.left.is_some(), expr.right.is_some()) {
		return None
	}
	if expr.single_value.is_some() {
		return expr.single_value
	}
	let result = match expr.operator {
//		BinaryOperator::Value => expr.single_value,
		BinaryOperator::Or => Some(evaluate(*(expr.left.expect("no"))).expect("no") ||
						evaluate(*(expr.right.expect("no"))).expect("no")),
		BinaryOperator::And => Some(evaluate(*(expr.left.expect("no"))).expect("no") &&
						evaluate(*(expr.right.expect("no"))).expect("no")),
		BinaryOperator::Xor => Some(xor(evaluate(*(expr.left.expect("no"))).expect("no"),
                                        evaluate(*(expr.right.expect("no"))).expect("no"))),
//		BinaryOperator::Not => Some(!evaluate(*(expr.left.expect("invalid expression"))).expect("no")),
	};
	result
}

fn xor (left: bool, right: bool) -> bool {
	let a = left && !right;
	let b = right && !left;
	a || b
}

fn is_valid_expr(operator: BinaryOperator, value_set: bool, left_set: bool, right_set: bool) -> bool{
	match operator {
//		BinaryOperator::Value => value_set && !left_set && !right_set,
		BinaryOperator::Or => !value_set && left_set && right_set,
		BinaryOperator::And => {
			if value_set || !left_set || !right_set {
				false
			} else {
				true
			}
		},
		BinaryOperator::Xor => {
			if value_set || !left_set || !right_set {
				false
			} else {
				true
			}
		},
//		BinaryOperator::Not => {
//			if value_set || !left_set || right_set {
//				false
//			} else {
//				true
//			}
//		}
	}
}
