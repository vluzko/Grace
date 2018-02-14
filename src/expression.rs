

pub trait ASTNode: ToString {
}

pub trait Expression: ASTNode{
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOperator {
	Or,
	And,
	Xor,
}

impl ToString for BinaryOperator{
	fn to_string(&self) -> String {
		match self {
			&BinaryOperator::Or => "or".to_string(),
			&BinaryOperator::And => "and".to_string(),
			&BinaryOperator::Xor => "xor".to_string(),
		}
	}
}

#[derive(Debug, Copy, Clone)]
pub enum Boolean {
    True,
    False
}

impl ToString for Boolean {
    fn to_string(&self) -> String {
        match self {
            &Boolean::True => "true".to_string(),
            &Boolean::False => "false".to_string()
        }
    }

}

impl Expression for Boolean {}
impl ASTNode for Boolean {}

// Currently this only handles boolean expressions 
// because they have a fixed size.
// It should be expanded to other types.
pub struct BinaryExpression <'a, 'b> {
	pub operator: BinaryOperator,
	pub left: &'a Expression,
	pub right: &'b Expression
}

impl<'a, 'b> ToString for BinaryExpression<'a, 'b> {
    fn to_string(&self) -> String {
        format!("{} {} {}",
				self.left.to_string(),
				self.operator.to_string(),
				self.right.to_string())
		//self.left.to_string()
        //self.operator.to_string()
    }
}

impl<'a, 'b> Expression for BinaryExpression<'a, 'b> {}
impl<'a, 'b> ASTNode for BinaryExpression<'a, 'b> {}

/*pub fn evaluate (expr: BinaryExpression) -> Option<bool> {
	let result = match expr.operator {
		BinaryOperator::Or => Some(evaluate(*(expr.left.expect("no"))).expect("no") ||
						evaluate(*(expr.right.expect("no"))).expect("no")),
		BinaryOperator::And => Some(evaluate(*(expr.left.expect("no"))).expect("no") &&
						evaluate(*(expr.right.expect("no"))).expect("no")),
		BinaryOperator::Xor => Some(xor(evaluate(*(expr.left.expect("no"))).expect("no"),
                                        evaluate(*(expr.right.expect("no"))).expect("no"))),
	};
	result
}*/

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
