pub trait ASTNode: ToString {
}

pub trait Expression: ASTNode{
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

// Currently Expression only handles boolean expressions
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
    }
}

impl<'a, 'b> Expression for BinaryExpression<'a, 'b> {}
impl<'a, 'b> ASTNode for BinaryExpression<'a, 'b> {}

// This is not the set of unary operators we will end up supporting.
// This is a random grab bag just to get the UnaryExpression struct working.
#[derive(Debug, Copy, Clone)]
pub enum UnaryOperator {
    Not,
    PreIncrement,
    PreDecrement,
    PostIncrement,
    PostDecrement
}

impl ToString for UnaryOperator{
    fn to_string(&self) -> String {
        match self {
            &UnaryOperator::Not => "not".to_string(),
            &UnaryOperator::PreIncrement => "++".to_string(),
            &UnaryOperator::PreDecrement => "--".to_string(),
            &UnaryOperator::PostIncrement => "++".to_string(),
            &UnaryOperator::PostDecrement => "--".to_string(),
        }
    }
}

pub struct UnaryExpression <'a> {
    pub operator: UnaryOperator,
    pub operand: &'a Expression
}

impl<'a> ToString for UnaryExpression<'a> {
    fn to_string(&self) -> String {
        match self.operator {
            UnaryOperator::Not => format!("{} {}", self.operator.to_string(), self.operand.to_string()),
            UnaryOperator::PreIncrement | UnaryOperator::PreDecrement => format!(
                "{}{}", self.operator.to_string(), self.operand.to_string()),
            UnaryOperator::PostIncrement | UnaryOperator::PostDecrement => format!(
                "{}{}", self.operand.to_string(), self.operator.to_string()),
        }
    }
}

impl<'a> Expression for UnaryExpression<'a> {}
impl<'a> ASTNode for UnaryExpression<'a> {}