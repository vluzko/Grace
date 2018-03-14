use std::fmt;
use std::fmt::Display;

// TODO: Print subtree
pub trait ASTNode: Display {}

pub trait Statement: ASTNode {}

pub trait Expression: Statement {}

#[derive(Debug, Copy, Clone)]
pub enum Boolean {
    True,
    False
}

impl Display for Boolean {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &Boolean::True => "true",
            &Boolean::False => "false"
        })
    }
}
impl ASTNode for Boolean {}
impl Statement for Boolean {}
impl Expression for Boolean {}

pub struct Identifier {
    pub name: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
impl ASTNode for Identifier {}


pub struct Assignment {
    pub identifier: Identifier,
    pub expression: Box<Expression>,
}

impl Display for Assignment{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", self.identifier, self.expression.to_string())
    }
}
impl ASTNode for Assignment {}
impl Statement for Assignment {}



#[derive(Debug, Copy, Clone)]
pub enum BinaryOperator {
    Or,
    And,
    Xor,
}

impl Display for BinaryOperator {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &BinaryOperator::Or => "or",
            &BinaryOperator::And => "and",
            &BinaryOperator::Xor => "xor",
        })
    }
}

// Currently Expression only handles boolean expressions
// because they have a fixed size.
// It should be expanded to other types.
// TODO: you will have to switch everything to boxes
pub struct BinaryExpression{
	pub operator: BinaryOperator,
	pub left: Box<Expression>,
	pub right: Box<Expression>
}

impl Display for BinaryExpression {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.operator, self.right)
    }
}
impl ASTNode for BinaryExpression {}
impl Statement for BinaryExpression {}
impl Expression for BinaryExpression {}


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

impl Display for UnaryOperator{

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &UnaryOperator::Not => "not",
            &UnaryOperator::PreIncrement => "++",
            &UnaryOperator::PreDecrement => "--",
            &UnaryOperator::PostIncrement => "++",
            &UnaryOperator::PostDecrement => "--",
        })
    }
}

pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>
}

impl<'a> Display for UnaryExpression {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self.operator {
            UnaryOperator::Not => format!("{} {}", self.operator.to_string(), self.operand.to_string()),
            UnaryOperator::PreIncrement | UnaryOperator::PreDecrement => format!(
                "{}{}", self.operator.to_string(), self.operand.to_string()),
            UnaryOperator::PostIncrement | UnaryOperator::PostDecrement => format!(
                "{}{}", self.operand.to_string(), self.operator.to_string()),
        })
    }
}
impl ASTNode for UnaryExpression {}
impl Statement for UnaryExpression {}
impl Expression for UnaryExpression {}

