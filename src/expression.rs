use std::fmt;
use std::fmt::Display;

pub trait ASTNode: Display {
}

pub trait Expression: ASTNode{
}

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

impl Expression for Boolean {}
impl ASTNode for Boolean {}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOperator {
    Or,
    And,
    Xor,
}

impl Display for BinaryOperator{

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &BinaryOperator::Or => "or",
            &BinaryOperator::And => "and",
            &BinaryOperator::Xor => "xor",
        })
    }
}

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

// Currently Expression only handles boolean expressions
// because they have a fixed size.
// It should be expanded to other types.
// TODO: you will have to switch everything to boxes
pub struct BinaryExpression <'a, 'b> {
	pub operator: BinaryOperator,
	pub left: &'a Expression,
	pub right: &'b Expression
}

impl<'a, 'b> Display for BinaryExpression<'a, 'b> {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} {}", self.left, self.operator, self.right)
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

pub struct UnaryExpression <'a> {
    pub operator: UnaryOperator,
    pub operand: &'a Expression
}

impl<'a> Display for UnaryExpression<'a> {

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

impl<'a> Expression for UnaryExpression<'a> {}
impl<'a> ASTNode for UnaryExpression<'a> {}
