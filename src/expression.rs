use std::fmt;
use std::fmt::Display;


// TODO: Print subtree
pub trait ASTNode: Display {
    fn subtree_as_string(&self) -> &str;
}

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
impl ASTNode for Boolean {
    fn subtree_as_string(&self) -> &str {
        return "";
    }
}
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
impl ASTNode for Identifier {

    fn subtree_as_string(&self) -> &str {
        panic!()
    }
}

pub struct IfStatement {
    pub condition: Box<Expression>,
    pub main_block: Box<Block>,
    pub elifs: Option<Vec<(Box<Expression>, Box<Block>)>>,
    pub else_block: Option<Box<Block>>
}

impl Display for IfStatement {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "If statement.\n  Condition: {}.\n  Block: {}", self.condition, self.main_block)
    }
}
impl ASTNode for IfStatement {
    fn subtree_as_string(&self) -> &str {
        return "";
    }
}
impl Statement for IfStatement {}
impl Expression for IfStatement {}

pub struct Assignment{
    pub identifier: Identifier,
    pub expression: Box<Expression>,
}

impl Display for Assignment{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", self.identifier, self.expression.to_string())
    }
}
impl ASTNode for Assignment{
    fn subtree_as_string(&self) -> &str {
        panic!()
    }

}
impl Statement for Assignment {}

pub struct Block {
    pub statements: Vec<Box<Statement>>,
}

//TODO: put all of the statements not just the first
impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let statement_iter = self.statements.iter();
        let mapped =
            statement_iter.map( |x| (*x).to_string());
        let strings = mapped.collect::<Vec<String>>().join("\n");
        write!(f, "Block containing:\n{}\n", strings)
    }
}

impl ASTNode for Block {
    fn subtree_as_string(&self) -> &str {
        panic!()
    }
}

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
impl ASTNode for BinaryExpression {
    fn subtree_as_string(&self) -> &str {
        panic!()
    }
}
impl Statement for BinaryExpression {}
impl Expression for BinaryExpression {}


// This is not the set of unary operators we will end up supporting.
// This is a random grab bag just to get the UnaryExpression struct working.
#[derive(Debug, Copy, Clone)]
pub enum UnaryOperator {
    Not,
}

impl Display for UnaryOperator{

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &UnaryOperator::Not => "not",
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
        })
    }
}
impl ASTNode for UnaryExpression {
    fn subtree_as_string(&self) -> &str {
        panic!();
    }
}
impl Statement for UnaryExpression {}
impl Expression for UnaryExpression {}

