use std::fmt;
use std::fmt::Display;

fn indent_block(block_str: String) -> String {
    let split = block_str.lines();
    let mut ret: String = "  ".to_string();
    ret.push_str(&split.collect::<Vec<&str>>().join("\n  "));
    return ret;
}

// TODO: Print subtree
pub trait ASTNode: Display {}
pub trait Statement: ASTNode {}
pub trait Expression: Statement {}

/// A block of code. Just a series of statements.
pub struct Block {
    pub statements: Vec<Box<Statement>>,
}
impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let statement_iter = self.statements.iter();
        let mapped =
            statement_iter.map( |x| (*x).to_string());
        let strings = indent_block(mapped.collect::<Vec<String>>().join("\n"));
        write!(f, "Block:\n{}", strings)
    }
}
impl ASTNode for Block {}

/// A named function declaration.
pub struct FunctionDec {
    pub name: Identifier,
    pub args: Vec<Identifier>,
    pub body: Box<Block>
}
impl Display for FunctionDec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let arg_iter = self.args.iter().map(|x| x.to_string());
        let args_string = arg_iter.collect::<Vec<_>>().join(", ");

        write!(f, "Function declaration:\n  Name: {}\n  Args: {}\n{}", self.name, args_string, indent_block(self.body.to_string()))
    }
}
impl ASTNode for FunctionDec {}
impl Statement for FunctionDec {}

/// An if statement. Supports elif and else, but neither is required.
pub struct IfStatement {
    pub condition: Expr,
    pub main_block: Box<Block>,
    pub elifs: Vec<(Expr, Box<Block>)>,
    pub else_block: Option<Box<Block>>
}
impl Display for IfStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let elifs_iter = self.elifs.iter();
        let mapped = elifs_iter.map( |x| (*x).1.to_string());
        let strings = indent_block(mapped.collect::<Vec<String>>().join("\n"));

        let else_string = match self.else_block {
            Some(ref x) => {
                (*x).to_string()
            },
            None => {
                "".to_string()
            }
        };
        write!(f, "If statement:\n  Condition: {}.\n{}\nelifs:\n{}\nelse:\n  {}", self.condition, indent_block(self.main_block.to_string()), strings, indent_block(else_string))
    }
}
impl ASTNode for IfStatement {}
impl Statement for IfStatement {}
impl Expression for IfStatement {}

/// An assignment statement.
pub struct Assignment {
    pub identifier: Identifier,
    pub expression: Expr,
}
impl Display for Assignment{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Assignment: {} = {}", self.identifier, self.expression.to_string())
    }
}
impl ASTNode for Assignment{}
impl Statement for Assignment {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    BinaryExpr{operator: BinaryOperator, left: Box<Expr>, right: Box<Expr>},
    UnaryExpr{operator: UnaryOperator, operand: Box<Expr>},
    FunctionCall{name: Identifier, args: Vec<Identifier>},
    IdentifierExpr{ident: Identifier},
    Bool(Boolean)
}
impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string_rep = match self {
            &Expr::BinaryExpr{ref operator, ref left, ref right} => format!("Binary:\n Left: {} Op:{} Right: {}", left, operator, right),
            &Expr::UnaryExpr{ref operator, ref operand} => format!("Unary expression. Operator: {}. Operand: {}", operator, operand),
            &Expr::FunctionCall{ref name, ref args} => {
                let joined_args = args.iter().map(|x| x.name.clone()).collect::<Vec<String>>().join(", ");
                format!("Function call. Name: {}. Args: {}", name, joined_args)
            },
            &Expr::IdentifierExpr{ref ident} => ident.name.clone(),
            &Expr::Bool(b) => b.to_string()
        };
        write!(f, "{}", string_rep.as_str())
    }
}
impl ASTNode for Expr {}

//fn display_unary_expr()

/// An identifier. Alphanumeric characters and underscores. Cannot start with a digit.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
}
impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
impl ASTNode for Identifier {}

/// Currently Expression only handles boolean expressions
/// because they have a fixed size.
/// It should be expanded to other types.
pub struct BinaryExpression {
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


/// Any binary operator
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
    Or,
    And,
    Xor,
    Add,
    Mult,
    Sub,
    Div,
    Mod,

}
impl Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &BinaryOperator::Or => "or",
            &BinaryOperator::And => "and",
            &BinaryOperator::Xor => "xor",
            x => ""
        })
    }
}

/// This is not the set of unary operators we will end up supporting.
/// This is a random grab bag just to get the UnaryExpression struct working.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
impl Display for UnaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self.operator {
            UnaryOperator::Not => format!("{} {}", self.operator.to_string(), self.operand.to_string()),
        })
    }
}
impl ASTNode for UnaryExpression {}
impl Statement for UnaryExpression {}
impl Expression for UnaryExpression {}

/// A boolean value.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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


#[test]
fn test_indent() {
    let block = "Block:\n  Assignment: test2 = true\n  Assignment: bar = false and true".to_string();
    assert_eq!(indent_block(block), "  Block:\n    Assignment: test2 = true\n    Assignment: bar = false and true".to_string())
}
