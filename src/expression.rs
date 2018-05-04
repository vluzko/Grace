use std::fmt;
use std::fmt::Display;

fn indent_block(block_str: String) -> String {
    let split = block_str.lines();
    let mut ret: String = "  ".to_string();
    ret.push_str(&split.collect::<Vec<&str>>().join("\n  "));
    return ret;
}

// TODO: Print subtree
pub trait ASTNode: Display{}
// pub trait Statement: ASTNode {}
// pub trait Expression: Statement {}

/// A block of code. Just a series of statements.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub statements: Vec<Box<Stmt>>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    AssignmentStmt{identifier: Identifier, expression: Expr},
    IfStmt{condition: Expr, main_block: Box<Block>, elifs: Vec<(Expr, Box<Block>)>, else_block: Option<Box<Block>>},
    FunctionDecStmt{name: Identifier, args: Vec<Identifier>, body: Box<Block>}
}
impl Display for Stmt {
     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string_rep = match self {
            &Stmt::AssignmentStmt{ref identifier, ref expression} => format!("Assignment:\n Name: {} Expression: {}", identifier, expression),
            &Stmt::IfStmt{ref condition, ref main_block, ref elifs, ref else_block} => {
                let elifs_iter = elifs.iter();
                let mapped = elifs_iter.map( |x| (*x).1.to_string());
                let strings = indent_block(mapped.collect::<Vec<String>>().join("\n"));

                let else_string = match else_block {
                    &Some(ref x) => {
                        (*x).to_string()
                    },
                    &None => {
                        "".to_string()
                    }
                };
                format!("If statement:\n  Condition: {}.\n{}\nelifs:\n{}\nelse:\n  {}", condition, indent_block(main_block.to_string()), strings, indent_block(else_string))
            },
            &Stmt::FunctionDecStmt{ref name, ref args, ref body} => {
                let arg_iter = args.iter().map(|x| x.to_string());
                let args_string = arg_iter.collect::<Vec<_>>().join(", ");

                format!("Function declaration:\n  Name: {}\n  Args: {}\n{}", name, args_string, indent_block(body.to_string()))
            }
        };
        write!(f, "{}", string_rep.as_str())
    }
}
impl ASTNode for Stmt {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    ComparisonExpr{operator: ComparisonOperator, left: Box<Expr>, right: Box<Expr>},
    BinaryExpr{operator: BinaryOperator, left: Box<Expr>, right: Box<Expr>},
    UnaryExpr{operator: UnaryOperator, operand: Box<Expr>},
    FunctionCall{name: Identifier, args: Vec<Expr>},
    IdentifierExpr{ident: Identifier},
    Bool(Boolean)
}
impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string_rep = match self {
            &Expr::ComparisonExpr{ref operator, ref left, ref right} => format!("Comparison:\n Left: {} Op:{} Right: {}", left, operator, right),
            &Expr::BinaryExpr{ref operator, ref left, ref right} => format!("Binary:\n Left: {} Op:{} Right: {}", left, operator, right),
            &Expr::UnaryExpr{ref operator, ref operand} => format!("Unary expression. Operator: {}. Operand: {}", operator, operand),
            &Expr::FunctionCall{ref name, ref args} => {
                let joined_args = args.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", ");
                format!("Function call. Name: {}. Args: {}", name, joined_args)
            },
            &Expr::IdentifierExpr{ref ident} => ident.name.clone(),
            &Expr::Bool(b) => b.to_string()
        };
        write!(f, "{}", string_rep.as_str())
    }
}
impl ASTNode for Expr {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DottedIdentifier {
    pub names: Vec<String>
}
impl Display for DottedIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Dotted Ident: {:?}", self.names)
    }
}

#[derive (Debug, Clone, PartialEq, Eq)]
pub enum PostIdent {
    Call{args: Vec<Expr>},
    Access{name: Identifier}
}

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

/// Any comparator
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ComparisonOperator {
    Greater,
    Less,
    Equal,
    Unequal,
    GreaterEqual,
    LessEqual
}

impl Display for ComparisonOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

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
            _ => ""
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


#[test]
fn test_indent() {
    let block = "Block:\n  Assignment: test2 = true\n  Assignment: bar = false and true".to_string();
    assert_eq!(indent_block(block), "  Block:\n    Assignment: test2 = true\n    Assignment: bar = false and true".to_string())
}
