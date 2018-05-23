use std::fmt;
use std::fmt::Display;
use std::str::from_utf8;

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
    pub statements: Vec<Stmt>,
}
impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let statement_iter = self.statements.iter();
        let mapped =
            statement_iter.map( |x| x.to_string());
        let strings = indent_block(mapped.collect::<Vec<String>>().join("\n"));
        write!(f, "Block:\n{}", strings)
    }
}
impl ASTNode for Block {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    AssignmentStmt{identifier: Identifier, operator: Assignment, expression: Expr},
    IfStmt{condition: Expr, main_block: Block, elifs: Vec<(Expr, Block)>, else_block: Option<Block>},
    WhileStmt{condition: Expr, block: Block},
    ForInStmt{iter_var: Identifier, iterator: Expr, block: Block},
    FunctionDecStmt{name: Identifier, args: Vec<Identifier>, body: Block},
    // TODO: Change to be tuples instead of records.
    ImportStmt{module: DottedIdentifier},
    ReturnStmt{value: Expr},
    BreakStmt,
    PassStmt,
    ContinueStmt,
    YieldStmt(Expr),
    // TODO: Fix contents of exception
    TryExceptStmt{main: Block, exception: Block, finally: Option<Block>}
}
impl Display for Stmt {
     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string_rep = match self {
            &Stmt::AssignmentStmt{ref identifier, ref operator, ref expression} =>
                format!("Assignment:\n Name: {} Operator {:?} Expression: {}", identifier, operator, expression),
            &Stmt::WhileStmt {ref condition, ref block} => format!("While statement:\n  Condition: {}\n{}", condition, indent_block(block.to_string())),
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
            },
            &Stmt::ImportStmt {ref module} => {
                format!("Import: module {}", module)
            },
            &Stmt::ReturnStmt {ref value} => {
                format!("Return: value {}", value)
            }
            _ => "Not implemented".to_string()
        };
        write!(f, "{}", string_rep.as_str())
    }
}
impl ASTNode for Stmt {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    MatchExpr{value: Box<Expr>, cases: Vec<(Expr, Expr)>},
    ComparisonExpr{operator: ComparisonOperator, left: Box<Expr>, right: Box<Expr>},
    BinaryExpr{operator: BinaryOperator, left: Box<Expr>, right: Box<Expr>},
    UnaryExpr{operator: UnaryOperator, operand: Box<Expr>},
    FunctionCall{func_expr: Box<Expr>, args: Vec<Expr>},
    AttributeAccess{container: Box<Expr>, attributes: Vec<Identifier>},
    IdentifierExpr{ident: Identifier},
    Bool(Boolean),
    Int(IntegerLiteral),
    Float(FloatLiteral),
    String(String),
    VecComprehension{values: Box<Expr>, iterator_unpacking: Vec<Identifier>, iterator: Box<Expr>},
    MapComprehension{keys: Box<Expr>, values: Box<Expr>, iterator_unpacking: Vec<Identifier>, iterator: Box<Expr>},
    SetComprehension{values: Box<Expr>, iterator_unpacking: Vec<Identifier>, iterator: Box<Expr>}
}
impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string_rep = match self {
            &Expr::ComparisonExpr{ref operator, ref left, ref right} => format!("Comparison:\n Left: {} Op:{} Right: {}", left, operator, right),
            &Expr::BinaryExpr{ref operator, ref left, ref right} => format!("Binary:\n Left: {} Op:{} Right: {}", left, operator, right),
            &Expr::UnaryExpr{ref operator, ref operand} => format!("Unary expression. Operator: {}. Operand: {}", operator, operand),
            &Expr::FunctionCall{ref func_expr, ref args} => {
                let joined_args = args.iter().map(|x| x.to_string()).collect::<Vec<String>>().join(", ");
                format!("Function call. Func: {}. Args: {}", func_expr, joined_args)
            },
            &Expr::AttributeAccess{ref container, ref attributes} => {
                format!("Attribute access. Container: {}. Attributes: {}", container,
                        attributes.iter().map(|x| x.to_string()).collect::<Vec<String>>().join("."))
            },
            &Expr::IdentifierExpr{ref ident} => ident.name.clone(),
            &Expr::Bool(b) => b.to_string(),
            _ => "Not implemented".to_string()
        };
        write!(f, "{}", string_rep.as_str())
    }
}
impl ASTNode for Expr {}
impl <'a> From<&'a str> for Expr {
    fn from(input: &'a str) -> Self {
        return Expr::IdentifierExpr{ident: Identifier::from(input)};
    }
}
impl From<bool> for Expr {
    fn from(input: bool) -> Self {
        return Expr::Bool(Boolean::from(input));
    }
}
impl<'a> From<&'a [u8]> for Expr {
    fn from(input: &'a [u8]) -> Self {
        return Expr::IdentifierExpr {ident: Identifier::from(input)};
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DottedIdentifier {
    pub attributes: Vec<Identifier>
}
impl Display for DottedIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Dotted Ident: {:?}", self.attributes)
    }
}

#[derive (Debug, Clone, PartialEq, Eq)]
pub enum PostIdent {
    Call{args: Vec<Expr>},
    Access{attributes: Vec<Identifier>}
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
impl <'a> From<&'a str> for Identifier {
    fn from(input: &'a str) -> Self {
        return Identifier{name: input.to_string()};
    }
}
impl <'a> From<&'a [u8]> for Identifier {
    fn from(input: &'a [u8]) -> Self {
        let val = match from_utf8(input) {
            Ok(v) => v,
            _ => panic!()
        };
        return Identifier{name: val.to_string()};
    }
}

/// An assignment
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Assignment {
    Normal,
    Add,
    Mult,
    Div,
    Sub,
    Exponent,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    BitShiftL,
    BitShiftR,
}
impl<'a> From<&'a str> for Assignment {
    fn from(input: &'a str) -> Self {
        return match input {
            "=" => Assignment::Normal,
            "+=" => Assignment::Add,
            "-=" => Assignment::Sub,
            "*=" => Assignment::Mult,
            "/=" => Assignment::Div,
            "%=" => Assignment::Mod,
            "&=" => Assignment::BitAnd,
            "|=" => Assignment::BitOr,
            "^=" => Assignment::BitXor,
            "<<=" => Assignment::BitShiftL,
            ">>=" => Assignment::BitShiftR,
            "**=" => Assignment::Exponent,
            _ => {
                // TODO: Log
                println!("Bad input to Assignment::from<&str>: {}", input);
                panic!()
            }
        };
    }
}

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
    Sub,
    Mult,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    BitShiftL,
    BitShiftR,
    Exponent
}
impl Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &BinaryOperator::Or => "or",
            &BinaryOperator::And => "and",
            &BinaryOperator::Xor => "xor",
            &BinaryOperator::Add => "+",
            &BinaryOperator::Sub => "-",
            &BinaryOperator::Mult=> "*",
            &BinaryOperator::Div => "/",
            &BinaryOperator::Mod => "%",
            &BinaryOperator::BitAnd => "&",
            &BinaryOperator::BitOr => "|",
            &BinaryOperator::BitXor => "^",
            &BinaryOperator::BitShiftL => "<<",
            &BinaryOperator::BitShiftR => ">>",
            &BinaryOperator::Exponent => "**",
        })
    }
}
impl<'a> From<&'a str> for BinaryOperator {
    fn from(input: &'a str) -> Self {
        return match input {
            "or" => BinaryOperator::Or,
            "and" => BinaryOperator::And,
            "xor" => BinaryOperator::Xor,
            "+" => BinaryOperator::Add,
            "-" => BinaryOperator::Sub,
            "*" => BinaryOperator::Mult,
            "/" => BinaryOperator::Div,
            "%" => BinaryOperator::Mod,
            "&" => BinaryOperator::BitAnd,
            "|" => BinaryOperator::BitOr,
            "^" => BinaryOperator::BitXor,
            "<<" => BinaryOperator::BitShiftL,
            ">>" => BinaryOperator::BitShiftR,
            "**" => BinaryOperator::Exponent,
            _ => {
                // TODO: Log
                println!("Bad input to BinaryOperator::from<&str>: {}", input);
                panic!()
            }
        };
    }
}

/// This is not the set of unary operators we will end up supporting.
/// This is a random grab bag just to get the UnaryExpression struct working.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    Not,
    Positive,
    Negative,
    BitNot
}
impl Display for UnaryOperator{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &UnaryOperator::Not => "not",
            &UnaryOperator::Positive => "+",
            &UnaryOperator::Negative => "-",
            &UnaryOperator::BitNot => "~",
        })
    }
}
impl <'a> From<&'a str> for UnaryOperator {
    fn from(input: &'a str) -> Self {
        return match input {
            "not" => UnaryOperator::Not,
            "+" => UnaryOperator::Positive,
            "-" => UnaryOperator::Negative,
            "~" => UnaryOperator::BitNot,
            _ => panic!()
        };
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
impl From<bool> for Boolean {
    fn from(input: bool) -> Self {
        return match input {
            true => Boolean::True,
            false => Boolean::False
        };
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntegerLiteral {
    pub string_rep: String
}
impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.string_rep)
    }
}
impl ASTNode for IntegerLiteral {}
impl From<i64> for IntegerLiteral {
    fn from(input: i64) -> Self {
        return IntegerLiteral{string_rep: format!("{}", input)}
    }
}
impl <'a> From<&'a [u8]> for IntegerLiteral {
    fn from(input: &'a [u8]) -> Self {
        let val = match from_utf8(input) {
            Ok(v) => v,
            _ => panic!()
        };
        return IntegerLiteral{string_rep: val.to_string()};
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FloatLiteral {
    pub string_rep: String
}
impl Display for FloatLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.string_rep)
    }
}
impl ASTNode for FloatLiteral {}
impl From<f64> for FloatLiteral {
    fn from(input: f64) -> Self {
        return FloatLiteral{string_rep: format!("{}", input)}
    }
}
impl <'a> From<&'a [u8]> for FloatLiteral {
    fn from(input: &'a [u8]) -> Self {
        let val = match from_utf8(input) {
            Ok(v) => v,
            _ => panic!()
        };
        return FloatLiteral{string_rep: val.to_string()};
    }
}

#[test]
fn test_indent() {
    let block = "Block:\n  Assignment: test2 = true\n  Assignment: bar = false and true".to_string();
    assert_eq!(indent_block(block), "  Block:\n    Assignment: test2 = true\n    Assignment: bar = false and true".to_string())
}
