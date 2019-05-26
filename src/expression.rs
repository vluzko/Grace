use std::fmt;
use std::fmt::Display;
use std::str::from_utf8;
use std::convert::From;

use scoping::*;
use typing::*;
use general_utils;

#[derive(Debug, Clone, Eq, Hash)]
pub struct Node<T> {
    pub id: usize,
    pub data: T,
    pub scope: usize
}

impl <T> PartialEq for Node<T> where T:PartialEq {
    fn eq(&self, other: &Node<T>) -> bool {
        return self.data == other.data && self.scope == other.scope;
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub declarations: Vec<Node<Stmt>>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub statements: Vec<Node<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    AssignmentStmt  {name: Identifier, operator: Assignment, expression: Node<Expr>},
    LetStmt         {typed_name: TypedIdent, expression: Node<Expr>},
    IfStmt          {condition: Node<Expr>, block: Node<Block>, elifs: Vec<(Node<Expr>, Node<Block>)>, else_block: Option<Node<Block>>},
    WhileStmt       {condition: Node<Expr>, block: Node<Block>},
    ForInStmt       {iter_vars: Identifier, iterator: Node<Expr>, block: Node<Block>},
    FunctionDecStmt {name: Identifier, args: Vec<TypedIdent>, vararg: Option<Identifier>,
        kwargs: Vec<(TypedIdent, Node<Expr>)>, varkwarg: Option<Identifier>, block: Node<Block>, return_type: Option<TypeAnnotation>},
    TryExceptStmt   {block: Node<Block>, exceptions: Vec<Node<Block>>, else_block: Option<Node<Block>>, final_block: Option<Node<Block>>},
    ImportStmt      (DottedIdentifier),
    ReturnStmt      (Node<Expr>),
    YieldStmt       (Node<Expr>),
    BreakStmt,
    PassStmt,
    ContinueStmt,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    MatchExpr       {value: Box<Node<Expr>>, cases: Vec<(Node<Expr>, Node<Expr>)>},
    ComparisonExpr  {operator: ComparisonOperator, left: Box<Node<Expr>>, right: Box<Node<Expr>>},//TODO take this out
    BinaryExpr      {operator: BinaryOperator, left: Box<Node<Expr>>, right: Box<Node<Expr>>},
    UnaryExpr       {operator: UnaryOperator, operand: Box<Node<Expr>>},
    FunctionCall    {function: Box<Node<Expr>>, args: Vec<Node<Expr>>, kwargs: Vec<(Identifier, Node<Expr>)>},
    AttributeAccess {base: Box<Node<Expr>>, attributes: Vec<Identifier>},
    Index           {slices: Vec<(Option<Node<Expr>>, Option<Node<Expr>>, Option<Node<Expr>>)>},
    VecComprehension{values: Box<Node<Expr>>, iterators: Vec<ComprehensionIter>},
    GenComprehension{values: Box<Node<Expr>>, iterators: Vec<ComprehensionIter>},
    MapComprehension{keys: Box<Node<Expr>>, values: Box<Node<Expr>>, iterators: Vec<ComprehensionIter>},
    SetComprehension{values: Box<Node<Expr>>, iterators: Vec<ComprehensionIter>},
    IdentifierExpr  (Identifier),
    Bool            (bool),
    Int             (String),
    Float           (String),
    String          (String),
    VecLiteral      (Vec<Node<Expr>>),
    SetLiteral      (Vec<Node<Expr>>),
    TupleLiteral    (Vec<Node<Expr>>),
    MapLiteral      (Vec<(Identifier, Node<Expr>)>)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeAnnotation {
    Simple(Identifier)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComprehensionIter {
    pub iter_vars: Vec<Identifier>,
    pub iterator: Box<Node<Expr>>,
    pub if_clauses: Vec<Node<Expr>>
}

/// A helper Enum for trailers.
#[derive (Debug, Clone, PartialEq, Eq, Hash)]
pub enum PostIdent {
    Call{args: Vec<Node<Expr>>, kwargs: Vec<(Identifier, Node<Expr>)>},
    Index{slices: Vec<(Option<Node<Expr>>, Option<Node<Expr>>, Option<Node<Expr>>)>},
    Access{attributes: Vec<Identifier>}
}

/// An assignment
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
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

/// Any comparator
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ComparisonOperator {
    Greater,
    Less,
    Equal,
    Unequal,
    GreaterEqual,
    LessEqual
}

/// Any binary operator
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
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

/// Any unary operator.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Not,
    Positive,
    Negative,
    BitNot,
    ToI32,
    ToUi32,
    ToI64,
    ToUi64,
    ToF32,
    ToF64
}

/// A dotted identifier. Only used with import statements.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DottedIdentifier {
    pub attributes: Vec<Identifier>
}

/// An identifier. Alphanumeric characters and underscores. Cannot start with a digit.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedIdent {
    pub name: Identifier,
    pub type_annotation: Option<TypeAnnotation>
}

pub mod trait_impls {
    use super::*;
    /// Impl for Nodes
    impl <T> Node<T> {
        pub fn replace(&self, new_data: T) -> Node<T> {
            return Node {
                id: self.id,
                data: new_data,
                scope: self.scope.clone()
            };
        }
    }

    impl Node<Expr> {
        pub fn get_type(&self) -> Type {
            return self.data.get_type();
        }
    }

    /// Impl for Expr
    impl Expr {
        pub fn get_type(&self) -> Type {
            match self {
                &Expr::String (..) => Type::string,
                &Expr::Bool (..) => Type::boolean,
                &Expr::Int (..) => Type::i32,
                &Expr::Float (..) => Type::f64,
                &Expr::UnaryExpr {ref operator, ref operand, ..} => {
                    match operator {
                        &UnaryOperator::ToF32 => Type::f32,
                        &UnaryOperator::ToF64 => Type::f64,
                        &UnaryOperator::ToI32 => Type::i32,
                        &UnaryOperator::ToI64 => Type::i64,
                        _ => operand.get_type()
                    }
                },
                &Expr::BinaryExpr {ref operator, ref left, ref right, ..} => {
                    return operator.get_return_type(&left.get_type(), &right.get_type());
                },
                &Expr::IdentifierExpr(ref name) => {
                    // Get declaration
                    panic!()
                },
                _ => {
                    println!("{:?}", self);
                    panic!()
                }
            }
        }
    }

    /// Display implementations
    impl Display for TypedIdent {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{} (typed)", self.name)
        }
    }

    impl Display for Identifier {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", self.name)
        }
    }

    /// From implementations

    /// From for Node
    impl <T> From<T> for Node<T> {
        fn from(input: T) -> Self {
            return Node{
                id: general_utils::get_next_id(),
                data: input,
                scope: 0
            };
        }
    }

    impl From<bool> for Node<Expr> {
        fn from(input: bool) -> Self {
            let expr = Expr::from(input);
            return Node {
                id: general_utils::get_next_id(),
                data: expr,
                scope: 0
            }
        }
    }

    impl <'a> From<&'a str> for Node<Expr> {
        fn from(input: &'a str) -> Self {
            let expr: Expr = Expr::from(input);
            return Node {
                id: general_utils::get_next_id(),
                data: expr,
                scope: 0
            }
        }
    }

    impl From<i64> for Node<Expr> {
        fn from(input: i64) -> Self {
            let expr: Expr = Expr::from(input);
            return Node {
                id: general_utils::get_next_id(),
                data: expr,
                scope: 0
            }
        }
    }

    impl From<f64> for Node<Expr> {
        fn from(input: f64) -> Self {
            let expr: Expr = Expr::from(input);
            return Node {
                id: general_utils::get_next_id(),
                data: expr,
                scope: 0
            }
        }
    }

    /// From for UnaryOperator
    impl <'a> From<&'a Type> for UnaryOperator {
        fn from(input: &'a Type) -> Self {
            match input {
                Type::i32 => UnaryOperator::ToI32,
                Type::ui32 => UnaryOperator::ToF32,
                Type::i64 => UnaryOperator::ToI64,
                Type::f32 => UnaryOperator::ToF32,
                Type::f64 => UnaryOperator::ToF64,
                _ => panic!()
            }
        }
    }

    /// From for Expr
    impl <'a> From<&'a str> for Expr {
        fn from(input: &'a str) -> Self {
            return Expr::IdentifierExpr(Identifier::from(input));
        }
    }
    impl From<bool> for Expr {
        fn from(input: bool) -> Self {
            return Expr::Bool(input);
        }
    }
    impl<'a> From<&'a [u8]> for Expr {
        fn from(input: &'a [u8]) -> Self {
            return Expr::IdentifierExpr (Identifier::from(input));
        }
    }
    impl From<i64> for Expr {
        fn from(input: i64) -> Self {
            return Expr::Int(input.to_string());
        }
    }
    impl From<f64> for Expr {
        fn from(input: f64) -> Self {
            return Expr::Float(input.to_string());
        }
    }

    /// From for Assignment
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

    /// From for BinaryOperator
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

    /// From for UnaryOperator
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

    /// From for TypedIdent
    impl <'a> From<&'a str> for TypedIdent {
        fn from(input: &'a str) -> Self {
            return TypedIdent{name: Identifier::from(input), type_annotation: None};
        }
    }

    /// From for Identifier
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

    /// From for TypeAnnotation
    impl <'a> From<&'a str> for TypeAnnotation {
        fn from(input: &'a str) -> Self {
            return TypeAnnotation::Simple(Identifier::from(input));
        }
    }
}

