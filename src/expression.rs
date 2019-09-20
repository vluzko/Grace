use std::fmt;
use std::fmt::Display;
use std::str::from_utf8;
use std::convert::From;

use typing::*;
use position_tracker::PosStr;
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
    pub declarations: Vec<Box<Node<Stmt>>>,
    pub imports: Vec<Box<Node<Import>>>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Import {
    pub path: Vec<Identifier>,
    pub alias: Option<Identifier>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub statements: Vec<Box<Node<Stmt>>>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    AssignmentStmt  {name: Identifier, operator: Assignment, expression: Node<Expr>},
    LetStmt         {typed_name: TypedIdent, expression: Node<Expr>},
    FunctionDecStmt {name: Identifier, args: Vec<(Identifier, Type)>, vararg: Option<Identifier>,
        kwargs: Vec<(Identifier, Type, Node<Expr>)>, varkwarg: Option<Identifier>, block: Node<Block>, return_type: Type},
    IfStmt          {condition: Node<Expr>, block: Node<Block>, elifs: Vec<(Node<Expr>, Node<Block>)>, else_block: Option<Node<Block>>},
    WhileStmt       {condition: Node<Expr>, block: Node<Block>},
    ForInStmt       {iter_vars: Identifier, iterator: Node<Expr>, block: Node<Block>},
    TryExceptStmt   {block: Node<Block>, exceptions: Vec<Node<Block>>, else_block: Option<Node<Block>>, final_block: Option<Node<Block>>},
    ReturnStmt      (Node<Expr>),
    YieldStmt       (Node<Expr>),
    BreakStmt,
    PassStmt,
    ContinueStmt,
}

impl Stmt {
    pub fn get_name(&self) -> Identifier {
        return match self {
            Stmt::AssignmentStmt{ref name, ..} => name.clone(),
            Stmt::LetStmt{ref typed_name, ..} => typed_name.name.clone(),
            Stmt::FunctionDecStmt{ref name, ..} => name.clone(),
            x => panic!("get_name called on an enum value that doesn't have a name: {:?}", x)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    MatchExpr       {value: Box<Node<Expr>>, cases: Vec<(Node<Expr>, Node<Expr>)>},
    ComparisonExpr  {operator: ComparisonOperator, left: Box<Node<Expr>>, right: Box<Node<Expr>>},//TODO take this out
    BinaryExpr      {operator: BinaryOperator, left: Box<Node<Expr>>, right: Box<Node<Expr>>},
    UnaryExpr       {operator: UnaryOperator, operand: Box<Node<Expr>>},
    FunctionCall    {function: Box<Node<Expr>>, args: Vec<Node<Expr>>, kwargs: Vec<(Identifier, Node<Expr>)>},
    AttributeAccess {base: Box<Node<Expr>>, attribute: Identifier},
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
pub struct ComprehensionIter {
    pub iter_vars: Vec<Identifier>,
    pub iterator: Box<Node<Expr>>,
    pub if_clauses: Vec<Node<Expr>>
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
    ToF64,
    ToBool
}

/// An identifier. Alphanumeric characters and underscores. Cannot start with a digit.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedIdent {
    pub name: Identifier,
    pub type_annotation: Option<Type>
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

    /// From for Expr
    impl <'a> From<&'a str> for Expr {
        fn from(input: &'a str) -> Self {
            return Expr::IdentifierExpr(Identifier::from(input));
        }
    }
    impl<'a> From<&'a [u8]> for Expr {
        fn from(input: &'a [u8]) -> Self {
            return Expr::IdentifierExpr (Identifier::from(input));
        }
    }
    impl<'a> From<PosStr<'a>> for Expr {
        fn from(input: PosStr<'a>) -> Self {
            return Expr::from(input.slice);
        }
    }
    impl From<bool> for Expr {
        fn from(input: bool) -> Self {
            return Expr::Bool(input);
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
                    panic!("Bad input to Assignment::from<&str>: {}", input)
                }
            };
        }
    }
    impl<'a> From<&'a [u8]> for Assignment {
        fn from(input: &'a [u8]) -> Self {
            return match input {
                b"=" => Assignment::Normal,
                b"+=" => Assignment::Add,
                b"-=" => Assignment::Sub,
                b"*=" => Assignment::Mult,
                b"/=" => Assignment::Div,
                b"%=" => Assignment::Mod,
                b"&=" => Assignment::BitAnd,
                b"|=" => Assignment::BitOr,
                b"^=" => Assignment::BitXor,
                b"<<=" => Assignment::BitShiftL,
                b">>=" => Assignment::BitShiftR,
                b"**=" => Assignment::Exponent,
                _ => {
                    panic!("Bad input to Assignment::from<&[u8]>: {:?}", input)
                }
            };
        }
    }
    impl<'a> From<PosStr<'a>> for Assignment {
        fn from(input: PosStr<'a>) -> Self {
            return Assignment::from(input.slice);
        }
    }

    /// From for ComparisonOperator
    impl <'a> From<&'a [u8]> for ComparisonOperator {
        fn from(input: &'a [u8]) -> Self {
            return match input {
                b"==" => ComparisonOperator::Equal,
                b">=" => ComparisonOperator::GreaterEqual,
                b"<=" => ComparisonOperator::LessEqual,
                b">"  => ComparisonOperator::Greater,
                b"<"  => ComparisonOperator::Less,
                b"!=" => ComparisonOperator::Unequal,
                _ => panic!(),
            };
        }
    }
    impl <'a> From<PosStr<'a>> for ComparisonOperator {
        fn from(input: PosStr<'a>) -> Self {
            return ComparisonOperator::from(input.slice);
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
    impl<'a> From<&'a [u8]> for BinaryOperator {
        fn from(input: &'a [u8]) -> Self {
            return match input {
                b"or" => BinaryOperator::Or,
                b"and" => BinaryOperator::And,
                b"xor" => BinaryOperator::Xor,
                b"+" => BinaryOperator::Add,
                b"-" => BinaryOperator::Sub,
                b"*" => BinaryOperator::Mult,
                b"/" => BinaryOperator::Div,
                b"%" => BinaryOperator::Mod,
                b"&" => BinaryOperator::BitAnd,
                b"|" => BinaryOperator::BitOr,
                b"^" => BinaryOperator::BitXor,
                b"<<" => BinaryOperator::BitShiftL,
                b">>" => BinaryOperator::BitShiftR,
                b"**" => BinaryOperator::Exponent,
                _ => {
                    panic!("Bad input to BinaryOperator::from<&[u8]>: {:?}", input)
                }
            };
        }
    }
    impl<'a> From<PosStr<'a>> for BinaryOperator {
        fn from(input: PosStr<'a>) -> Self {
            return BinaryOperator::from(input.slice);
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
    impl <'a> From<&'a [u8]> for UnaryOperator {
        fn from(input: &'a [u8]) -> Self {
            return match input {
                b"not" => UnaryOperator::Not,
                b"+" => UnaryOperator::Positive,
                b"-" => UnaryOperator::Negative,
                b"~" => UnaryOperator::BitNot,
                _ => panic!("Bad input to UnaryOperator::from<&[u8]>: {:?}", input)
            };
        }
    }
    impl <'a> From<PosStr<'a>> for UnaryOperator {
        fn from(input: PosStr<'a>) -> Self {
            return UnaryOperator::from(input.slice);
        }
    }

    impl <'a> From<&'a Type> for UnaryOperator {
        fn from(input: &'a Type) -> Self {
            match input {
                Type::i32 => UnaryOperator::ToI32,
                Type::ui32 => UnaryOperator::ToF32,
                Type::i64 => UnaryOperator::ToI64,
                Type::f32 => UnaryOperator::ToF32,
                Type::f64 => UnaryOperator::ToF64,
                Type::boolean => UnaryOperator::ToBool,
                _ => panic!()
            }
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
    
    impl <'a> From<PosStr<'a>> for Identifier {
        fn from(input: PosStr<'a>) -> Self {
            return Identifier::from(input.slice)
        }
    }
}
