//! The Grace high-level AST.
use std::collections::HashMap;
use std::convert::From;
use std::fmt;
use std::fmt::Display;
use std::str::from_utf8;

use itertools::join;

use general_utils;
use type_checking::types::{Trait, Type};

#[allow(clippy::derived_hash_with_manual_eq)]
/// AST node.
/// Keeps track of line, column, and scope information.
#[derive(Debug, Clone, Eq, Hash)]
pub struct Node<T> {
    pub id: usize,
    /// The line the node starts on
    pub start_line: u32,
    /// The column the node starts on
    pub start_col: u32,
    /// The line the node ends on
    pub end_line: u32,
    /// The column the node ends on
    pub end_col: u32,
    pub data: T,
    pub scope: usize,
}

impl<T> Node<T> {
    pub fn replace<S>(&self, new_data: S) -> Node<S> {
        Node {
            id: self.id,
            start_line: self.start_line,
            start_col: self.start_col,
            end_line: self.end_line,
            end_col: self.end_col,
            data: new_data,
            scope: self.scope,
        }
    }
}

/// A module (a file) in Grace.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub functions: Vec<Box<Node<Stmt>>>,
    pub imports: Vec<Box<Import>>,
    pub structs: Vec<Box<Node<Stmt>>>,
    // Map from trait_name to trait
    pub traits: HashMap<Identifier, Trait>,
    // (trait_name, internal_type_name (often a struct name), function_declarations)
    pub trait_implementations: Vec<(Identifier, Identifier, Vec<Node<Stmt>>)>,
}

#[allow(clippy::derived_hash_with_manual_eq)]
#[derive(Debug, Clone, Eq, Hash)]
pub struct Import {
    pub id: usize,
    pub path: Vec<Identifier>,
    pub alias: Option<Identifier>,
    pub values: Vec<Identifier>,
}

impl Import {
    /// Get a string referring to the imported value.
    pub fn string_ref(&self) -> String {
        return match &self.alias {
            Some(x) => x.name.clone(),
            None => join(self.path.iter().map(|x| x.name.clone()), "."),
        };
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub statements: Vec<Box<Node<Stmt>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    AssignmentStmt {
        name: Identifier,
        expression: Node<Expr>,
    },
    LetStmt {
        name: Identifier,
        type_annotation: Option<Type>,
        expression: Node<Expr>,
    },
    FunctionDecStmt {
        name: Identifier,
        args: Vec<(Identifier, Type)>,
        kwargs: Vec<(Identifier, Type, Node<Expr>)>,
        block: Node<Block>,
        return_type: Type,
    },
    StructDec {
        name: Identifier,
        fields: Vec<(Identifier, Type)>,
    },
    IfStmt {
        condition: Node<Expr>,
        block: Node<Block>,
        else_block: Option<Node<Block>>,
    },
    WhileStmt {
        condition: Node<Expr>,
        block: Node<Block>,
    },
    ReturnStmt(Node<Expr>),
    YieldStmt(Node<Expr>),
    BreakStmt,
    PassStmt,
    ContinueStmt,
}

impl Stmt {
    pub fn get_name(&self) -> Identifier {
        match self {
            Stmt::AssignmentStmt { ref name, .. } => name.clone(),
            Stmt::LetStmt { ref name, .. } => name.clone(),
            Stmt::FunctionDecStmt { ref name, .. } => name.clone(),
            Stmt::StructDec { ref name, .. } => name.clone(),
            x => panic!(
                "get_name called on an enum value that doesn't have a name: {:?}",
                x
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    BinaryExpr {
        operator: BinaryOperator,
        left: Box<Node<Expr>>,
        right: Box<Node<Expr>>,
    },
    UnaryExpr {
        operator: UnaryOperator,
        operand: Box<Node<Expr>>,
    },
    FunctionCall {
        function: Box<Node<Expr>>,
        args: Vec<Node<Expr>>,
        kwargs: Vec<(Identifier, Node<Expr>)>,
    },
    StructLiteral {
        base: Box<Node<Expr>>,
        fields: Vec<Node<Expr>>,
    },
    AttributeAccess {
        base: Box<Node<Expr>>,
        attribute: Identifier,
    },
    TraitAccess {
        base: Box<Node<Expr>>,
        trait_name: Identifier,
        attribute: Identifier,
    },
    Index {
        base: Box<Node<Expr>>,
        #[allow(clippy::type_complexity)]
        slices: Vec<(Option<Node<Expr>>, Option<Node<Expr>>, Option<Node<Expr>>)>,
    },
    ModuleAccess(usize, Vec<Identifier>),
    IdentifierExpr(Identifier),
    Bool(bool),
    Int(String),
    Float(String),
    String(String),
    VecLiteral(Vec<Node<Expr>>),
    TupleLiteral(Vec<Node<Expr>>),
}

impl Expr {
    pub fn flatten_to_module(&self) -> String {
        match self {
            Expr::IdentifierExpr(ref ident) => ident.name.clone(),
            Expr::AttributeAccess {
                ref base,
                ref attribute,
            } => {
                let base_str = base.data.flatten_to_module();
                format!("{}.{}", base_str, attribute.name)
            }
            _ => panic!(),
        }
    }
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
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ComparisonOperator {
    Greater,
    Less,
    Equal,
    Unequal,
    GreaterEqual,
    LessEqual,
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
    Exponent,
    Greater,
    Less,
    Equal,
    Unequal,
    GreaterEqual,
    LessEqual,
}

/// Any unary operator.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Not,
    Positive,
    Negative,
    BitNot,
}

/// An identifier. Alphanumeric characters and underscores. Cannot start with a digit.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Identifier {
    pub name: String,
}

pub fn wrap<T>(data: T) -> Box<Node<T>> {
    Box::new(Node::from(data))
}

/// Various helpful constructors for `Expr` and `Stmt`.
pub mod constructors {
    use super::*;

    impl Stmt {
        /// Construct an if statement from a condition and a block.
        pub fn simple_if<T>(condition: T, block: Block) -> Stmt
        where
            Node<Expr>: From<T>,
        {
            Stmt::IfStmt {
                condition: Node::from(condition),
                block: Node {
                    id: general_utils::get_next_id(),
                    start_line: 0,
                    start_col: 0,
                    end_line: 0,
                    end_col: 0,
                    data: block,
                    scope: 0,
                },
                else_block: None,
            }
        }

        /// Construct an assignment statement from an identifier and an expression.
        pub fn assn<T, S>(name: T, expr: S) -> Stmt
        where
            Identifier: From<T>,
            Expr: From<S>,
        {
            Stmt::AssignmentStmt {
                name: Identifier::from(name),
                expression: Node::from(Expr::from(expr)),
            }
        }

        /// Construct a no-argument function declaration from a name, a block, and a return type.
        pub fn no_args<T>(name: T, block: Block, ret: Type) -> Stmt
        where
            Identifier: From<T>,
        {
            Stmt::FunctionDecStmt {
                name: Identifier::from(name),
                args: vec![],
                kwargs: vec![],
                block: Node::from(block),
                return_type: ret,
            }
        }

        /// Construct a return statement from an expression.
        pub fn ret<T>(expr: T) -> Stmt
        where
            Expr: From<T>,
        {
            Stmt::ReturnStmt(Node::from(Expr::from(expr)))
        }
    }

    impl Expr {
        pub fn access<T>(&self, name: &T) -> Expr
        where
            Identifier: From<T>,
            T: Clone,
        {
            Expr::AttributeAccess {
                base: wrap(self.clone()),
                attribute: Identifier::from(name.clone()),
            }
        }

        /// Create an empty call expression.
        pub fn call(&self) -> Expr {
            Expr::FunctionCall {
                function: wrap(self.clone()),
                args: vec![],
                kwargs: vec![],
            }
        }

        /// Create a call with the passed arguments.
        pub fn callw(&self, args: Vec<Node<Expr>>) -> Expr {
            Expr::FunctionCall {
                function: wrap(self.clone()),
                args,
                kwargs: vec![],
            }
        }
    }

    impl Identifier {
        /// Create a LetStmt assigning `expr` to `self`.
        pub fn simple_let(&self, expr: Expr) -> Stmt {
            Stmt::LetStmt {
                name: self.clone(),
                type_annotation: None,
                expression: Node::from(expr),
            }
        }

        /// Create an AssignmentStmt assigning `expr` to `self`.
        pub fn assn(&self, expr: Expr) -> Stmt {
            Stmt::AssignmentStmt {
                name: self.clone(),
                expression: Node::from(expr),
            }
        }

        /// Create an `IdentifierExpr` referring to `self`.
        pub fn as_expr(&self) -> Expr {
            Expr::IdentifierExpr(self.clone())
        }
    }
}

/// Implementations of common traits.
pub mod rust_trait_impls {
    use super::*;

    /// PartialEq implementations

    /// We don't compare the ids of Nodes.
    impl<T> PartialEq for Node<T>
    where
        T: PartialEq,
    {
        fn eq(&self, other: &Node<T>) -> bool {
            self.data == other.data && self.scope == other.scope
        }
    }

    /// We don't compare the ids of import statements.
    impl PartialEq for Import {
        fn eq(&self, other: &Import) -> bool {
            self.path == other.path && self.alias == other.alias && self.values == other.values
        }
    }

    /// Display implementations
    mod display_impl {
        use super::*;
        /// Display *just* the name of the identifier, without accompanying metadata.
        impl Display for Identifier {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.name)
            }
        }

        /// Display *just* the operator, without accompanying metadata.
        impl Display for BinaryOperator {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(
                    f,
                    "{}",
                    match self {
                        BinaryOperator::Or => " or ",
                        BinaryOperator::And => " and ",
                        BinaryOperator::Xor => " xor ",
                        BinaryOperator::Add => "+",
                        BinaryOperator::Sub => "-",
                        BinaryOperator::Mult => "*",
                        BinaryOperator::Div => "/",
                        BinaryOperator::Mod => "%",
                        BinaryOperator::BitAnd => "&",
                        BinaryOperator::BitOr => "|",
                        BinaryOperator::BitXor => "^",
                        BinaryOperator::BitShiftL => "<<",
                        BinaryOperator::BitShiftR => ">>",
                        BinaryOperator::Exponent => "**",
                        BinaryOperator::Greater => ">",
                        BinaryOperator::Less => "<",
                        BinaryOperator::Equal => "==",
                        BinaryOperator::Unequal => "!=",
                        BinaryOperator::GreaterEqual => ">=",
                        BinaryOperator::LessEqual => "<=",
                    }
                )
            }
        }

        /// Display *just* the operator, without accompanying metadata.
        impl Display for ComparisonOperator {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(
                    f,
                    "{}",
                    match self {
                        ComparisonOperator::Greater => ">",
                        ComparisonOperator::Less => "<",
                        ComparisonOperator::Equal => "==",
                        ComparisonOperator::Unequal => "!=",
                        ComparisonOperator::GreaterEqual => ">=",
                        ComparisonOperator::LessEqual => "<=",
                    }
                )
            }
        }

        /// Display *just* the operator, without accompanying metadata.
        impl Display for UnaryOperator {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(
                    f,
                    "{}",
                    match self {
                        UnaryOperator::Positive => "+",
                        UnaryOperator::Negative => "-",
                        UnaryOperator::Not => "not ",
                        UnaryOperator::BitNot => "~",
                    }
                )
            }
        }
    }

    /// From implementations
    mod from_impl {
        use super::*;
        use parser::position_tracker::PosStr;

        /// From for Node

        impl<T> From<(T, u32, u32, u32, u32)> for Node<T> {
            fn from(input: (T, u32, u32, u32, u32)) -> Self {
                Node {
                    id: general_utils::get_next_id(),
                    start_line: input.1,
                    start_col: input.2,
                    end_line: input.3,
                    end_col: input.4,
                    data: input.0,
                    scope: 0,
                }
            }
        }

        impl<T> From<T> for Node<T> {
            fn from(input: T) -> Self {
                Node {
                    id: general_utils::get_next_id(),
                    start_line: 0,
                    start_col: 0,
                    end_line: 0,
                    end_col: 0,
                    data: input,
                    scope: 0,
                }
            }
        }

        impl From<bool> for Node<Expr> {
            fn from(input: bool) -> Self {
                let expr = Expr::from(input);
                Node {
                    id: general_utils::get_next_id(),
                    start_line: 0,
                    start_col: 0,
                    end_line: 0,
                    end_col: 0,
                    data: expr,
                    scope: 0,
                }
            }
        }

        impl<'a> From<&'a str> for Node<Expr> {
            fn from(input: &'a str) -> Self {
                let expr: Expr = Expr::from(input);
                Node {
                    id: general_utils::get_next_id(),
                    start_line: 0,
                    start_col: 0,
                    end_line: 0,
                    end_col: 0,
                    data: expr,
                    scope: 0,
                }
            }
        }

        impl From<i32> for Node<Expr> {
            fn from(input: i32) -> Self {
                let expr: Expr = Expr::from(input);
                Node {
                    id: general_utils::get_next_id(),
                    start_line: 0,
                    start_col: 0,
                    end_line: 0,
                    end_col: 0,
                    data: expr,
                    scope: 0,
                }
            }
        }
        impl From<i64> for Node<Expr> {
            fn from(input: i64) -> Self {
                let expr: Expr = Expr::from(input);
                Node {
                    id: general_utils::get_next_id(),
                    start_line: 0,
                    start_col: 0,
                    end_line: 0,
                    end_col: 0,
                    data: expr,
                    scope: 0,
                }
            }
        }
        impl From<Identifier> for Node<Expr> {
            fn from(input: Identifier) -> Self {
                let expr: Expr = Expr::from(input);
                Node {
                    id: general_utils::get_next_id(),
                    start_line: 0,
                    start_col: 0,
                    end_line: 0,
                    end_col: 0,
                    data: expr,
                    scope: 0,
                }
            }
        }
        impl From<f64> for Node<Expr> {
            fn from(input: f64) -> Self {
                let expr: Expr = Expr::from(input);
                Node {
                    id: general_utils::get_next_id(),
                    start_line: 0,
                    start_col: 0,
                    end_line: 0,
                    end_col: 0,
                    data: expr,
                    scope: 0,
                }
            }
        }

        /// From for Expr
        impl<'a> From<&'a str> for Expr {
            fn from(input: &'a str) -> Self {
                Expr::IdentifierExpr(Identifier::from(input))
            }
        }
        impl<'a> From<&'a [u8]> for Expr {
            fn from(input: &'a [u8]) -> Self {
                Expr::IdentifierExpr(Identifier::from(input))
            }
        }
        impl<'a> From<PosStr<'a>> for Expr {
            fn from(input: PosStr<'a>) -> Self {
                Expr::from(input.slice)
            }
        }
        impl From<bool> for Expr {
            fn from(input: bool) -> Self {
                Expr::Bool(input)
            }
        }
        impl From<i32> for Expr {
            fn from(input: i32) -> Self {
                Expr::Int(input.to_string())
            }
        }
        impl From<i64> for Expr {
            fn from(input: i64) -> Self {
                Expr::Int(input.to_string())
            }
        }
        impl From<f64> for Expr {
            fn from(input: f64) -> Self {
                Expr::Float(input.to_string())
            }
        }
        impl From<Identifier> for Expr {
            fn from(input: Identifier) -> Self {
                Expr::IdentifierExpr(input)
            }
        }

        /// From for Assignment
        impl<'a> From<&'a str> for Assignment {
            fn from(input: &'a str) -> Self {
                match input {
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
                }
            }
        }
        impl<'a> From<&'a [u8]> for Assignment {
            fn from(input: &'a [u8]) -> Self {
                match input {
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
                }
            }
        }
        impl<'a> From<PosStr<'a>> for Assignment {
            fn from(input: PosStr<'a>) -> Self {
                Assignment::from(input.slice)
            }
        }

        /// From for ComparisonOperator
        impl<'a> From<&'a [u8]> for ComparisonOperator {
            fn from(input: &'a [u8]) -> Self {
                match input {
                    b"==" => ComparisonOperator::Equal,
                    b">=" => ComparisonOperator::GreaterEqual,
                    b"<=" => ComparisonOperator::LessEqual,
                    b">" => ComparisonOperator::Greater,
                    b"<" => ComparisonOperator::Less,
                    b"!=" => ComparisonOperator::Unequal,
                    _ => panic!(),
                }
            }
        }
        impl<'a> From<PosStr<'a>> for ComparisonOperator {
            fn from(input: PosStr<'a>) -> Self {
                ComparisonOperator::from(input.slice)
            }
        }

        /// From for BinaryOperator
        impl<'a> From<&'a str> for BinaryOperator {
            fn from(input: &'a str) -> Self {
                match input {
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
                    "==" => BinaryOperator::Equal,
                    ">=" => BinaryOperator::GreaterEqual,
                    "<=" => BinaryOperator::LessEqual,
                    ">" => BinaryOperator::Greater,
                    "<" => BinaryOperator::Less,
                    "!=" => BinaryOperator::Unequal,
                    _ => {
                        // TODO: Log
                        panic!("Bad input to BinaryOperator::from<&str>: {}", input)
                    }
                }
            }
        }
        impl<'a> From<&'a [u8]> for BinaryOperator {
            fn from(input: &'a [u8]) -> Self {
                match input {
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
                    b"==" => BinaryOperator::Equal,
                    b">=" => BinaryOperator::GreaterEqual,
                    b"<=" => BinaryOperator::LessEqual,
                    b">" => BinaryOperator::Greater,
                    b"<" => BinaryOperator::Less,
                    b"!=" => BinaryOperator::Unequal,
                    _ => {
                        panic!("Bad input to BinaryOperator::from<&[u8]>: {:?}", input)
                    }
                }
            }
        }
        impl<'a> From<PosStr<'a>> for BinaryOperator {
            fn from(input: PosStr<'a>) -> Self {
                BinaryOperator::from(input.slice)
            }
        }

        /// From for UnaryOperator
        impl<'a> From<&'a str> for UnaryOperator {
            fn from(input: &'a str) -> Self {
                match input {
                    "not" => UnaryOperator::Not,
                    "+" => UnaryOperator::Positive,
                    "-" => UnaryOperator::Negative,
                    "~" => UnaryOperator::BitNot,
                    _ => panic!(),
                }
            }
        }
        impl<'a> From<&'a [u8]> for UnaryOperator {
            fn from(input: &'a [u8]) -> Self {
                match input {
                    b"not" => UnaryOperator::Not,
                    b"+" => UnaryOperator::Positive,
                    b"-" => UnaryOperator::Negative,
                    b"~" => UnaryOperator::BitNot,
                    _ => panic!("Bad input to UnaryOperator::from<&[u8]>: {:?}", input),
                }
            }
        }
        impl<'a> From<PosStr<'a>> for UnaryOperator {
            fn from(input: PosStr<'a>) -> Self {
                UnaryOperator::from(input.slice)
            }
        }

        /// From for Identifier
        impl<'a> From<&'a str> for Identifier {
            fn from(input: &'a str) -> Self {
                Identifier {
                    name: input.to_string(),
                }
            }
        }

        impl<'a> From<&'a [u8]> for Identifier {
            fn from(input: &'a [u8]) -> Self {
                let val = match from_utf8(input) {
                    Ok(v) => v,
                    _ => panic!(),
                };
                Identifier {
                    name: val.to_string(),
                }
            }
        }

        impl<'a> From<PosStr<'a>> for Identifier {
            fn from(input: PosStr<'a>) -> Self {
                Identifier::from(input.slice)
            }
        }

        impl From<String> for Identifier {
            fn from(input: String) -> Self {
                Identifier { name: input }
            }
        }
    }
}
