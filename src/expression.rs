use std::fmt;
use std::fmt::Display;
use std::str::from_utf8;
use std::convert::From;

use compiler_layers::get_next_id;
use scoping::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedIdent {
    pub name: Identifier,
    pub type_annotation: Option<TypeAnnotation>
}

#[derive(Debug, Clone, Eq, Hash)]
pub struct IdNode<T> {
    pub id: u64,
    pub data: T,
    pub scope: Scope
}

impl <T> PartialEq for IdNode<T> where T:PartialEq {
    fn eq(&self, other: &IdNode<T>) -> bool {
        return self.data == other.data && self.scope == other.scope;
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub declarations: Vec<IdNode<Stmt>>
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub statements: Vec<IdNode<Stmt>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    AssignmentStmt  {name: Identifier, operator: Assignment, expression: IdNode<Expr>},
    LetStmt         {typed_name: TypedIdent, expression: IdNode<Expr>},
    IfStmt          {condition: IdNode<Expr>, block: IdNode<Block>, elifs: Vec<(IdNode<Expr>, IdNode<Block>)>, else_block: Option<IdNode<Block>>},
    WhileStmt       {condition: IdNode<Expr>, block: IdNode<Block>},
    ForInStmt       {iter_vars: Identifier, iterator: IdNode<Expr>, block: IdNode<Block>},
    FunctionDecStmt {name: Identifier, args: Vec<TypedIdent>, vararg: Option<Identifier>,
        kwargs: Vec<(TypedIdent, IdNode<Expr>)>, varkwarg: Option<Identifier>, block: IdNode<Block>, return_type: Option<TypeAnnotation>},
    TryExceptStmt   {block: IdNode<Block>, exceptions: Vec<IdNode<Block>>, else_block: Option<IdNode<Block>>, final_block: Option<IdNode<Block>>},
    ImportStmt      (DottedIdentifier),
    ReturnStmt      (IdNode<Expr>),
    YieldStmt       (IdNode<Expr>),
    BreakStmt,
    PassStmt,
    ContinueStmt,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    MatchExpr       {value: Box<IdNode<Expr>>, cases: Vec<(IdNode<Expr>, IdNode<Expr>)>},
    ComparisonExpr  {operator: ComparisonOperator, left: Box<IdNode<Expr>>, right: Box<IdNode<Expr>>},//TODO take this out
    BinaryExpr      {operator: BinaryOperator, left: Box<IdNode<Expr>>, right: Box<IdNode<Expr>>},
    UnaryExpr       {operator: UnaryOperator, operand: Box<IdNode<Expr>>},
    FunctionCall    {function: Box<IdNode<Expr>>, args: Vec<IdNode<Expr>>, kwargs: Vec<(Identifier, IdNode<Expr>)>},
    AttributeAccess {base: Box<IdNode<Expr>>, attributes: Vec<Identifier>},
    Index           {slices: Vec<(Option<IdNode<Expr>>, Option<IdNode<Expr>>, Option<IdNode<Expr>>)>},
    VecComprehension{values: Box<IdNode<Expr>>, iterators: Vec<ComprehensionIter2>},
    GenComprehension{values: Box<IdNode<Expr>>, iterators: Vec<ComprehensionIter2>},
    MapComprehension{keys: Box<IdNode<Expr>>, values: Box<IdNode<Expr>>, iterators: Vec<ComprehensionIter2>},
    SetComprehension{values: Box<IdNode<Expr>>, iterators: Vec<ComprehensionIter2>},
    IdentifierExpr  (Identifier),
    Bool            (bool),
    Int             (String),
    Float           (String),
    String          (String),
    VecLiteral      (Vec<IdNode<Expr>>),
    SetLiteral      (Vec<IdNode<Expr>>),
    TupleLiteral    (Vec<IdNode<Expr>>),
    MapLiteral      (Vec<(Identifier, IdNode<Expr>)>)
}

/// Types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub enum Type {
    i32,
    i64,
    f32,
    f64,
    ui32,
    ui64,
    string,
    boolean,
    array
}

impl Type {

    /// Get the name of this type in WAST.
    pub fn wast_name(&self) -> String {
        match self {
            &Type::i32 => "i32".to_string(),
            &Type::i64 => "i64".to_string(),
            &Type::f32 => "f32".to_string(),
            &Type::f64 => "f64".to_string(),
            &Type::ui32 => "i32".to_string(),
            &Type::ui64 => "i64".to_string(),
            _ => panic!()
        }
    }

    /// Get _s or _u for signed values, otherwise an empty string.
    pub fn sign(&self) -> String {
        match &self {
            &Type::i32 | &Type::i64 => "_s".to_string(),
            &Type::ui32 | &Type::ui64 => "_u".to_string(),
            _ => "".to_string()
        }
    }
}

/// Impl for IdNodes
impl <T> IdNode<T> {
    pub fn replace(&self, new_data: T) -> IdNode<T> {
        return IdNode {
            id: self.id,
            data: new_data,
            scope: self.scope.clone()
        };
    }
}

impl IdNode<Expr> {
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeAnnotation {
    Simple(Identifier)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComprehensionIter2 {
    pub iter_vars: Vec<Identifier>,
    pub iterator: Box<IdNode<Expr>>,
    pub if_clauses: Vec<IdNode<Expr>>
}

/// A helper Enum for trailers.
#[derive (Debug, Clone, PartialEq, Eq, Hash)]
pub enum PostIdent2 {
    Call{args: Vec<IdNode<Expr>>, kwargs: Vec<(Identifier, IdNode<Expr>)>},
    Index{slices: Vec<(Option<IdNode<Expr>>, Option<IdNode<Expr>>, Option<IdNode<Expr>>)>},
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

impl BinaryOperator {

    pub fn get_return_type(&self, left: &Type, right: &Type) -> Type {

        let add_order = hashmap!{
            Type::i32 => vec!{Type::i32, Type::i64, Type::f64},
            Type::ui32 => vec!{Type::ui32, Type::i64, Type::ui64, Type::f64},
            Type::f32 => vec!{Type::f32, Type::f64},
            Type::i64 => vec!{Type::i64},
            Type::f64 => vec!{Type::f64}
        };

        let div_order = hashmap!{
            Type::i32 => vec!{Type::f64},
            Type::f32 => vec!{Type::f32, Type::f64},
            Type::f64 => vec!{Type::f64}
        };

        let order = match self {
            BinaryOperator::Add | BinaryOperator::Sub |
            BinaryOperator::Mult | BinaryOperator::Mod => add_order,
            BinaryOperator::Div => div_order,
            _ => panic!()
        };

        let left_upper = order.get(left).unwrap();
        let right_upper = order.get(right).unwrap();
        let mut intersection = c![*x, for x in left_upper, if right_upper.contains(x)];
        // TODO: Check that 0 exists, throw a TypeError if it doesn't.
        intersection.remove(0)
    }

    pub fn requires_sign(&self) -> bool {
        match self {
            BinaryOperator::Div => true,
            _ => false
        }
    }
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

impl <'a> From<&'a Type> for UnaryOperator {
    fn from(input: &'a Type) -> Self {
        match input {
            Type::i32 => UnaryOperator::ToI32,
            Type::ui32 => UnaryOperator::ToF32,
            Type::i64 => UnaryOperator::ToI64,
            Type::ui64 => UnaryOperator::ToUi64,
            Type::f32 => UnaryOperator::ToF32,
            Type::f64 => UnaryOperator::ToF64,
            _ => panic!()
        }
    }
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

/// From for IdNode
impl <T> From<T> for IdNode<T> {
    fn from(input: T) -> Self {
        return IdNode{
            id: get_next_id(),
            data: input,
            scope: empty_scope()
        };
    }
}

impl From<bool> for IdNode<Expr> {
    fn from(input: bool) -> Self {
        let expr = Expr::from(input);
        return IdNode {
            id: get_next_id(),
            data: expr,
            scope: empty_scope()
        };
    }
}

impl <'a> From<&'a str> for IdNode<Expr> {
    fn from(input: &'a str) -> Self {
        let expr: Expr = Expr::from(input);
        return IdNode {
            id: get_next_id(),
            data: expr,
            scope: empty_scope()
        };
    }
}

impl From<i64> for IdNode<Expr> {
    fn from(input: i64) -> Self {
        let expr: Expr = Expr::from(input);
        return IdNode {
            id: get_next_id(),
            data: expr,
            scope: empty_scope()
        };
    }
}

impl From<f64> for IdNode<Expr> {
    fn from(input: f64) -> Self {
        let expr: Expr = Expr::from(input);
        return IdNode {
            id: get_next_id(),
            data: expr,
            scope: empty_scope()
        };
    }
}

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
