use std::fmt;
use std::fmt::Display;
use std::str::from_utf8;

fn indent_block(block_str: String) -> String {
    let split = block_str.lines();
    let mut ret: String = "  ".to_string();
    ret.push_str(&split.collect::<Vec<&str>>().join("\n  "));
    return ret;
}

/// A top level module.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub declarations: Vec<Stmt>
}

/// A block of code. Just a series of statements.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

/// A statement.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    AssignmentStmt{identifier: Identifier, operator: Assignment, expression: Expr},
    LetStmt{value_name: TypedIdent, value: Expr},
    IfStmt{condition: Expr, main_block: Block, elifs: Vec<(Expr, Block)>, else_block: Option<Block>},
    WhileStmt{condition: Expr, block: Block},
    ForInStmt{iter_var: Identifier, iterator: Expr, block: Block},
    FunctionDecStmt{name: Identifier, args: Vec<TypedIdent>, vararg: Option<Identifier>, keyword_args: Option<Vec<(TypedIdent, Expr)>>, varkwarg: Option<Identifier>, body: Block, return_type: Option<TypeAnnotation>},
    // TODO: Change to be values instead of records.
    ImportStmt{module: DottedIdentifier},
    ReturnStmt{value: Expr},
    BreakStmt,
    PassStmt,
    ContinueStmt,
    YieldStmt(Expr),
    TryExceptStmt{main: Block, exception: Vec<Block>, else_block: Option<Block>, finally: Option<Block>}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedIdent {
    pub name: Identifier,
    pub type_annotation: Option<TypeAnnotation>
}

/// An expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    MatchExpr{value: Box<Expr>, cases: Vec<(Expr, Expr)>},
    ComparisonExpr{operator: ComparisonOperator, left: Box<Expr>, right: Box<Expr>},
    BinaryExpr{operator: BinaryOperator, left: Box<Expr>, right: Box<Expr>},
    UnaryExpr{operator: UnaryOperator, operand: Box<Expr>},
    FunctionCall{func_expr: Box<Expr>, args: Vec<Expr>, kwargs: Vec<(Identifier, Expr)>},
    AttributeAccess{container: Box<Expr>, attributes: Vec<Identifier>},
    Index{slices: Vec<(Option<Expr>, Option<Expr>, Option<Expr>)>},
    IdentifierExpr{ident: Identifier},
    Bool(Boolean),
    Int(IntegerLiteral),
    Float(FloatLiteral),
    String(String),
    VecLiteral(Vec<Expr>),
    SetLiteral(Vec<Expr>),
    TupleLiteral(Vec<Expr>),
    MapLiteral(Vec<(Identifier, Expr)>),
    VecComprehension{values: Box<Expr>, iterators: Vec<ComprehensionIter>},
    GenComprehension{values: Box<Expr>, iterators: Vec<ComprehensionIter>},
    MapComprehension{keys: Box<Expr>, values: Box<Expr>, iterators: Vec<ComprehensionIter>},
    SetComprehension{values: Box<Expr>, iterators: Vec<ComprehensionIter>}
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

impl Expr {
    pub fn get_type(&self) -> Type {
        match self {
            &Expr::String (_) => Type::string,
            &Expr::Bool (_) => Type::boolean,
            &Expr::Int (_) => Type::i32,
            &Expr::Float (_) => Type::f64,
            &Expr::UnaryExpr {ref operator, ref operand} => {
                match operator {
                    &UnaryOperator::ToF32 => Type::f32,
                    &UnaryOperator::ToF64 => Type::f64,
                    &UnaryOperator::ToI32 => Type::i32,
                    &UnaryOperator::ToI64 => Type::i64,
                    _ => operand.get_type()
                }
            }
            _ => panic!()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeAnnotation {
    Simple(Identifier)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComprehensionIter {
    pub iter_vars: Vec<Identifier>,
    pub iterator: Box<Expr>,
    pub if_clauses: Vec<Expr>
}

/// A helper Enum for trailers.
#[derive (Debug, Clone, PartialEq, Eq, Hash)]
pub enum PostIdent {
    Call{args: Vec<Expr>, kwargs: Vec<(Identifier, Expr)>},
    Index{slices: Vec<(Option<Expr>, Option<Expr>, Option<Expr>)>},
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub name: String,
}

/// A boolean literal.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Boolean {
    True,
    False
}

/// An integer literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntegerLiteral {
    pub string_rep: String
}

/// A floating point literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FloatLiteral {
    pub string_rep: String
}


/// Display implementations
impl Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Block")
    }
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
            &Stmt::FunctionDecStmt{ref name, ref args, ref vararg, ref keyword_args, ref varkwarg, ref body, ref return_type} => {
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
impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string_rep = match self {
            &Expr::ComparisonExpr{ref operator, ref left, ref right} => format!("Comparison:\n Left: {} Op:{} Right: {}", left, operator, right),
            &Expr::BinaryExpr{ref operator, ref left, ref right} => format!("Binary:\n Left: {} Op:{} Right: {}", left, operator, right),
            &Expr::UnaryExpr{ref operator, ref operand} => format!("Unary expression. Operator: {}. Operand: {}", operator, operand),
            &Expr::FunctionCall{ref func_expr, ref args, ref kwargs} => {
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
impl Display for ComparisonOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
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
impl Display for UnaryOperator{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &UnaryOperator::Not => "not",
            &UnaryOperator::Positive => "+",
            &UnaryOperator::Negative => "-",
            &UnaryOperator::BitNot => "~",
            _ => panic!()
        })
    }
}
impl Display for TypedIdent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} (typed)", self.name)
    }
}
impl Display for DottedIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Dotted Ident: {:?}", self.attributes)
    }
}
impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
impl Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &Boolean::True => "true",
            &Boolean::False => "false"
        })
    }
}
impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.string_rep)
    }
}
impl Display for FloatLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.string_rep)
    }
}

/// From implementations

/// From for Expr
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

/// From for Boolean
impl From<bool> for Boolean {
    fn from(input: bool) -> Self {
        return match input {
            true => Boolean::True,
            false => Boolean::False
        };
    }
}

impl <'a> From<&'a [u8]> for Boolean {
    fn from(input: &'a [u8]) -> Self {
        return match from_utf8(input) {
            Ok("true") => Boolean::True,
            Ok("false") => Boolean::False,
            _ => panic!()
        }
    }
}

/// From for IntegerLiteral
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

/// From for FloatLiteral
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

/// From for TypeAnnotation
impl <'a> From<&'a str> for TypeAnnotation {
    fn from(input: &'a str) -> Self {
        return TypeAnnotation::Simple(Identifier::from(input));
    }
}

#[test]
fn test_indent() {
    let block = "Block:\n  Assignment: test2 = true\n  Assignment: bar = false and true".to_string();
    assert_eq!(indent_block(block), "  Block:\n    Assignment: test2 = true\n    Assignment: bar = false and true".to_string())
}
