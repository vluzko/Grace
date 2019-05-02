use std::collections::HashSet;
use std::iter::FromIterator;

use expression::*;
use scoping::*;
use general_utils::*;

/// Types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub enum Type {
    i32,
    i64,
    f32,
    f64,
    ui32,
    string,
    boolean,
    Sum(Vec<Type>),
    Product(Vec<Type>),
    Vector(Box<Type>),
    Function(Vec<Type>, Box<Type>)
}

// The signed integral types.
#[allow(non_snake_case)]
pub fn Signed<'a>() -> Type {
    Type::Sum(vec!(Type::i32, Type::i64))
}

// The unsigned integral types.
#[allow(non_snake_case)]
pub fn Unsigned<'a>() -> Type {
    Type::Sum(vec!(Type::ui32))
}

// The floating point types.
#[allow(non_snake_case)]
pub fn Integral<'a>() -> Type {
    return Signed().merge(&Unsigned());
}

// The floating point types.
#[allow(non_snake_case)]
pub fn FloatingPoint<'a>() -> Type {
    Type::Sum(vec!(Type::f32, Type::f64))
}

pub fn Numeric<'a>() -> Type {
    return FloatingPoint().merge(&Integral());
}

impl Type {

    /// Get the name of this type in WAST.
    pub fn wast_name(&self) -> String {
        match self {
            &Type::i32 => "i32".to_string(),
            &Type::i64 => "i64".to_string(),
            &Type::f32 => "f32".to_string(),
            &Type::f64 => "f64".to_string(),
            &Type::ui32 => "i64".to_string(),
            _ => panic!()
        }
    }

    /// Get _s or _u for signed values, otherwise an empty string.
    pub fn sign(&self) -> String {
        match &self {
            &Type::i32 | &Type::i64 => "_s".to_string(),
            &Type::ui32 => "_u".to_string(),
            _ => "".to_string()
        }
    }

    /// Merge two types if they're compatible.
    pub fn merge(&self, other: &Type) -> Type {
        if self == other {
            return self.clone()
        } else {
            return match self {
                Type::Sum(ref types) => {
                    match other {
                        Type::Sum(ref other_types) => {
                            return Type::Sum(vec_c_int(types, other_types))
                        }, 
                        _ => panic!()
                    }
                }, 
                _ => panic!()
            }
        }
    }
}

pub trait Typed<T> {
    fn type_based_rewrite(self) -> T;

    fn resolve_types(&self) -> Type;
}

impl Typed<CanModifyScope> for CanModifyScope {
    fn type_based_rewrite(self) -> CanModifyScope {
        panic!()
    }

    fn resolve_types(&self) -> Type {
        return match self {
            CanModifyScope::Statement(ref ptr) => {
                unsafe {
                    (**ptr).resolve_types() 
                }
            },
            CanModifyScope::Expression(ref ptr) => {
                unsafe {
                    (**ptr).resolve_types()
                }
            },
            CanModifyScope::Argument => {
                panic!()
            }
        };
    }
}

impl Typed<Node<Module>> for Node<Module> {
    fn type_based_rewrite(self) -> Node<Module> {
        let new_decs = c![x.type_based_rewrite(), for x in self.data.declarations];
        return Node{
            id: self.id,
            data: Module{ declarations: new_decs},
            scope: self.scope
        };
    }

    fn resolve_types(&self) -> Type {
        panic!()
    }
}

impl Typed<Node<Block>> for Node<Block> {
    fn type_based_rewrite(self) -> Node<Block> {
        let new_stmts = c![x.type_based_rewrite(), for x in self.data.statements];
        return Node {
            id: self.id,
            data: Block{statements: new_stmts},
            scope: self.scope
        };
    }

    fn resolve_types(&self) -> Type {
        for stmt in self.data.statements.iter() {
            stmt.resolve_types();
        }
        return Type::i32;
    }
}

impl Typed<Node<Stmt>> for Node<Stmt> {
    fn type_based_rewrite(self) -> Node<Stmt> {
        let new_stmt = match self.data {
            Stmt::FunctionDecStmt {name, block, args, vararg, kwargs, varkwarg, return_type} => {
                Stmt::FunctionDecStmt {block: block.type_based_rewrite(), name, args, vararg, kwargs, varkwarg, return_type}
            },
            Stmt::AssignmentStmt {mut expression, name, operator} => {
                expression = expression.type_based_rewrite();
                Stmt::AssignmentStmt {name, operator, expression: expression.type_based_rewrite()}
            },
            Stmt::IfStmt {condition, block, elifs,  else_block} => {
                let new_elifs =  c![(elif.0.type_based_rewrite(), elif.1.type_based_rewrite()), for elif in elifs];
                let new_else_block = match else_block {
                    None => None,
                    Some(block) => Some(block.type_based_rewrite())
                };
                Stmt::IfStmt {condition: condition.type_based_rewrite(), block: block.type_based_rewrite(), elifs: new_elifs, else_block: new_else_block}
            },
            Stmt::LetStmt {typed_name, expression} => {
                Stmt::LetStmt {typed_name, expression: expression.type_based_rewrite()}
            },
            Stmt::WhileStmt {condition, block} => {
                Stmt::WhileStmt {condition: condition.type_based_rewrite(), block: block.type_based_rewrite()}
            },
            Stmt::ReturnStmt (value) => {
                Stmt::ReturnStmt (value.type_based_rewrite())
            },
            _ => self.data
        };
        return Node {
            id: self.id, 
            data: new_stmt,
            scope: self.scope 
        };
    }

    fn resolve_types(&self) -> Type {
        return match self.data {
            Stmt::LetStmt{ref typed_name, ref expression} => {
                expression.resolve_types()
            },
            Stmt::AssignmentStmt{ref expression, ref name, ref operator} => {
                let expr_types = expression.resolve_types();
                panic!()
            }
            Stmt::ReturnStmt(ref value) => value.resolve_types(),
            _ => panic!()
        };
    }
}

impl Typed<Node<Expr>> for Node<Expr> {
    fn type_based_rewrite(self) -> Node<Expr> {
        let new_expr = match self.data {
            Expr::ComparisonExpr {mut left, mut right, operator} => {
                let left = Box::new(left.type_based_rewrite());
                let right = Box::new(right.type_based_rewrite());
                Expr::ComparisonExpr {left, right, operator}
            },
            Expr::BinaryExpr {operator, left, right} => {
                let left_type = &left.get_type();
                let right_type = &right.get_type();
                let return_type = &operator.get_return_type(left_type, right_type);
                let new_left = if left_type != return_type {
                    let conversion_op = UnaryOperator::from(return_type);
                    let left_expr = Expr::UnaryExpr { operator: conversion_op, operand: left.clone()};
                    let node = left.replace(left_expr);
                    Box::new(node)
                } else {
                    left
                };
                let new_right = if right_type != return_type {
                    let conversion_op = UnaryOperator::from(return_type);
                    let right_expr = Expr::UnaryExpr { operator: conversion_op, operand: right.clone()};
                    let node = right.replace(right_expr);
                    Box::new(node)
                } else {
                    right
                };
                Expr::BinaryExpr {operator: operator, left: new_left, right: new_right}
            },
            Expr::FunctionCall {function, args, kwargs} => {
                let new_func_expr = Box::new(function.type_based_rewrite());
                let new_args = args.into_iter().map(|x| x.type_based_rewrite()).collect();
                let new_kwargs = kwargs.into_iter().map(|x| (x.0, x.1.type_based_rewrite())).collect();
                Expr::FunctionCall {function: new_func_expr, args: new_args, kwargs: new_kwargs}
            },
            _ => self.data
        };

        return Node {
            id: self.id,
            data: new_expr,
            scope: self.scope
        };
    }

    fn resolve_types(&self) -> Type {
        return match &self.data {
            Expr::BinaryExpr{ref operator, ref left, ref right} => {
                operator.get_possible_return_types(left, right)
            },
            Expr::ComparisonExpr{ref left, ref right, ..} => {
                Type::boolean
            },
            Expr::UnaryExpr{ref operator, ref operand} => {
                panic!()
            }
            Expr::IdentifierExpr(ref name) => {
                // TODO: Look up the name in scope.
                let creation = self.scope.get_declaration(name);
                println!("{:?}", self.scope);
                match creation {
                    Some(y) => y.resolve_types(),
                    None => panic!()
                }
            }
            Expr::String(_) => Type::string,
            Expr::Float(_) => FloatingPoint(),
            Expr::Bool(_) => Type::i32,
            Expr::Int(_) => Numeric(),
            _ => panic!()
        }
    }
}

pub fn choose_return_type(possible: &Type) -> Type {
    let preference = vec!(Type::i32, Type::i64, Type::f32, Type::f64);
    return match possible {
        Type::Sum(ref types) => {
            for t in preference {
                if types.contains(&t) {
                    return t;
                }
            }
            possible.clone()
        },
        _ => possible.clone()
    }
}

impl BinaryOperator {

    pub fn get_possible_return_types(&self, left: &Box<Node<Expr>>, right: &Box<Node<Expr>>) -> Type {
        //let mut intersection = HashSet::new();
        return match self {
            BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mult => {
                // c_int(&left.resolve_types(), &right.resolve_types())
                left.resolve_types().merge(&right.resolve_types())
            },
            BinaryOperator::Div => {
                FloatingPoint()
            },
            BinaryOperator::And | BinaryOperator::Or | BinaryOperator::Xor => Type::boolean,
            _ => panic!()
        }
    }

    // TODO: Rename to CHOOSE return type
    pub fn get_return_type(&self, left: &Type, right: &Type) -> Type {

        let add_order = hashmap!{
            Type::i32 => vec!{Type::i32, Type::i64, Type::f64},
            Type::ui32 => vec!{Type::ui32, Type::i64, Type::f64},
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

#[cfg(test)]
mod test {

    use super::*;
    use parser;
    use parser_utils;
    use scoping;


    #[test]
    fn test_identifier_resolution() {
        let block = "let a = 1\nlet b = a";
        let parsed = parser_utils::output(parser::block(block.as_bytes(), 0));
        let scoped = parsed.gen_scopes(&scoping::empty_scope());
        let types = scoped.resolve_types();
        println!("{:?}", types);
    }
}
