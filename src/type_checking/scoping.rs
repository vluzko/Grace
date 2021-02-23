use std::collections::{BTreeSet, BTreeMap, HashSet, HashMap};
use std::iter::FromIterator;

use expression::*;
use general_utils;
use type_checking::refinements::check_constraints;
use type_checking::types::{Type, Refinement, Trait};
use type_checking::context::Context;


/// A sum type for things that can modify scope.
/// Currently the only things that can do so are:
/// * import statements
/// * let statements
/// * function declarations
/// * comprehensions
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CanModifyScope {
    Statement(*const Node<Stmt>, usize),
    Argument(Type),
    Return(Type),
    ImportedModule(usize)
}

impl CanModifyScope {

    pub fn extract_stmt(&self) -> Stmt {
        return unsafe {
            match self {
                CanModifyScope::Statement(stmt_ptr, _) => (**stmt_ptr).data.clone(),
                _ => panic!()
            }
        };
    }

    pub fn get_id(&self) -> usize {
        return match self {
            CanModifyScope::Statement(ref _ptr, ref id) => *id,
            CanModifyScope::ImportedModule(ref id) => *id,
            CanModifyScope::Argument(..) | CanModifyScope::Return(..) => panic!()
        };
    }
}

/// A single layer of scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scope {
    /// The id of the parent scope.
    pub parent_id: Option<usize>,
    // TODO: Consider replacing with an insertion ordered map.
    /// The identifiers declared in this scope, and raw pointers to the statements that created them.
    pub declarations: BTreeMap<Identifier, CanModifyScope>,
    /// The order in which each identifier was declared. (Important for blocks.)
    pub declaration_order: BTreeMap<Identifier, usize>,
    // The trait we're in, if we're in one.
    pub maybe_trait: Option<Identifier>,
    // The struct we're in, if we're in one.
    pub maybe_struct: Option<Identifier>,
}

impl Scope {
    /// Create an empty scope.
    pub fn empty() -> Scope {
        return Scope{
            parent_id: None,
            declarations: BTreeMap::new(),
            declaration_order: BTreeMap::new(),
            maybe_trait: None,
            maybe_struct: None
        };
    }

    /// Create a child of the given parent
    pub fn child(parent_id: usize) -> Scope {
        return Scope{
            parent_id: Some(parent_id),
            declarations: BTreeMap::new(),
            declaration_order: BTreeMap::new(),
            maybe_trait: None,
            maybe_struct: None
        };
    }

    /// Create a child of the given parent with a struct
    pub fn child_struct_impl(parent_id: usize, structname: &Identifier) -> Scope {
        return Scope{
            parent_id: Some(parent_id),
            declarations: BTreeMap::new(),
            declaration_order: BTreeMap::new(),
            maybe_trait: None,
            maybe_struct: Some(structname.clone())
        };
    }

    /// Create a child of the given parent with a trait and a struct
    pub fn child_trait_impl(parent_id: usize, structname: &Identifier, traitname: &Identifier) -> Scope {
        return Scope{
            parent_id: Some(parent_id),
            declarations: BTreeMap::new(),
            declaration_order: BTreeMap::new(),
            maybe_trait: Some(traitname.clone()),
            maybe_struct: Some(structname.clone())
        };
    }

    pub fn append_declaration(&mut self, name: &Identifier, stmt: &Box<Node<Stmt>>) {
        self.declaration_order.insert(name.clone(), self.declaration_order.len() + 1);
        let scope_mod = CanModifyScope::Statement(stmt.as_ref() as *const _, stmt.id);
        self.declarations.insert(name.clone(), scope_mod);
    }

    pub fn append_modification(&mut self, name: &Identifier, modification: CanModifyScope) {
        self.declaration_order.insert(name.clone(), self.declaration_order.len() + 1);
        self.declarations.insert(name.clone(), modification);
    }
}

pub fn get_convert_expr(from: &Type, to: &Type, base: Node<Expr>, context: &mut Context) -> Node<Expr> {
    return match from == to {
        false => {
            let operator = UnaryOperator::cast(&from, &to);
            let raw_expr = Expr::UnaryExpr{operator: operator, operand: Box::new(base)};
            let new_node = Node::from(raw_expr);
            context.add_type(new_node.id, to.clone());
            new_node
        },
        true => base
    };
}

pub fn choose_return_type(possible: &Type) -> Type {
    return match possible {
        Type::Sum(ref types) => {
            let mut i = 0;
            let mut min_size = usize::MAX;
            for (j, t) in types.iter().enumerate() {
                if t.size() < min_size {
                    i = j;
                    min_size = t.size();
                }
            }
            types[i].clone()
        },
        _ => possible.clone()
    }
}

impl BinaryOperator {

    pub fn get_return_types(&self, left: &Type, right: &Type) -> Type {
        //let mut intersection = HashSet::new();
        return match self {
            BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mult | BinaryOperator::Mod => left.merge(right),
            BinaryOperator::Div => Type::f64,
            BinaryOperator::And | BinaryOperator::Or | BinaryOperator::Xor => Type::boolean,
            _ => panic!()
        }
    }

    pub fn choose_return_type(&self, merged_type: &Type) -> Type {

        return match self {
            BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mult | BinaryOperator::Mod => 
            choose_return_type(merged_type),
            BinaryOperator::Div => Type::f32,
            _ => panic!()   
        };
    }

    pub fn requires_sign(&self) -> bool {
        match self {
            BinaryOperator::Div => true,
            _ => false
        }
    }

    /// Return the index of the corresponding gradual function in the gradual function table.
    pub fn gradual_index(&self) -> i32 {
        return match self {
            BinaryOperator::Add => 0,
            BinaryOperator::Sub => 1,
            BinaryOperator::Mult => 2,
            _ => panic!()
        };
    }

    pub fn get_builtin_trait(&self) -> (Identifier, Identifier) {
        let (x, y) = match self {
            BinaryOperator::Add => ("Add", "add"),
            BinaryOperator::Sub => ("Sub", "sub"),
            BinaryOperator::Mult => ("Mult", "mult"),
            BinaryOperator::Div => ("Div", "div"),
            BinaryOperator::And => ("And", "and"),
            BinaryOperator::Or => ("Or", "or"),
            _ => panic!()
        };

        return (Identifier::from(x), Identifier::from(y));
    }
}

impl UnaryOperator {
    fn cast(from: &Type, to: &Type) -> UnaryOperator {
        let possible = from.is_compatible(to);

        if possible {
            return UnaryOperator::Convert(from.clone(), to.clone());
        } else {
            panic!("Cannot cast from {:?} to {:?}", from, to);
        }
    }
}

impl <'a> From<&'a Identifier> for Type {
    fn from(input: &'a Identifier) -> Self {
        return match input.name.as_ref() {
            "i32" => Type::i32,
            "i64" => Type::i64,
            "f32" => Type::f32,
            "f64" => Type::f64,
            "ui32" => Type::ui32,
            "ui64" => Type::ui64,
            "boolean" => Type::boolean,
            "string" => Type::string,
            "any" => Type::Gradual(general_utils::get_next_grad()),
            _ => Type::Named(input.clone())
        };
    }
}

impl From<Identifier> for Type {
    fn from(input: Identifier) -> Self {
        return Type::from(&input);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use compiler_layers;
    use difference::{Difference, Changeset};
    use regex::Regex;
    use std::fs::File;
    use std::io::Read;

    #[cfg(test)]
    mod scope_generation {
        use super::*;
        use parser::Parseable;
        use position_tracker::PosStr;

        fn check_ptr_stmt(ptr: &CanModifyScope, expected: &Stmt) -> bool {
            return match ptr {
                CanModifyScope::Statement(x, _) => {
                    unsafe {
                        let actual_stmt = &(**x).data;
                        assert_eq!(actual_stmt, expected);
                        true
                    }
                },
                x => panic!("Expected a statement modification, found: {:?}", x)
            }
        }

        // impl Node<Stmt> {
        //     fn mod_scope(&mut self, new_scope: usize) {
        //         self.scope = new_scope;
        //         match self.data {
        //             Stmt::LetStmt{ref mut expression, ..} => {
        //                 expression.mod_scope(new_scope);
        //             }
        //             _ => {}
        //         }
        //     }
        // }

        /// Utility function to recursively modify the scope of an Expr AST.
        impl Node<Expr> {
            fn mod_scope(&mut self, new_scope: usize) {
                self.scope = new_scope;
                match self.data {
                    Expr::BinaryExpr{ref mut left, ref mut right, ..} => {
                        left.mod_scope(new_scope);
                        right.mod_scope(new_scope);
                    },
                    Expr::FunctionCall{ref mut function, ref mut args, ..} => {
                        function.mod_scope(new_scope);
                        for arg in args {
                            arg.mod_scope(new_scope);
                        }
                    },
                    _ => {}
                }
            }
        }

        #[test]
        fn test_block_scope() {
            let mut e1 = Node::<Expr>::parse(PosStr::from("5 + -1"));
            e1.mod_scope(1);
            let s1 = Stmt::LetStmt{name: Identifier::from("a"), type_annotation: None, expression: Node::from(e1)};

            let mut e2 = Node::<Expr>::parse(PosStr::from("true and false"));
            e2.mod_scope(1);
            let s2 = Stmt::LetStmt{name: Identifier::from("b"), type_annotation: None, expression: Node::from(e2)};

            let input = r#"
            let a = 5 + -1
            let b = true and false"#;

            let (block, context) = compiler_layers::to_context::<Node<Block>>(input.as_bytes());
            let scope = context.get_scope(block.scope);
            assert_eq!(scope.declarations.len(), 2);

            let stmt1_pointer = context.get_declaration(block.scope, &Identifier::from("a")).unwrap();
            check_ptr_stmt(stmt1_pointer, &s1);

            let stmt2_pointer = context.get_declaration(block.scope, &Identifier::from("b")).unwrap();
            check_ptr_stmt(stmt2_pointer, &s2);
        }


        #[cfg(test)]
        mod stmts {
            use super::*;

            #[test]
            fn test_let_stmt() {
                let (block, context) = compiler_layers::to_context::<Node<Block>>("let a = 1".as_bytes());
                assert_eq!(context.scopes.len(), 2);
                let scope = context.get_scope(block.scope);
                assert_eq!(scope.declarations.len(), 1);
                assert!(scope.declarations.contains_key(&Identifier::from("a")));
            }

            #[test]
            fn test_function_decl() {
                let block_str = r#"
                fn a() -> i32:
                    return 0

                fn b() -> i32:
                    return 1
                "#;
                let (block, context) = compiler_layers::to_context::<Node<Block>>(block_str.as_bytes());
                let fn1 = context.get_declaration(block.scope, &Identifier::from("a")).unwrap();
                let fn2 = context.get_declaration(block.scope, &Identifier::from("b")).unwrap();
                match fn1.extract_stmt() {
                    Stmt::FunctionDecStmt{name, ..} => {
                        assert_eq!(name, Identifier::from("a"))
                    },
                    _ => panic!()
                };

                match fn2.extract_stmt() {
                    Stmt::FunctionDecStmt{name, .. } => {
                        assert_eq!(name, Identifier::from("b"))
                    },
                    _ => panic!()
                };
            }
        }

        
    }
}
