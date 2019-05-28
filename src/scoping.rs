use std::collections::BTreeMap;
use std::collections::HashSet;
use std::collections::HashMap;
use std::hash::Hash;
use expression::*;
use general_utils;

extern crate cute;

/// The full scoping and typing context for a compilation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Context {
    // A map from Scope ids to Scopes.
    scopes: HashMap<usize, Scope>,
    // A map from Node ids to Scope ids. Each node that modifies scope
    // maps to the scope it's contained in.
    containing_scopes: HashMap<usize, usize>
}

/// A sum type for things that can modify scope.
/// Currently the only things that can do so are:
/// * import statements
/// * let statements
/// * function declarations
/// * comprehensions
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CanModifyScope {
    Statement(*const Node<Stmt>),
    Expression(*const Node<Expr>),
    Argument
}

/// A single layer of scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scope {
    pub parent_id: Option<usize>,
    // pub parent_scope: Option<*const Scope>,
    // TODO: Consider replacing with an insertion ordered map.
    pub declarations: BTreeMap<Identifier, CanModifyScope>,
    pub declaration_order: BTreeMap<Identifier, usize>
}

/// Any object that has a scope.
pub trait Scoped<T> {
    /// Get the scope of the object.

    fn get_usages(&self) -> HashSet<Identifier>;

    /// Get all *non-Argument* declarations.
    fn get_true_declarations(&self, context: &Context) -> HashSet<Identifier>;

    /// Check that all of the objects references are valid.
    fn check_scope(self, scope: Scope) -> bool;

    /// Generate scopes recursively. Returns a new node with the correct scope attached.
    fn gen_scopes(self, parent_id: Option<usize>, context: &Context) -> T;
    
    fn gen_scopes2(&mut self, parent_id: usize, context: &Context) -> Context;
}

/// Create an empty scope.
pub fn empty_scope() -> Scope {
    return Scope{
        parent_id: Some(0),
        declarations: BTreeMap::new(),
        declaration_order: BTreeMap::new()
    };
}

/// Create a context containing only the empty scope.
pub fn initial_context() -> Context {
    let empty = empty_scope();
    let mut init_scopes = HashMap::new();
    init_scopes.insert(0, empty);
    let context = Context{scopes: init_scopes, containing_scopes: HashMap::new()};
    return context;
}

pub fn empty_context() -> Context {
    let scopes = HashMap::new();
    return Context{scopes, containing_scopes: HashMap::new()};
}

impl Context {
    pub fn get_scope(&self, scope_id: usize) -> &Scope {
        return self.scopes.get(&scope_id).unwrap();
    }

    pub fn add_scope(&mut self, scope_id: usize, scope: Scope) {
        self.scopes.insert(scope_id, scope);
    }

    pub fn new_scope(&mut self, scope: Scope) -> usize {
        let scope_id = general_utils::get_next_scope_id();
        self.scopes.insert(scope_id, scope);
        return scope_id;
    }

    pub fn get_declaration(&self, scope_id: usize, name: &Identifier) -> Option<&CanModifyScope> {
        let initial_scope = self.scopes.get(&scope_id).unwrap();
        if initial_scope.declarations.contains_key(name) {
            return initial_scope.declarations.get(name);
        } else {
            return match initial_scope.parent_id {
                Some(id) => {
                    self.get_declaration(id, name)
                },
                None => None
            };
        }
    }

    /// Extend this context with another one.
    pub fn extend(&mut self, other_context: Context) {
        for (id, scope) in other_context.scopes.into_iter() {
            if (self.scopes.contains_key(&id)) {
                // TODO: Make this a real error.
                panic!("Duplicate scope IDs.\n ID: {}.\n Existing: {:?}.\n Replacement: {:?}", id, self.scopes.get(&id), scope);
            } else {
                self.scopes.insert(id, scope);
            }
        }
    }
}

impl CanModifyScope {
    pub fn extract_stmt(&self) -> Stmt {
        return unsafe {
            match self {
                CanModifyScope::Statement(stmt_ptr) => (**stmt_ptr).data.clone(),
                _ => panic!()
            }
        };
    }
}

impl Scope {

    pub fn get_parent(&self, context: &Context) -> Option<&Scope> {
        panic!()
        // return match self.parent_id {
        //     Some(id) => context.scopes.get(&id),
        //     None => None
        // };
    }
}

impl Scoped<Node<Module>> for Node<Module> {

    fn get_usages(&self) -> HashSet<Identifier> {
        panic!();
    }

    fn get_true_declarations(&self, context: &Context) -> HashSet<Identifier> {
        panic!()
    }

    fn check_scope(self, _scope: Scope) -> bool {
        panic!();
    }

    /// Check scope and generate
    fn gen_scopes(self, parent_id: Option<usize>, context: &Context) -> Node<Module> {
        panic!()
        // let declarations = BTreeMap::new();
        // let declaration_order = BTreeMap::new();

        // let mut new_scope = Scope{parent_id, declarations, declaration_order};
        // let scope_id = general_utils::get_next_scope_id();
        // let new_stmts: Vec<Node<Stmt>> = self.data.declarations.into_iter().enumerate().map(|(_i, stmt)| {
        //     let new_stmt = stmt.gen_scopes(Some(scope_id), context);
        //     return new_stmt;
        // }).collect();

        // // Update the parent scope with any changes caused by sub statements.
        // // Yes this does mean we loop through all the statements twice. If this
        // // proves to be a performance problem we can optimize it later.
        // // (The alternative is passing mutable data around recursively and that looks
        // // like several dozen migraines in the making.)
        // for (i, stmt) in new_stmts.iter().enumerate() {
        //     match &stmt.data {
        //         Stmt::FunctionDecStmt{ref name, ..} => {
        //             new_scope.declaration_order.insert(name as *const Identifier, i);
        //         },
        //         _ => {}
        //     };
        // }
        // context.add_scope(scope_id, new_scope);
        // let new_block = Module{declarations: new_stmts};
        // return Node{
        //     id: self.id,
        //     data: new_block,
        //     scope: scope_id
        // };
    }

    fn gen_scopes2(&mut self, parent_id: usize, context: &Context) -> Context {
        let declarations = BTreeMap::new();
        let declaration_order = BTreeMap::new();

        let mut new_scope = Scope{parent_id: Some(parent_id), declarations, declaration_order};
        let scope_id = general_utils::get_next_scope_id();
        self.scope = scope_id;

        let mut new_context = empty_context();

        for (i, stmt) in self.data.declarations.iter_mut().enumerate() {
            let child_context = stmt.gen_scopes2(scope_id, context);
            new_context.extend(child_context);
            match &stmt.data {
                Stmt::FunctionDecStmt{ref name, ..} => {
                    new_scope.declaration_order.insert(name.clone(), i);
                },
                _ => {}
            };
        }

        new_context.add_scope(scope_id, new_scope);
        return new_context;
    }
}

impl Scoped<Node<Block>> for Node<Block> {

    fn get_usages(&self) -> HashSet<Identifier> {
        let mut usages = HashSet::new();
        for stmt in &self.data.statements {
            usages = general_utils::m_union(usages, stmt.get_usages());
        }
        return usages;
    }

    fn get_true_declarations(&self, context: &Context) -> HashSet<Identifier> {
        let mut top_level: HashSet<Identifier> = context.get_scope(self.scope).declarations.keys().map(|x| x.clone()).collect();
        for stmt in &self.data.statements {
            top_level = general_utils::m_union(top_level, stmt.get_true_declarations(context));
        }
        return top_level;
    }


    fn check_scope(self, _scope: Scope) -> bool {
        panic!();
    }

    fn gen_scopes(self, parent_id: Option<usize>, context: &Context) -> Node<Block> {
        panic!()
        // let declarations = BTreeMap::new();
        // let declaration_order = BTreeMap::new();

        // let mut new_scope = Scope{parent_id, declarations, declaration_order};
        // let new_id = general_utils::get_next_scope_id();

        // let new_stmts: Vec<Node<Stmt>> = self.data.statements.into_iter().enumerate().map(|(_i, stmt)| {
        //     let new_stmt = stmt.gen_scopes(Some(new_id), context);
        //     return new_stmt;
        // }).collect();

        // // Update the parent scope with any changes caused by sub statements.
        // // Yes this does mean we loop through all the statements twice. If this
        // // proves to be a performance problem we can optimize it later.
        // // (The alternative is passing mutable data around recursively and that looks
        // // like several dozen migraines in the making.)
        // for (i, stmt) in new_stmts.iter().enumerate() {
        //     match &stmt.data {
        //         Stmt::FunctionDecStmt{ref name, ..} => {
        //             new_scope.declaration_order.insert(name as *const Identifier, i);
        //         },
        //         Stmt::LetStmt{ref typed_name, ..} => {
        //             let raw_name = &typed_name.name as *const Identifier;
        //             new_scope.declaration_order.insert(raw_name, i);
        //             let scope_mod = CanModifyScope::Statement(stmt as *const Node<Stmt>);
        //             new_scope.declarations.insert(raw_name, scope_mod);
        //         },
        //         _ => {}
        //     };
        // }
        // context.add_scope(new_id, new_scope);
        // let new_block = Block{statements: new_stmts};
        // return Node{
        //     id: self.id,
        //     data: new_block,
        //     scope: new_id
        // };
    }
    
    fn gen_scopes2(&mut self, parent_id: usize, context: &Context) -> Context {
        let declarations = BTreeMap::new();
        let declaration_order = BTreeMap::new();

        let mut new_scope = Scope{parent_id: Some(parent_id), declarations, declaration_order};
        let scope_id = general_utils::get_next_scope_id();
        self.scope = scope_id;

        let mut new_context = empty_context();

        for (i, stmt) in self.data.statements.iter_mut().enumerate() {
            // Compute the child contexts.
            let child_context = stmt.gen_scopes2(scope_id, context);
            new_context.extend(child_context);

            // Make updates to the current scope based on the child nodes. 
            // Only function declarations and lets can modify scope.
            match &stmt.data {
                Stmt::FunctionDecStmt{ref name, ..} => {
                    new_scope.declaration_order.insert(name.clone(), i);
                },
                Stmt::LetStmt{ref typed_name, ..} => {
                    new_scope.declaration_order.insert(typed_name.name.clone(), i);
                    let scope_mod = CanModifyScope::Statement(stmt as *const Node<Stmt>);
                    new_scope.declarations.insert(typed_name.name.clone(), scope_mod);
                },
                _ => {}
            };
        }

        new_context.add_scope(scope_id, new_scope);
        return new_context;
    }
}

impl Scoped<Node<Stmt>> for Node<Stmt> {

    fn get_usages(&self) -> HashSet<Identifier> {
        return match self.data {
            Stmt::IfStmt{ref condition, ref block, ref elifs, ref else_block} => {
                let mut usages = general_utils::m_union(condition.get_usages(), block.get_usages());
                for (cond, blck) in elifs {
                    usages = general_utils::m_union(usages, cond.get_usages());
                    usages = general_utils::m_union(usages, blck.get_usages());
                }
                match else_block {
                    Some(y) => general_utils::m_union(usages, y.get_usages()),
                    None => usages
                }
            },
            Stmt::WhileStmt{ref condition, ref block} => general_utils::m_union(condition.get_usages(), block.get_usages()),
            Stmt::FunctionDecStmt{ref block, ..} => block.get_usages(),
            Stmt::ReturnStmt(ref expression) | 
            Stmt::YieldStmt(ref expression) | 
            Stmt::LetStmt{ref expression, ..} |
            Stmt::AssignmentStmt{ref expression, ..} => expression.get_usages(),
            Stmt::BreakStmt | Stmt::ContinueStmt | Stmt::PassStmt => HashSet::new(),
            _ => panic!()
        };
    }

    fn get_true_declarations(&self, context: &Context) -> HashSet<Identifier> {
        return match self.data {
            Stmt::FunctionDecStmt{ref block, ..} => {
                block.get_true_declarations(context)
            },
            _ => HashSet::new()
        };
    }

    fn check_scope(self, _scope: Scope) -> bool {
        panic!();
    }

    /// Check scope and generate
    fn gen_scopes(self, parent_id: Option<usize>, context: &Context) -> Node<Stmt> {
        panic!()
        // let new_node = match self.data {
        //     Stmt::LetStmt{typed_name, expression} => {
        //         let new_value = expression.gen_scopes(parent_id, context);
        //         // Build new scope
        //         // TODO: Reference counted reference to value name.

        //         // Build new statement
        //         let new_let = Stmt::LetStmt{typed_name, expression: new_value};

        //         let declarations = BTreeMap::new();
        //         let declaration_order = BTreeMap::new();
        //         let new_scope = Scope{parent_id, declarations, declaration_order};
        //         let new_id = context.new_scope(new_scope);
        //         let new_node = Node{id: self.id, data: new_let, scope: new_id};
        //         new_node
        //     },
        //     Stmt::FunctionDecStmt{name, args, vararg, kwargs, varkwarg, block, return_type} => {
        //         // TODO: Handle keyword args expressions. They should receive just the parent scope.
        //         let mut declarations = BTreeMap::new();
        //         let mut declaration_order = BTreeMap::new();

        //         // Add arguments to declarations.
        //         for (i, arg) in args.iter().enumerate() {
        //             declaration_order.insert(&arg.name as *const Identifier, i+1);
        //             declarations.insert(&arg.name as *const Identifier, CanModifyScope::Argument);
        //         }

        //         // Add the variable length arguments to declarations.
        //         match vararg {
        //             Some(ref x) => {
        //                 let raw_vararg = x as *const Identifier;
        //                 let index = declaration_order.len() - 1;
        //                 declaration_order.insert(raw_vararg, index);
        //                 declarations.insert(raw_vararg, CanModifyScope::Argument);
        //             },
        //             None => {}
        //         };

        //         // Add the variable length keyword arguments to declarations.
        //         match varkwarg {
        //             Some(ref x) => {
        //                 let raw_vararg = x as *const Identifier;
        //                 let index = declaration_order.len() - 1;
        //                 declaration_order.insert(raw_vararg, index);
        //                 declarations.insert(raw_vararg, CanModifyScope::Argument);
        //             }, 
        //             None => {}
        //         };

        //         let mut new_scope = Scope{parent_id, declarations, declaration_order};
        //         let new_id = context.new_scope(new_scope);

        //         let new_body = block.gen_scopes(Some(new_id), context);
        //         let mut new_stmt = Stmt::FunctionDecStmt{name, args, vararg, kwargs, varkwarg, block: new_body, return_type};

        //         let mut new_node = Node{id: self.id, data: new_stmt, scope: new_id};

        //         new_node
        //     },
        //     Stmt::ReturnStmt(_) => self,
        //     _ => panic!()
        // };

        // return new_node;
    }

    fn gen_scopes2(&mut self, parent_id: usize, context: &Context) -> Context {
        let new_context = match &mut self.data {
            Stmt::LetStmt{typed_name, expression} => {
                self.scope = parent_id;
                expression.gen_scopes2(parent_id, context)
            },
            Stmt::AssignmentStmt{name, expression, ..} => {
                self.scope = parent_id;
                expression.gen_scopes2(parent_id, context)
            },
            Stmt::FunctionDecStmt{name, args, vararg, kwargs, varkwarg, ref mut block, return_type} => {
                // TODO: Handle keyword args expressions. They should receive just the parent scope.
                let mut declarations = BTreeMap::new();
                let mut declaration_order = BTreeMap::new();

                // Add arguments to declarations.
                for (i, arg) in args.iter().enumerate() {
                    declaration_order.insert(arg.name.clone(), i+1);
                    declarations.insert(arg.name.clone(), CanModifyScope::Argument);
                }

                // Add the variable length arguments to declarations.
                match vararg {
                    Some(ref x) => {
                        let index = declaration_order.len() - 1;
                        declaration_order.insert(x.clone(), index);
                        declarations.insert(x.clone(), CanModifyScope::Argument);
                    },
                    None => {}
                };

                // Add the variable length keyword arguments to declarations.
                match varkwarg {
                    Some(ref x) => {
                        let index = declaration_order.len() - 1;
                        declaration_order.insert(x.clone(), index);
                        declarations.insert(x.clone(), CanModifyScope::Argument);
                    }, 
                    None => {}
                };
                
                let mut new_context = empty_context();
                let mut new_scope = Scope{parent_id: Some(parent_id), declarations, declaration_order};
                let scope_id = new_context.new_scope(new_scope);
                self.scope = scope_id;

                let block_context = block.gen_scopes2(scope_id, context);
                new_context.extend(block_context);
                new_context
            },
            Stmt::ReturnStmt(_) => {
                self.scope = parent_id;
                empty_context()
            },
            _ => panic!()
        };

        return new_context;
    }
}

impl Scoped<Node<Expr>> for Node<Expr> {

    fn get_usages(&self) -> HashSet<Identifier> {
        return match self.data {
            Expr::BinaryExpr{ref left, ref right, ..} => {
                general_utils::m_union(left.get_usages(), right.get_usages())
            },
            Expr::ComparisonExpr{ref left, ref right, ..} => {
                general_utils::m_union(left.get_usages(), right.get_usages())
            },
            Expr::UnaryExpr{ref operand, ..} => {
                operand.get_usages()
            },
            Expr::FunctionCall{ref args, ref kwargs, ..} => {
                let mut usages = HashSet::new();
                for expr in args {
                    usages = general_utils::m_union(usages, expr.get_usages());
                }

                for (_, expr) in kwargs {
                    usages = general_utils::m_union(usages, expr.get_usages());
                }

                usages
            },
            Expr::IdentifierExpr(ref name) => {
                let mut usages = HashSet::new();
                usages.insert(name.clone());
                usages
            },
            Expr::Int(_) | Expr::Bool(_) | Expr::String(_) | Expr::Float(_) => HashSet::new(),
            _ => panic!()
        };
    }

    fn get_true_declarations(&self, context: &Context) -> HashSet<Identifier> {
        panic!()
    }

    fn check_scope(self, _scope: Scope) -> bool {
        panic!();
    }

    fn gen_scopes(self, parent_id: Option<usize>, context: &Context) -> Node<Expr> {
        panic!()
        // let new_node: Node<Expr> = match self.data {
        //     Expr::BinaryExpr{operator, left, right} => {
        //         let new_left = left.gen_scopes(parent_id, context);
        //         let new_right = right.gen_scopes(parent_id, context);
        //         let new_expr = Expr::BinaryExpr{operator: operator, left: Box::new(new_left), right: Box::new(new_right)};
        //         let new_id = self.id;
        //         let declarations = BTreeMap::new();
        //         let declaration_order = BTreeMap::new();
        //         let new_scope = Scope{parent_id, declarations, declaration_order};
        //         let new_scope_id = general_utils::get_next_scope_id();
        //         context.add_scope(new_scope_id, new_scope);
        //         Node{id: new_id, data: new_expr, scope: new_scope_id}
        //     },
        //     Expr::Int(_) | Expr::Bool(_) | Expr::IdentifierExpr(_) => {
        //         let declarations = BTreeMap::new();
        //         let declaration_order = BTreeMap::new();
        //         let new_scope = Scope{parent_id, declarations, declaration_order};
        //         let new_scope_id = general_utils::get_next_scope_id();
        //         context.add_scope(new_scope_id, new_scope);
        //         Node{id: self.id, data: self.data, scope: new_scope_id}
        //     },
        //     _ => panic!()
        // };

        // return new_node;
    }

    fn gen_scopes2(&mut self, parent_id: usize, context: &Context) -> Context {
        let new_context = match &mut self.data {
            Expr::BinaryExpr{ref mut operator, ref mut left, ref mut right} => {
                let mut new_context = empty_context();
                new_context.extend(left.gen_scopes2(parent_id, context));
                new_context.extend(right.gen_scopes2(parent_id, context));
                self.scope = parent_id;
                new_context
            },
            Expr::Int(_) | Expr::Float(_) | Expr::Bool(_) | Expr::String(_) | Expr::IdentifierExpr(_) => {
                self.scope = parent_id;
                empty_context()
            },
            _ => panic!()
        };

        return new_context;
    }
}

#[cfg(test)]
mod test {
    use parser;
    use super::*;
    use parser_utils::output;

    #[test]
    fn test_function_locals() {
        let func_str = r#"fn a(b, c):
        return b + c
        "#;
        let mut func_stmt = output(parser::statement(func_str.as_bytes(), 0));
        let context = func_stmt.gen_scopes2(0, &initial_context());
        let usages = func_stmt.get_usages();
        assert!(usages.contains(&Identifier::from("b")));
        assert!(usages.contains(&Identifier::from("c")));
    }

    #[cfg(test)]
    mod scope_generation {
        use super::*;

        #[cfg(test)]
        mod exprs {
            use super::*;

            /// Test scope modifications from literals.
            /// All should be empty.
            #[test]
            fn test_literal_scope() {
                let mut literals: Vec<Node<Expr>> = vec![Node::from(1), Node::from(0.5), Node::from(Expr::String("asdf".to_string())), Node::from(true)];
                let init_context = initial_context();
                for literal in literals.iter_mut() {
                    let context = literal.gen_scopes2(0, &init_context);
                    assert_eq!(context, empty_context());
                }
            }
        }
        
        #[cfg(test)]
        mod stmts {
            use super::*;

            #[test]
            fn test_let_stmt() {
                let mut stmt = output(parser::statement("let a = 1".as_bytes(), 0));
                let context = stmt.gen_scopes2(0, &initial_context());
            }

            fn test_function_decl() {
                let block_str = r#"
                fn a():
                    return 0

                fn b():
                    return 1
                "#;

                let mut block = output(parser::block(block_str.as_bytes(),0 ));
                let context = block.gen_scopes2(0, &initial_context());
                let fn1 = context.get_declaration(block.scope, &Identifier::from("a")).unwrap();
                let fn2 = context.get_declaration(block.scope, &Identifier::from("b")).unwrap();
                unsafe {
                    let fn_node1 = match fn1.extract_stmt() {
                        Stmt::FunctionDecStmt{name, ..} => {
                            assert_eq!(name, Identifier::from("a"))
                        },
                        _ => panic!()
                    };

                    let fn_node1 = match fn1.extract_stmt() {
                        Stmt::FunctionDecStmt{name, .. } => {
                            assert_eq!(name, Identifier::from("b"))
                        },
                        _ => panic!()
                    };
                }
            }
        }

        #[test]
        fn test_block_scope() {
            // Block is:
            // let a = 5 + -1
            // let b = true and false
            let l1 = Node::from(Expr::Int("5".to_string()));
            let r1 = Node::from(Expr::Int("-1".to_string()));
            let l2 = Node::from(true);
            let r2 = Node::from(false);

            let e1 = Expr::BinaryExpr{operator: BinaryOperator::Add, left: Box::new(l1), right: Box::new(r1)};
            let e2 = Expr::BinaryExpr{operator: BinaryOperator::And, left: Box::new(l2), right: Box::new(r2)};

            let s1 = Stmt::LetStmt{typed_name: TypedIdent::from("a"), expression: Node::from(e1)};
            let s2 = Stmt::LetStmt{typed_name: TypedIdent::from("b"), expression: Node::from(e2)};

            let mut block = Node::from(Block{
                statements: vec!(Node::from(s1), Node::from(s2))
            });

            let context = block.gen_scopes2(0, &initial_context());
            let scope = context.get_scope(block.scope);
            assert_eq!(scope.declarations.len(), 2);
            unsafe {
                let stmt1_pointer = context.get_declaration(block.scope, &Identifier::from("a")).unwrap();
                match stmt1_pointer {
                    CanModifyScope::Statement(x) => {
                        match (**x).data {
                            Stmt::LetStmt{ref typed_name, ..} => {
                                assert_eq!(typed_name.clone(), TypedIdent::from("a"))
                            },
                            _ => panic!()
                        }
                    },
                    _ => panic!()
                }

                let stmt1_pointer = context.get_declaration(block.scope, &Identifier::from("b")).unwrap();
                match stmt1_pointer {
                    CanModifyScope::Statement(x) => {
                        match (**x).data {
                            Stmt::LetStmt{ref typed_name, ..} => {
                                assert_eq!(typed_name.clone(), TypedIdent::from("b"))
                            },
                            _ => panic!()
                        }
                    },
                    _ => panic!()
                }
            }
        }
    }


    #[test]
    fn test_get_declarations() {
        let mut func_dec = output(parser::statement("fn a(b):\n let x = 5 + 6\n return x\n".as_bytes(), 0));
        let context = func_dec.gen_scopes2(0, &initial_context());
        let new_ident = Identifier::from("x");
        let actual = func_dec.get_true_declarations(&context);
        for ptr in actual {
            assert_eq!(ptr, new_ident);
        }
    }
}
