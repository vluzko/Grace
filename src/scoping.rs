use std::collections::BTreeMap;
use std::collections::HashSet;
use std::collections::HashMap;
use std::hash::Hash;
use expression::*;
use general_utils;
use compiler_layers;
use typing::Type;

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
    Argument(*const Node<Stmt>, usize)
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

    /// Generate scopes recursively. Returns all scopes.
    fn gen_scopes(&mut self, parent_id: usize, context: &Context) -> Context;
}

/// Create an empty scope.
pub fn empty_scope() -> Scope {
    return Scope{
        parent_id: None,
        declarations: BTreeMap::new(),
        declaration_order: BTreeMap::new()
    };
}

/// Create a context containing only the empty scope.
pub fn initial_context() -> (usize, Context) {
    let empty = empty_scope();
    let mut init_scopes = HashMap::new();
    let id = general_utils::get_next_scope_id();
    init_scopes.insert(id, empty);
    let context = Context{scopes: init_scopes, containing_scopes: HashMap::new()};
    return (id, context);
}

pub fn empty_context() -> Context {
    let scopes = HashMap::new();
    return Context{scopes, containing_scopes: HashMap::new()};
}

impl Context {
    pub fn get_scope(&self, scope_id: usize) -> &Scope {
        return self.scopes.get(&scope_id).unwrap();
    }

    pub fn get_mut_scope(&mut self, scope_id: usize) -> &mut Scope {
        return self.scopes.get_mut(&scope_id).unwrap();
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
        // println!("Name: {:?}\nScope: {:?}\n", name, self.scopes.get(&scope_id));
        let initial_scope = self.scopes.get(&scope_id).unwrap();
        if scope_id == 0 {
            panic!("Reached scope id 0");
        } else if initial_scope.declarations.contains_key(name) {
            return initial_scope.declarations.get(name);
        } else {
            // println!("Scope reached: {:?}", initial_scope.parent_id);
            return match initial_scope.parent_id {
                Some(id) => {
                    self.get_declaration(id, name)
                },
                None => None
            };
        }
    }

    pub fn get_type(&self, scope_id: usize, name: &Identifier, type_map: &HashMap<usize, Type>) -> Type {
        let scope_mod = self.get_declaration(scope_id, name).unwrap();
        let t = unsafe {
            match scope_mod {
                CanModifyScope::Statement(ptr) => {
                    type_map.get(&(**ptr).id).unwrap().clone()
                },
                _ => panic!()
            }
        };
        return t;
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

    fn gen_scopes(&mut self, parent_id: usize, context: &Context) -> Context {
        let declarations = BTreeMap::new();
        let declaration_order = BTreeMap::new();

        let mut new_scope = Scope{parent_id: Some(parent_id), declarations, declaration_order};
        let scope_id = general_utils::get_next_scope_id();
        self.scope = scope_id;

        let mut new_context = empty_context();
        new_context.add_scope(scope_id, new_scope);

        for (i, stmt) in self.data.declarations.iter_mut().enumerate() {
            let child_context = stmt.gen_scopes(scope_id, context);
            new_context.extend(child_context);
            // match &stmt.data {
            //     Stmt::FunctionDecStmt{ref name, ..} => {
            //         let scope_mod = CanModifyScope::Statement(Box::into_raw(stmt.clone()));
            //         new_scope.declarations.insert(name.clone(), scope_mod);
            //         new_scope.declaration_order.insert(name.clone(), i);
            //     },
            //     Stmt::ReturnStmt(name) => {},
            //     _ => {}
            // };
        }

        self.update_scope(&mut new_context);

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
    
    fn gen_scopes(&mut self, parent_id: usize, context: &Context) -> Context {
        let declarations = BTreeMap::new();
        let declaration_order = BTreeMap::new();

        let mut new_scope = Scope{parent_id: Some(parent_id), declarations, declaration_order};
        let scope_id = general_utils::get_next_scope_id();
        self.scope = scope_id;
        let mut new_context = empty_context();
        new_context.add_scope(scope_id, new_scope);
        // Compute the child contexts.
        for (i, stmt) in self.data.statements.iter_mut().enumerate() {
            let child_context = stmt.gen_scopes(scope_id, context);
            new_context.extend(child_context);
        }

        self.update_scope(&mut new_context);

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

    fn gen_scopes(&mut self, parent_id: usize, context: &Context) -> Context {
        let raw_pointer = self as *const Node<Stmt>;
        let new_context = match &mut self.data {
            Stmt::LetStmt{typed_name, expression} => {
                self.scope = parent_id;
                expression.gen_scopes(parent_id, context)
            },
            Stmt::AssignmentStmt{name, expression, ..} => {
                self.scope = parent_id;
                expression.gen_scopes(parent_id, context)
            },
            Stmt::FunctionDecStmt{name, args, vararg, kwargs, varkwarg, ref mut block, return_type} => {
                // TODO: Handle keyword args expressions. They should receive just the parent scope.
                let mut declarations = BTreeMap::new();
                let mut declaration_order = BTreeMap::new();

                // Add arguments to declarations.
                for (i, arg) in args.iter().enumerate() {
                    declaration_order.insert(arg.0.clone(), i+1);
                    declarations.insert(arg.0.clone(), CanModifyScope::Argument(raw_pointer, i));
                }

                // Add the variable length arguments to declarations.
                match vararg {
                    Some(ref x) => {
                        let index = declaration_order.len() - 1;
                        declaration_order.insert(x.clone(), index);
                        declarations.insert(x.clone(), CanModifyScope::Argument(raw_pointer, args.len()+1));
                    },
                    None => {}
                };

                // Add the variable length keyword arguments to declarations.
                match varkwarg {
                    Some(ref x) => {
                        let index = declaration_order.len() - 1;
                        declaration_order.insert(x.clone(), index);
                        declarations.insert(x.clone(), CanModifyScope::Argument(raw_pointer, args.len()+2));
                    }, 
                    None => {}
                };
                
                let mut new_context = empty_context();
                let mut new_scope = Scope{parent_id: Some(parent_id), declarations, declaration_order};
                let scope_id = new_context.new_scope(new_scope);
                self.scope = scope_id;

                let block_context = block.gen_scopes(scope_id, context);
                new_context.extend(block_context);
                new_context
            },
            Stmt::ReturnStmt(expression) => {
                self.scope = parent_id;
                expression.gen_scopes(parent_id, context)
            },
            Stmt::WhileStmt{condition, block} => {
                let mut condition_context = condition.gen_scopes(parent_id, context);
                let block_context = block.gen_scopes(parent_id, context);
                condition_context.extend(block_context);
                self.scope = parent_id;
                condition_context
            },
            Stmt::IfStmt{condition, block, elifs, else_block} => {
                let mut condition_context = condition.gen_scopes(parent_id, context);
                let block_context = block.gen_scopes(parent_id, context);
                condition_context.extend(block_context);

                for (elif_cond, elif_block) in elifs {
                    condition_context.extend(elif_cond.gen_scopes(parent_id, context));
                    condition_context.extend(elif_block.gen_scopes(parent_id, context));
                }

                match else_block {
                    Some(b) => condition_context.extend(b.gen_scopes(parent_id, context)),
                    None => {}
                };

                self.scope = parent_id;
                condition_context
            }
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

    fn gen_scopes(&mut self, parent_id: usize, context: &Context) -> Context {
        let new_context = match &mut self.data {
            Expr::BinaryExpr{ref mut operator, ref mut left, ref mut right} => {
                let mut new_context = empty_context();
                new_context.extend(left.gen_scopes(parent_id, context));
                new_context.extend(right.gen_scopes(parent_id, context));
                self.scope = parent_id;
                new_context
            },
            Expr::Int(_) | Expr::Float(_) | Expr::Bool(_) | Expr::String(_) | Expr::IdentifierExpr(_) => {
                self.scope = parent_id;
                empty_context()
            },
            Expr::FunctionCall{ref mut function, ref mut args, ref mut kwargs} => {
                let mut new_context = empty_context();
                new_context.extend(function.gen_scopes(parent_id, context));
                for arg in args {
                    new_context.extend(arg.gen_scopes(parent_id, context));
                }
                for (_, kwarg) in kwargs {
                    new_context.extend(kwarg.gen_scopes(parent_id, context));
                }
                self.scope = parent_id;
                new_context
            },
            Expr::ComparisonExpr{ref mut left, ref mut right, ..} => {
                let mut new_context = empty_context();
                new_context.extend(left.gen_scopes(parent_id, context));
                new_context.extend(right.gen_scopes(parent_id, context));
                self.scope = parent_id;
                new_context
            },
            _ => panic!()
        };

        return new_context;
    }
}

impl Node<Block> {
    /// Update a context with the declarations in this block.
    /// All declarations will be either functions or let statements.
    pub fn update_scope(&self, context: &mut Context) {

        let scope = context.get_mut_scope(self.scope);
        for (i, stmt) in self.data.statements.iter().enumerate() {
            // Make updates to the current scope based on the child nodes. 
            // Only function declarations and lets can modify scope.
            match &stmt.data {
                Stmt::FunctionDecStmt{ref name, ..} => {
                    scope.declaration_order.insert(name.clone(), i);
                    let scope_mod = CanModifyScope::Statement(stmt.as_ref() as *const _);
                    scope.declarations.insert(name.clone(), scope_mod);
                },
                Stmt::LetStmt{ref typed_name, ..} => {
                    scope.declaration_order.insert(typed_name.name.clone(), i);
                    let scope_mod = CanModifyScope::Statement(stmt.as_ref() as *const _);
                    scope.declarations.insert(typed_name.name.clone(), scope_mod);
                },
                _ => {}
            };
        }

    }
}

impl Node<Module> {
    /// Update a context with the declarations in this module.
    /// All declarations will be either functions or import statements.
    pub fn update_scope(&self, context: &mut Context) {
        let scope = context.get_mut_scope(self.scope);
        for (i, dec) in self.data.declarations.iter().enumerate() {
            match &dec.data {
                Stmt::FunctionDecStmt{ref name, ..} => {
                    let scope_mod = CanModifyScope::Statement(dec.as_ref() as *const _);
                    scope.declarations.insert(name.clone(), scope_mod);
                    scope.declaration_order.insert(name.clone(), i);
                },
                Stmt::ReturnStmt(name) => {
                    // TODO: Add module
                },
                _ => {
                    //TODO: panic
                }
            };
        }
        
    }
}

#[cfg(test)]
mod test {
    use parser;
    use super::*;
    use parser_utils::output;

    #[test]
    fn test_function_locals() {
        let func_str = r#"fn a(b: i32, c: i32) -> i32:
        return b + c
        "#;
        let mut func_stmt = output(parser::statement(func_str.as_bytes(), 0));
        let (id, init) = initial_context();
        let context = func_stmt.gen_scopes(id, &init);
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
                let (id, init) = initial_context();
                for literal in literals.iter_mut() {
                    let context = literal.gen_scopes(id, &init);
                    assert_eq!(context, empty_context());
                }
            }
        }
        
        #[cfg(test)]
        mod stmts {
            use super::*;

            #[test]
            fn test_let_stmt() {
                let (block, context) = compiler_layers::to_scopes::<Node<Block>>("let a = 1".as_bytes());
                assert_eq!(context.scopes.len(), 2);
                let scope = context.get_scope(block.scope);
                assert_eq!(scope.declarations.len(), 1);
                assert!(scope.declarations.contains_key(&Identifier::from("a")));
            }

            #[test]
            fn test_function_decl() {
                let block_str = r#"
                fn a():
                    return 0

                fn b():
                    return 1
                "#;

                let mut block = output(parser::block(block_str.as_bytes(),0 ));
                let (id, init) = initial_context();
                let context = block.gen_scopes(id, &init);
                let fn1 = context.get_declaration(block.scope, &Identifier::from("a")).unwrap();
                let fn2 = context.get_declaration(block.scope, &Identifier::from("b")).unwrap();
                let fn_node1 = match fn1.extract_stmt() {
                    Stmt::FunctionDecStmt{name, ..} => {
                        assert_eq!(name, Identifier::from("a"))
                    },
                    _ => panic!()
                };

                let fn_node2 = match fn2.extract_stmt() {
                    Stmt::FunctionDecStmt{name, .. } => {
                        assert_eq!(name, Identifier::from("b"))
                    },
                    _ => panic!()
                };
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
                statements: vec!(Box::new(Node::from(s1)), Box::new(Node::from(s2)))
            });
            let (id, init) = initial_context();
            let context = block.gen_scopes(id, &init);
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
        let mut func_dec = output(parser::statement("fn a(b: i32) -> i32:\n let x = 5 + 6\n return x\n".as_bytes(), 0));
        let (id, init) = initial_context();
        let context = func_dec.gen_scopes(id, &init);
        let new_ident = Identifier::from("x");
        let actual = func_dec.get_true_declarations(&context);
        for ptr in actual {
            assert_eq!(ptr, new_ident);
        }
    }

    struct Temp {
        a: i32,
        b: f64
    }

    fn make_box() -> Box<Temp> {
        let val = Temp{a: 0, b: 1.5};
        let b = Box::new(val);
        println!("Address at start: {:?}", b.as_ref() as *const Temp);
        return b;
    }

    fn proc_box(b: &mut Box<Temp>){
        b.a = 1;
        // return b;
    }

    #[test]
    fn test_ptrs() {
        let mut b = make_box();
        println!("Address after return: {:?}", b.as_ref() as *const Temp);
        proc_box(&mut b);
        println!("Address after proc: {:?}", b.as_ref() as *const Temp);
    }
}
