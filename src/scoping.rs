use std::collections::BTreeMap;
use std::collections::HashSet;
use std::hash::Hash;
use expression::*;

extern crate cute;

/// Take the union of two hashsets, moving out of the second.
fn m_union<T>(mut a: HashSet<T>,  b: HashSet<T>) -> HashSet<T>
where T: Eq, T: Hash {
    for element in b.into_iter() {
        a.insert(element);
    }
    return a;
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scope {
    pub parent_scope: Option<*const Scope>,
    // TODO: Consider replacing with an insertion ordered map.
    pub declarations: BTreeMap<*const Identifier, CanModifyScope>,
    pub declaration_order: BTreeMap<*const Identifier, usize>
}

impl Scope {
    pub fn get_declaration(&self, name: &Identifier) -> Option<&CanModifyScope> {
        let name_pointer = name as *const Identifier;
        if self.declarations.contains_key(&name_pointer) {
            return self.declarations.get(&name_pointer);
        } else {
            return match self.parent_scope {
                Some(parent) => {
                    unsafe {
                        (*parent).get_declaration(name)
                    }
                },
                None => None
            };
        }
    }
}

/// Create an empty scope.
pub fn empty_scope() -> Scope {
    return Scope{
        parent_scope: None,
        declarations: BTreeMap::new(),
        declaration_order: BTreeMap::new()
    };
}

pub trait Scoped<T> {
    /// Get the scope of the object.

    fn get_usages(&self) -> HashSet<Identifier>;

    /// Get all *non-Argument* declarations.
    fn get_true_declarations(&self) -> HashSet<&*const Identifier>;

    /// Check that all of the objects references are valid.
    fn check_scope(self, scope: Scope) -> bool;

    /// Generate scopes recursively. Returns a new node with the correct scope attached.
    fn gen_scopes(self, parent_scope: &Scope) -> Node<T>;
}

impl Scoped<Module> for Node<Module> {

    fn get_usages(&self) -> HashSet<Identifier> {
        panic!();
    }

    fn get_true_declarations(&self) -> HashSet<&*const Identifier> {
        panic!()
    }

    fn check_scope(self, _scope: Scope) -> bool {
        panic!();
    }

        /// Check scope and generate
    fn gen_scopes(self, parent_scope: &Scope) -> Node<Module> {
        let declarations = BTreeMap::new();
        let declaration_order = BTreeMap::new();

        let mut new_scope = Scope{parent_scope: Some(parent_scope as *const Scope), declarations, declaration_order};

        let new_stmts: Vec<Node<Stmt>> = self.data.declarations.into_iter().enumerate().map(|(_i, stmt)| {
            let new_stmt = stmt.gen_scopes(&new_scope);
            return new_stmt;
        }).collect();

        // Update the parent scope with any changes caused by sub statements.
        // Yes this does mean we loop through all the statements twice. If this
        // proves to be a performance problem we can optimize it later.
        // (The alternative is passing mutable data around recursively and that looks
        // like several dozen migraines in the making.)
        for (i, stmt) in new_stmts.iter().enumerate() {
            match &stmt.data {
                Stmt::FunctionDecStmt{ref name, ..} => {
                    new_scope.declaration_order.insert(name as *const Identifier, i);
                },
                _ => {}
            };
        }

        let new_block = Module{declarations: new_stmts};
        return Node{
            id: self.id,
            data: new_block,
            scope: new_scope
        };
    }
}

impl Scoped<Block> for Node<Block> {

    fn get_usages(&self) -> HashSet<Identifier> {
        let mut usages = HashSet::new();
        for stmt in &self.data.statements {
            usages = m_union(usages, stmt.get_usages());
        }
        return usages;
    }

    fn get_true_declarations(&self) -> HashSet<&*const Identifier> {
        let mut top_level = self.scope.declarations.keys().clone().collect();
        for stmt in &self.data.statements {
            top_level = m_union(top_level, stmt.get_true_declarations());
        }
        return top_level;
    }


    fn check_scope(self, _scope: Scope) -> bool {
        panic!();
    }

    fn gen_scopes(self, parent_scope: &Scope) -> Node<Block> {
        let declarations = BTreeMap::new();
        let declaration_order = BTreeMap::new();

        let mut new_scope = Scope{parent_scope: Some(parent_scope as *const Scope), declarations, declaration_order};

        let new_stmts: Vec<Node<Stmt>> = self.data.statements.into_iter().enumerate().map(|(_i, stmt)| {
            let new_stmt = stmt.gen_scopes(&new_scope);
            return new_stmt;
        }).collect();

        // Update the parent scope with any changes caused by sub statements.
        // Yes this does mean we loop through all the statements twice. If this
        // proves to be a performance problem we can optimize it later.
        // (The alternative is passing mutable data around recursively and that looks
        // like several dozen migraines in the making.)
        for (i, stmt) in new_stmts.iter().enumerate() {
            match &stmt.data {
                Stmt::FunctionDecStmt{ref name, ..} => {
                    new_scope.declaration_order.insert(name as *const Identifier, i);
                },
                Stmt::LetStmt{ref typed_name, ..} => {
                    let raw_name = &typed_name.name as *const Identifier;
                    new_scope.declaration_order.insert(raw_name, i);
                    let scope_mod = CanModifyScope::Statement(stmt as *const Node<Stmt>);
                    new_scope.declarations.insert(raw_name, scope_mod);
                },
                _ => {}
            };
        }

        let new_block = Block{statements: new_stmts};
        return Node{
            id: self.id,
            data: new_block,
            scope: new_scope
        };
    }
}

impl Scoped<Stmt> for Node<Stmt> {

    fn get_usages(&self) -> HashSet<Identifier> {
        return match self.data {
            Stmt::IfStmt{ref condition, ref block, ref elifs, ref else_block} => {
                let mut usages = m_union(condition.get_usages(), block.get_usages());
                for (cond, blck) in elifs {
                    usages = m_union(usages, cond.get_usages());
                    usages = m_union(usages, blck.get_usages());
                }
                match else_block {
                    Some(y) => m_union(usages, y.get_usages()),
                    None => usages
                }
            },
            Stmt::WhileStmt{ref condition, ref block} => m_union(condition.get_usages(), block.get_usages()),
            Stmt::FunctionDecStmt{ref block, ..} => block.get_usages(),
            Stmt::ReturnStmt(ref expression) | 
            Stmt::YieldStmt(ref expression) | 
            Stmt::LetStmt{ref expression, ..} |
            Stmt::AssignmentStmt{ref expression, ..} => expression.get_usages(),
            Stmt::BreakStmt | Stmt::ContinueStmt | Stmt::PassStmt => HashSet::new(),
            _ => panic!()
        };
    }

    fn get_true_declarations(&self) -> HashSet<&*const Identifier> {
        return match self.data {
            Stmt::FunctionDecStmt{ref block, ..} => {
                block.get_true_declarations()
            },
            _ => HashSet::new()
        };
    }

    fn check_scope(self, _scope: Scope) -> bool {
        panic!();
    }

        /// Check scope and generate
    fn gen_scopes(self, parent_scope: &Scope) -> Node<Stmt> {
        let new_node = match self.data {
            Stmt::LetStmt{typed_name, expression} => {
                let new_value = expression.gen_scopes(parent_scope);
                // Build new scope
                // TODO: Reference counted reference to value name.

                // Build new statement
                let new_let = Stmt::LetStmt{typed_name, expression: new_value};

                let new_node = Node{id: self.id, data: new_let, scope: parent_scope.clone()};
                new_node
            },
            Stmt::FunctionDecStmt{name, args, vararg, kwargs, varkwarg, block, return_type} => {
                // TODO: Handle keyword args expressions. They should receive just the parent scope.
                let mut declarations = BTreeMap::new();
                let mut declaration_order = BTreeMap::new();

                // Add arguments to declarations.
                for (i, arg) in args.iter().enumerate() {
                    declaration_order.insert(&arg.name as *const Identifier, i+1);
                    declarations.insert(&arg.name as *const Identifier, CanModifyScope::Argument);
                }

                // Add the variable length arguments to declarations.
                match vararg {
                    Some(ref x) => {
                        let raw_vararg = x as *const Identifier;
                        let index = declaration_order.len() - 1;
                        declaration_order.insert(raw_vararg, index);
                        declarations.insert(raw_vararg, CanModifyScope::Argument);
                    },
                    None => {}
                };

                // Add the variable length keyword arguments to declarations.
                match varkwarg {
                    Some(ref x) => {
                        let raw_vararg = x as *const Identifier;
                        let index = declaration_order.len() - 1;
                        declaration_order.insert(raw_vararg, index);
                        declarations.insert(raw_vararg, CanModifyScope::Argument);
                    }, 
                    None => {}
                };

                let mut new_scope = Scope{parent_scope: Some(parent_scope as *const Scope), declarations, declaration_order};

                let new_body = block.gen_scopes(&new_scope);
                let mut new_stmt = Stmt::FunctionDecStmt{name, args, vararg, kwargs, varkwarg, block: new_body, return_type};

                let mut new_node = Node{id: self.id, data: new_stmt, scope: new_scope};
                // parent_scope.declarations.insert(raw_name, CanModifyScope::Statement(&new_node as *const Node<Stmt>));

                new_node
            },
            Stmt::ReturnStmt(_) => self,
            _ => panic!()
        };

        return new_node;
    }
}

impl Scoped<Expr> for Node<Expr> {

    fn get_usages(&self) -> HashSet<Identifier> {
        return match self.data {
            Expr::BinaryExpr{ref left, ref right, ..} => {
                m_union(left.get_usages(), right.get_usages())
            },
            Expr::ComparisonExpr{ref left, ref right, ..} => {
                m_union(left.get_usages(), right.get_usages())
            },
            Expr::UnaryExpr{ref operand, ..} => {
                operand.get_usages()
            },
            Expr::FunctionCall{ref args, ref kwargs, ..} => {
                let mut usages = HashSet::new();
                for expr in args {
                    usages = m_union(usages, expr.get_usages());
                }

                for (_, expr) in kwargs {
                    usages = m_union(usages, expr.get_usages());
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

    fn get_true_declarations(&self) -> HashSet<&*const Identifier> {
        panic!()
    }


    fn check_scope(self, _scope: Scope) -> bool {
        panic!();
    }

    fn gen_scopes(self, parent_scope: &Scope) -> Node<Expr> {
        
        let new_node: Node<Expr> = match self.data {
            Expr::BinaryExpr{operator, left, right} => {
                let new_left = left.gen_scopes(parent_scope);
                let new_right = right.gen_scopes(parent_scope);
                let new_expr = Expr::BinaryExpr{operator: operator, left: Box::new(new_left), right: Box::new(new_right)};
                let new_id = self.id;
                Node{id: new_id, data: new_expr, scope: parent_scope.clone()}
            },
            Expr::Int(_) | Expr::Bool(_) | Expr::IdentifierExpr(_) => {
                Node{id: self.id, data: self.data, scope: parent_scope.clone()}
            },
            _ => panic!()
        };

        return new_node;
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
        let func_stmt = output(parser::statement(func_str.as_bytes(), 0)).gen_scopes(&empty_scope());
        let usages = func_stmt.get_usages();
        assert!(usages.contains(&Identifier::from("b")));
        assert!(usages.contains(&Identifier::from("c")));
    }

    #[test]
    fn test_scope_generation() {
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

        let block = Node::from(Block{
            statements: vec!(Node::from(s1), Node::from(s2))
        });

        let scoped = block.gen_scopes(&mut empty_scope());
        let stmt_scope = &scoped.data.statements[0].scope;
        assert_eq!(scoped.scope.declarations.len(), 2);

        unsafe {
            let ident1 = match scoped.data.statements[0].data {
                Stmt::LetStmt{ref typed_name, ..} => {
                    &typed_name.name
                },
                _ => panic!()
            };
            let ident1_ptr = ident1 as *const Identifier;
            let actual_ident1 = match scoped.scope.declarations.get(&ident1_ptr).unwrap() {
                CanModifyScope::Statement(ref stmt) => {
                    match (**stmt).data {
                        Stmt::LetStmt{ref typed_name, ..} => {
                            &typed_name.name
                        },
                        _ => panic!()
                    }
                },
                _ => panic!()
            };
            assert_eq!(ident1, actual_ident1);
        }
    }

    #[test]
    fn test_get_declarations() {
        let func_dec = output(parser::statement("fn a(b):\n let x = 5 + 6\n return x\n".as_bytes(), 0)).gen_scopes(&empty_scope());
        let mut expected = HashSet::new();
        expected.insert(&Identifier::from("x") as *const Identifier);
        let actual = func_dec.get_true_declarations();
        panic!()
        // assert_eq!(expected, actual);
    }
}
