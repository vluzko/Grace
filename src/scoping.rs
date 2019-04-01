//use std::collections::HashSet;
use std::collections::HashSet;
use std::collections::BTreeMap;
use std::collections::BTreeSet;

use expression::*;
use ast_node::*;

extern crate cute;


pub trait ScopedNode: ASTNode {
    fn get_scopes (&self) -> (HashSet<String>, HashSet<String>);

    fn check_scope(&self, parent_scope: Option<Scope>) -> bool;
}

/// ScopedNode for Module
impl ScopedNode for Module {
    fn get_scopes(&self) -> (HashSet<String>, HashSet<String>) {
        panic!()
    }

    fn check_scope(&self, parent_scope: Option<Scope>) -> bool {
        return false;
    }
}

/// ScopedNode for Stmt
impl ScopedNode for Stmt {
    fn get_scopes(&self) -> (HashSet<String>, HashSet<String>) {
        let (declarations, usages) = match self {
            &Stmt::LetStmt{ref value_name, ref value, ..} => {
                let temp = value.get_scopes().1;
                (hashset!{value_name.name.to_string()}, temp)
            }
            &Stmt::AssignmentStmt{ref identifier, ref expression, ..} => {
                let mut temp = expression.get_scopes().1;
                temp.insert(identifier.to_string());
                (HashSet::new(), temp)
            },
            // Currently only handles args and body
            &Stmt::FunctionDecStmt{name: _, ref args, ref vararg, ref keyword_args, ref varkwarg, ref body, ..} => {
                let (mut td, mut tu) = body.get_scopes();
                // Include function name
//                td.insert(name.to_string());

                // Include args
                for arg in args.iter() {
                    td.insert(arg.name.to_string());
                }

                // Include keyword_args
                match keyword_args {
                    Some(x) => {
                        for (ident, expr) in x {
                            td = td.union(&expr.get_scopes().1).cloned().collect();
                            tu.insert(ident.to_string());
                        }
                        {}
                    },
                    None => {}
                }

                // Include vararg
                match vararg {
                    Some(x) => {
                        td.insert(x.to_string());
                        {}
                    },
                    None => {}
                }

                // Include varkwargs
                match varkwarg {
                    Some(x) => {
                        td.insert(x.to_string());
                        {}
                    },
                    None => {}
                }

                (td, tu)
            },
            &Stmt::ReturnStmt {ref value, ..} => {
                (HashSet::new(), value.get_scopes().1)
            },
            &Stmt::IfStmt {ref condition, ref main_block, ref elifs, ref else_block, ..} => {
                let (mut td, mut tu) = condition.get_scopes();

                let (md, mu) = main_block.get_scopes();
                td = td.union(&md).cloned().collect();
                tu = tu.union(&mu).cloned().collect();

                for (cond, block) in elifs {
                    tu = tu.union(&cond.get_scopes().1).cloned().collect();
                    let (bd, bu) = block.get_scopes();
                    tu = tu.union(&bu).cloned().collect();
                    td = td.union(&bd).cloned().collect();
                }

                match else_block {
                    Some(x) => {
                        let (ed, eu) = x.get_scopes();
                        td = td.union(&ed).cloned().collect();
                        tu = tu.union(&eu).cloned().collect();
                        {}
                    },
                    None => {}
                }
                (td, tu)
            },
            &Stmt::WhileStmt {ref condition, ref block, ..} => {
                let mut condition_usages = condition.get_scopes().1;
                let (block_decs, mut block_usages) = block.get_scopes();
                let temp = condition_usages.union(&block_usages).cloned().collect();
                (block_decs, temp)
            },
            _ =>  panic!()
        };
        return (declarations, usages);
    }

    fn check_scope(&self, parent_scope: Option<Scope>) -> bool {
        return false;
    }
}

/// ScopedNode for Block
impl ScopedNode for Block {
    fn get_scopes(&self) -> (HashSet<String>, HashSet<String>) {
        let mut declarations: HashSet<String> = HashSet::new();
        let mut usages: HashSet<String> = HashSet::new();
        for statement in &self.statements {
            let statement_scope_info = statement.get_scopes();
            for declaration in statement_scope_info.0 {
                declarations.insert(declaration.to_string());
            }
            for usage in statement_scope_info.1 {
                usages.insert(usage.to_string());
            }
        }
        return (declarations, usages);
    }

    fn check_scope(&self, parent_scope: Option<Scope>) -> bool {
        return false;
    }
}

/// ScopedNode for Expr
impl ScopedNode for Expr {
    fn get_scopes(&self) -> (HashSet<String>, HashSet<String>) {
        let declarations = HashSet::new();
        let usages= match self {
            &Expr::ComparisonExpr {ref left, ref right, ..} => {
                left.get_scopes().1.union(&right.get_scopes().1).cloned().collect()
            },
            &Expr::BinaryExpr {ref left, ref right, ..} => {
                left.get_scopes().1.union(&right.get_scopes().1).cloned().collect()
            },
            &Expr::UnaryExpr {ref operand, ..} => {
                operand.get_scopes().1
            },
            &Expr::IdentifierExpr {ref ident, ..} => {
                hashset!{ident.to_string()}
            },
            &Expr::FunctionCall {ref func_expr, ref args, ref kwargs, ..} => {

                let func_expr_usages = func_expr.get_scopes().1;

                let mut arg_usages = HashSet::new();
//                usages.append(&mut func_expr.get_scopes().1);
                for arg in args {
//                    usages.append(&mut arg.get_scopes().1);
                    arg_usages = arg_usages.union(&arg.get_scopes().1).cloned().collect();
                }

                let mut kwargs_usages = HashSet::new();
                for (_, expr) in kwargs {
                    kwargs_usages = kwargs_usages.union(&expr.get_scopes().1).cloned().collect();
                }
                let temp: HashSet<String> = func_expr_usages.union(&arg_usages).cloned().collect();
                temp.union(&kwargs_usages).cloned().collect()
            }
            _ => HashSet::new()
        };
        return (declarations, usages);
    }

    fn check_scope(&self, parent_scope: Option<Scope>) -> bool {
        return true;
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CanModifyScope {
    Statement(*const IdNode<Stmt2>),
    Expression(*const IdNode<Expr2>),
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
    pub fn get_declaration(self, name: Identifier) -> Option<CanModifyScope> {
        panic!();
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
    fn get_scope(self, parent_scope: Scope) -> Scope;

    /// Check that all of the objects references are valid.
    fn check_scope2(self, scope: Scope) -> bool;

    /// Generate scopes recursively.
    fn gen_scopes(self, parent_scope: &Scope) -> IdNode<T>;
}

impl Scoped<Expr2>  for IdNode<Expr2> {
    fn get_scope(self, parent_scope: Scope) -> Scope {
        panic!()
    }

    fn check_scope2(self, scope: Scope) -> bool {
        panic!();
    }

        /// Check scope and generate
    fn gen_scopes(self, parent_scope: &Scope) -> IdNode<Expr2> {
        
        let new_node: IdNode<Expr2> = match self.data {
            Expr2::BinaryExpr{operator, left, right} => {
                let new_left = left.gen_scopes(parent_scope);
                let new_right = right.gen_scopes(parent_scope);
                let new_expr = Expr2::BinaryExpr{operator: operator, left: Box::new(new_left), right: Box::new(new_right)};
                let new_id = self.id;
                IdNode{id: new_id, data: new_expr, scope: parent_scope.clone()}
            },
            Expr2::Int(_) | Expr2::Bool(_) | Expr2::IdentifierExpr(_) => {
                IdNode{id: self.id, data: self.data, scope: parent_scope.clone()}
            },
            _ => panic!()
        };

        return new_node;

        // Error out if scope doesn't check.
        // TODO: This shouldn't be a literal panic.
        // if ! self.check_scope2(parent_scope) {
        //     panic!();
        // }

//        let ided_node = IDedNode::from(self);

        panic!();
    }
}

impl Scoped<Stmt2> for IdNode<Stmt2> {
    fn get_scope(self, parent_scope: Scope) -> Scope {
        panic!()
    }

    fn check_scope2(self, scope: Scope) -> bool {
        panic!();
    }

        /// Check scope and generate
    fn gen_scopes(self, parent_scope: &Scope) -> IdNode<Stmt2> {
        let new_node = match self.data {
            Stmt2::LetStmt{typed_name, expression} => {
                
                let new_value = expression.gen_scopes(parent_scope);
                // Build new scope
                // let mut declarations = BTreeSet::new();
                // let mut declaration_order = BTreeMap:: new();
                // TODO: Reference counted reference to value name.
                // let new_scope = Scope{parent_scope: Some(Box::new(parent_scope.clone())), declarations, declaration_order};

                // Build new statement
                let identifier_pointer = &typed_name.name as *const Identifier;
                let new_let = Stmt2::LetStmt{typed_name, expression: new_value};

                let new_node = IdNode{id: self.id, data: new_let, scope: parent_scope.clone()};
                let raw_pointer = &new_node as *const IdNode<Stmt2>;
                // parent_scope.declarations.insert(identifier_pointer, CanModifyScope::Statement(raw_pointer));
                // parent_scope.declaration_order.insert(identifier_pointer, parent_scope.declarations.len() - 1);
                new_node
            },
            Stmt2::FunctionDecStmt{name, args, vararg, kwargs, varkwarg, block, return_type} => {
                // TODO: Handle keyword args expressions. They should receive just the parent scope.
                let mut declarations = BTreeMap::new();
                let mut declaration_order = BTreeMap::new();

                // declarations.insert(&name as *const Identifier, &self as *const IdNode<Stmt2>);
                let raw_name = &name as *const Identifier;
                // let k = parent_scope.declaration_order.len();
                // parent_scope.declaration_order.insert(raw_name, k - 1);

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
                let mut new_stmt = Stmt2::FunctionDecStmt{name, args, vararg, kwargs, varkwarg, block: new_body, return_type};

                let mut new_node = IdNode{id: self.id, data: new_stmt, scope: new_scope};
                // parent_scope.declarations.insert(raw_name, CanModifyScope::Statement(&new_node as *const IdNode<Stmt2>));

                new_node
            },
            _ => panic!()
        };

        return new_node;
    }
}

impl Scoped<Block2> for IdNode<Block2> {
    fn get_scope(self, parent_scope: Scope) -> Scope {
        panic!()
    }

    fn check_scope2(self, scope: Scope) -> bool {
        panic!();
    }

        /// Check scope and generate
    fn gen_scopes(self, parent_scope: &Scope) -> IdNode<Block2> {
        let mut declarations = BTreeMap::new();
        let mut declaration_order = BTreeMap::new();

        

        let mut new_scope = Scope{parent_scope: Some(parent_scope as *const Scope), declarations, declaration_order};

        let new_stmts: Vec<IdNode<Stmt2>> = self.data.statements.into_iter().enumerate().map(|(i, stmt)| {
            
            let new_stmt = stmt.gen_scopes(&new_scope);
            // if !new_stmt.scope.declarations.is_empty() {
            //     declarations.append(new_stmt.scope.declarations.)
            // }
            return new_stmt;
        }).collect();

        // Update the parent scope with any changes caused by sub statements.
        // Yes this does mean we loop through all the statements twice. If this
        // proves to be a performance problem we can optimize it later.
        // (The alternative is passing mutable data around recursively and that looks
        // like several dozen migraines in the making.)
        for (i, stmt) in new_stmts.iter().enumerate() {
            match &stmt.data {
                Stmt2::FunctionDecStmt{ref name, ..} => {
                    new_scope.declaration_order.insert(name as *const Identifier, i);
                },
                Stmt2::LetStmt{ref typed_name, ..} => {
                    let raw_name = &typed_name.name as *const Identifier;
                    new_scope.declaration_order.insert(raw_name, i);
                    let scope_mod = CanModifyScope::Statement(stmt as *const IdNode<Stmt2>);
                    new_scope.declarations.insert(raw_name, scope_mod);
                },
                _ => {}
            };
        }

        let new_block = Block2{statements: new_stmts};
        return IdNode{
            id: self.id,
            data: new_block,
            scope: new_scope
        };
    }
}


// impl <t> idnode<t> {
//     /// find all the references for identifiers used in this node.
//     /// let a = b, for instance, should return a reference to whatever node created b
//     pub fn get_original_declarations(self) -> canmodifyscope {
//         panic!()
//     }
// }

//cached!{
//    exprscopes;
//    fn get_scope_expr(expr: expr, parent_scope: scope) -> scope = {
//        return scope{parent_scope: box::new(parent_scope), declarations: vec!()}
//    }
//}

//impl scoped for expr {
//    fn get_scope(&self, parent_scope: scope) -> scope {
//        return get_scope_expr(self.clone(), parent_scope)
//    }
//}



#[cfg(test)]
mod test {
    use parser::*;
    use super::*;
    use utils::output;

    #[test]
    fn test_function_locals() {
        let func_str = r#"fn a(b, c):
    return b + c
"#;
        let func_stmt = output(statement(func_str.as_bytes(), 0));
        panic!();
        // let (_, usgs) = func_stmt.get_scopes();
        // let mut real_usgs = HashSet::new();
        // real_usgs.insert("b".to_string());
        // real_usgs.insert("c".to_string());
        // assert_eq!(usgs, real_usgs);
    }

    #[test]
    fn test_scope_generation() {
        // Block is:
        // let a = 5 + -1
        // let b = true and false
        let l1 = IdNode::from(Expr2::Int("5".to_string()));
        let r1 = IdNode::from(Expr2::Int("-1".to_string()));
        let l2 = IdNode::from(Expr2::Bool("true".to_string()));
        let r2 = IdNode::from(Expr2::Bool("false".to_string()));

        let e1 = Expr2::BinaryExpr{operator: BinaryOperator::Add, left: Box::new(l1), right: Box::new(r1)};
        let e2 = Expr2::BinaryExpr{operator: BinaryOperator::And, left: Box::new(l2), right: Box::new(r2)};

        let s1 = Stmt2::LetStmt{typed_name: TypedIdent::from("a"), expression: IdNode::from(e1)};
        let s2 = Stmt2::LetStmt{typed_name: TypedIdent::from("b"), expression: IdNode::from(e2)};

        let block = IdNode::from(Block2{
            statements: vec!(IdNode::from(s1), IdNode::from(s2))
        });

        let scoped = block.gen_scopes(&mut empty_scope());
        let stmt_scope = &scoped.data.statements[0].scope;
        assert_eq!(scoped.scope.declarations.len(), 2);

        unsafe {
            let ident1 = match scoped.data.statements[0].data {
                Stmt2::LetStmt{ref typed_name, ..} => {
                    &typed_name.name
                },
                _ => panic!()
            };
            let ident1_ptr = ident1 as *const Identifier;
            let actual_ident1 = match scoped.scope.declarations.get(&ident1_ptr).unwrap() {
                CanModifyScope::Statement(ref stmt) => {
                    match (**stmt).data {
                        Stmt2::LetStmt{ref typed_name, ..} => {
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
}
