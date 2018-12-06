//use std::collections::HashSet;
use std::collections::HashSet;
use expression::*;
use ast_node::*;

extern crate cute;


pub trait ScopedNode: ASTNode {
    fn get_scopes (&self) -> (HashSet<String>, HashSet<String>);
}

/// ScopedNode for Module
impl ScopedNode for Module {
    fn get_scopes(&self) -> (HashSet<String>, HashSet<String>) {
        panic!()
    }
}

/// ScopedNode for Stmt
impl ScopedNode for Stmt {
    fn get_scopes(&self) -> (HashSet<String>, HashSet<String>) {
        let (declarations, usages) = match self {
            &Stmt::LetStmt{ref value_name, ref value} => {
                let temp = value.get_scopes().1;
                (hashset!{value_name.name.to_string()}, temp)
            }
            &Stmt::AssignmentStmt{ref identifier, operator: _, ref expression} => {
                let mut temp = expression.get_scopes().1;
                temp.insert(identifier.to_string());
                (HashSet::new(), temp)
            },
            // Currently only handles args and body
            &Stmt::FunctionDecStmt{name: _, ref args, ref vararg, ref keyword_args, ref varkwarg, ref body, return_type: _} => {
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
            &Stmt::ReturnStmt {ref value} => {
                (HashSet::new(), value.get_scopes().1)
            },
            &Stmt::IfStmt {ref condition, ref main_block, ref elifs, ref else_block} => {
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
            &Stmt::WhileStmt {ref condition, ref block} => {
                let mut condition_usages = condition.get_scopes().1;
                let (block_decs, mut block_usages) = block.get_scopes();
                let temp = condition_usages.union(&block_usages).cloned().collect();
                (block_decs, temp)
            },
            _ =>  panic!()
        };
        return (declarations, usages);
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
}

/// ScopedNode for Expr
impl ScopedNode for Expr {
    fn get_scopes(&self) -> (HashSet<String>, HashSet<String>) {
        let declarations = HashSet::new();
        let usages= match self {
            &Expr::ComparisonExpr {operator: _, ref left, ref right} => {
                left.get_scopes().1.union(&right.get_scopes().1).cloned().collect()
            },
            &Expr::BinaryExpr {operator: _, ref left, ref right} => {
                left.get_scopes().1.union(&right.get_scopes().1).cloned().collect()
            },
            &Expr::UnaryExpr {operator: _, ref operand} => {
                operand.get_scopes().1
            },
            &Expr::IdentifierExpr {ref ident} => {
                hashset!{ident.to_string()}
            },
            &Expr::FunctionCall {ref func_expr, ref args, ref kwargs} => {

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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CanModifyScope {
    Statement(Stmt),
    Expression(Expr)
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scope {
    pub parent_scope: Box<Scope>,
    pub declarations: Vec<(Identifier, CanModifyScope)>
}

pub trait Scoped {
    fn get_scope(&self, parent_scope: Scope) -> Scope;
}

cached!{
    ExprScopes;
    fn get_scope_expr(expr: Expr, parent_scope: Scope) -> Scope = {
        return Scope{parent_scope: Box::new(parent_scope), declarations: vec!()}
    }
}

impl Scoped for Expr {
    fn get_scope(&self, parent_scope: Scope) -> Scope {
        return get_scope_expr(self.clone(), parent_scope)
    }
}



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
        let (_, usgs) = func_stmt.get_scopes();
        let mut real_usgs = HashSet::new();
        real_usgs.insert("b".to_string());
        real_usgs.insert("c".to_string());
        assert_eq!(usgs, real_usgs);
    }
}
