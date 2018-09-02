use std::collections::BTreeSet;
use std::fmt::Display;
use std::iter::FromIterator;
use expression::*;
use utils::output;
use ast_node::ASTNode;

extern crate cute;


pub trait ScopedNode: ASTNode {
    fn get_scopes (&self) -> (BTreeSet<String>, BTreeSet<String>);
}

/// ScopedNode for Module
impl ScopedNode for Module {
    fn get_scopes(&self) -> (BTreeSet<String>, BTreeSet<String>) {
        panic!()
    }
}

/// ScopedNode for Stmt
// TODO implement the rest of the kinds of stmt
impl ScopedNode for Stmt {
    fn get_scopes(&self) -> (BTreeSet<String>, BTreeSet<String>) {
        let mut declarations = BTreeSet::new();
        let mut usages = BTreeSet::new();
        match &self {
            &Stmt::LetStmt{ref value_name, ref value} => {
                declarations.insert(value_name.name.to_string());
            }
            &Stmt::AssignmentStmt{ref identifier, ref operator, ref expression} => {
                usages = expression.get_scopes().1;
                usages.insert(identifier.to_string());
            },
            // Currently only handles args and body
            &Stmt::FunctionDecStmt{ref name, ref args, ref vararg, ref keyword_args, ref varkwarg, ref body, ref return_type} => {
                let block_scope_info = body.get_scopes();
                declarations = block_scope_info.0;
                usages = block_scope_info.1;
                for arg in args.iter() {
                    declarations.insert(arg.name.to_string());
                }
            },
            &Stmt::ReturnStmt {ref value} => {
                usages = value.get_scopes().1;
            },
            _ =>  panic!()
        }
        return (declarations, usages);
    }
}

/// ScopedNode for Block
impl ScopedNode for Block {
    fn get_scopes(&self) -> (BTreeSet<String>, BTreeSet<String>) {
        let mut declarations = BTreeSet::new();
        let mut usages = BTreeSet::new();
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
// TODO implement the rest of the kinds of expr
impl ScopedNode for Expr {
    fn get_scopes(&self) -> (BTreeSet<String>, BTreeSet<String>) {
        let declarations: BTreeSet<String> = BTreeSet::new();
        let mut usages: BTreeSet<String> = BTreeSet::new();
        match &self {
            &Expr::ComparisonExpr {ref operator, ref left, ref right} => {
                usages.append(&mut left.get_scopes().1);
                usages.append(&mut right.get_scopes().1);
            },
            &Expr::BinaryExpr {ref operator, ref left, ref right} => {
                usages.append(&mut left.get_scopes().1);
                usages.append(&mut right.get_scopes().1);
            },
            &Expr::UnaryExpr {ref operator, ref operand} => {
                usages.append(&mut operand.get_scopes().1);
            },
            &Expr::IdentifierExpr {ref ident} => {
                usages.insert(ident.to_string());
            },
            &Expr::FunctionCall {ref func_expr, ref args, ref kwargs} => {
                usages.append(&mut func_expr.get_scopes().1);
                for arg in args {
                    usages.append(&mut arg.get_scopes().1);
                }

                match kwargs {
                    Some(x) => {
                        for (_, expr) in x {
                            usages.append(&mut expr.get_scopes().1);
                        }
                    },
                    None => {}
                }
            }
            _ => {}
        }
        return (declarations, usages);
    }
}

#[cfg(test)]
mod test {
    use parser::*;
    use super::*;

    #[test]
    fn test_function_locals() {
        let func_str = r#"fn a(b, c):
    return b + c
"#;
        let func_stmt = output(statement(func_str.as_bytes(), 0));
        let (decs, usgs) = func_stmt.get_scopes();
        let mut real_usgs = BTreeSet::new();
        real_usgs.insert("b".to_string());
        real_usgs.insert("c".to_string());
        assert_eq!(usgs, real_usgs);
    }
}
