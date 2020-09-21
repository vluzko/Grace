use std::collections::HashSet;
use std::process::Command;

use itertools::join;

use expression::*;
use scoping::{Context, CanModifyScope, Type, Refinement};
use general_utils::{m_union};



pub fn check_constraints(scope_id: usize, context: &Context, constraints: Vec<Refinement>) -> bool {
    let (idents, constraint_strings) = call_from_refinement_type(scope_id, &constraints, context);
    let var_name_string = join(idents.iter().map(|x| x.name.clone()), ",");
    let constraints_str = join(constraint_strings, ",");
    let args = &["src/smt.py", var_name_string.as_str(), constraints_str.as_str()];
    let res = Command::new("python").args(args).output();
    return match res {
        Ok(output) => output.stdout != "no solution\n".as_bytes(),
        Err(_) => false
    };
}


fn call_from_refinement_type(scope_id: usize, constraints: &Vec<Refinement>, context: &Context) -> (HashSet<Identifier>, Vec<String>) {
    let mut full_constraints = constraints.clone();
    let mut variables = HashSet::new();
    let mut checked = HashSet::new();
    
    let mut constraint_strings = vec!();
    while full_constraints.len() > 0 {
        let constraint = full_constraints.pop().unwrap();
        let (set, constraint_str) = constraint.construct_condition(context);

        for variable in set.iter() {
            if !checked.contains(variable) {
                // Add the declaration to the constraints.
                let var_dec_opt = context.get_declaration(scope_id, &variable);
                match var_dec_opt {
                    Some(var_dec) => {
                        let (dec_set, dec_str) = var_dec.construct_condition(context);
                        checked = m_union(checked, dec_set.clone());
                        variables = m_union(variables, dec_set);
                        constraint_strings.push(format!("{} == {}", variable, dec_str));

                        // Add the type to the constraints.
                        let var_scope = context.get_declaring_scope(scope_id, &variable);
                        let var_type = context.get_type(scope_id, &variable);
                        match var_type {
                            Type::Refinement(_, ref new_conds) => {
                                let (new_idents, mut new_constraints) = call_from_refinement_type(var_scope, new_conds, context);
                                checked = m_union(checked, new_idents.clone());
                                variables = m_union(variables, new_idents);
                                constraint_strings.append(&mut new_constraints);
                            },
                            Type::i32 => {},
                            x => panic!("Can't have a variable of type {:?} in a constraint", x)
                        }
                        checked.insert(variable.clone());
                    },
                    None => {}
                }
            }
        }

        variables = m_union(variables, set);
        constraint_strings.push(constraint_str);
    }
    return (variables, constraint_strings);
}

/// Construct a call to Z3 using the Python API
trait ToPython {
    fn construct_condition(&self, &Context) -> (HashSet<Identifier>, String);
}

impl ToPython for CanModifyScope {
    fn construct_condition(&self, context: &Context) -> (HashSet<Identifier>, String) {
        return match self {
            CanModifyScope::Statement(ref raw_stmt, _) => {
                unsafe {
                    (**raw_stmt).construct_condition(context)
                }
            },
            _ => panic!()
        }
    }
}

impl ToPython for Refinement {
    fn construct_condition(&self,context: &Context) -> (HashSet<Identifier>, String) {
        let (l_set, l_str) = self.left.construct_condition(context);
        let (r_set, r_str) = self.right.construct_condition(context);
        let f_set = m_union(l_set, r_set);
        return (f_set, format!("({}) {} ({})", l_str, self.operator.to_string(), r_str));
    }
}

impl ToPython for Node<Stmt> {
    fn construct_condition(&self, context: &Context) -> (HashSet<Identifier>, String) {
        return match &self.data {
            Stmt::LetStmt{ref expression, ..} => expression.construct_condition(context),
            x => panic!("ToPython not implemented for {:?}", x)
        }
    }
}

impl ToPython for Node<Expr> {
    
    fn construct_condition(&self, context: &Context) -> (HashSet<Identifier>, String) {
        return match &self.data {
            Expr::BinaryExpr{ref operator, ref left, ref right} => {
                let (l_set, l_str) = left.construct_condition(context);
                let (r_set, r_str) = right.construct_condition(context);
                let f_set = m_union(l_set, r_set);
                (f_set, format!("({}) {} ({})", l_str, operator.to_string(), r_str))
            },
            Expr::IdentifierExpr(ref name) => {
                let mut set = HashSet::new();
                set.insert(name.clone());
                (set, name.name.clone())
            },
            Expr::Int(value) | Expr::Float(value) => (HashSet::new(), value.clone()),
            x => panic!("Not a legal constraint: {:?}", x)
        };
    }
}