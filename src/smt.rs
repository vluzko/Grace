use std::collections::HashSet;

use expression::*;
use typing::{Type, Refinement};
use scoping::Context;
use general_utils::{m_union, get_next_id};

pub fn check_constraints(name: &Identifier, expr: &Node<Expr>, context: &Context, mut constraints: Vec<Refinement>) -> bool {
    constraints.push(Refinement{
        operator: ComparisonOperator::Equal,
        left: Box::new(Node{
            id: 
            scope: expr.scope,
            data: Expr::from(name.clone())
        }),
        right: Box::new(expr.clone())
    });
    let (idents, mut constraint_strings) = call_from_refinement_type(name, expr.scope, constraints, context);
    let (_, python_encoded) = expr.construct_condition(context);
    constraint_strings.push(format!("{} = {}", name.name.clone(), python_encoded));
    println!("{:?}", idents);
    println!("{:?}", constraint_strings);
    panic!()
}


fn call_from_refinement_type(name: &Identifier, scope_id: usize, constraints: &Vec<Refinement>, context: &Context) -> (HashSet<Identifier>, Vec<String>) {
    let mut full_constraints = constraints.clone();
    let mut variables = HashSet::new();
    let mut checked = HashSet::new();
    
    let mut constraint_strings = vec!();
    while full_constraints.len() > 0 {
        let constraint = full_constraints.pop().unwrap();
        println!("constraint: {:?}", constraint);
        let (set, constraint_str) = constraint.construct_condition(context);

        for variable in set.iter() {
            println!("variable: {:?}", variable);
            if !checked.contains(variable) && variable != name {
                let var_scope = context.get_declaring_scope(scope_id, &variable);
                let var_type = context.get_type(scope_id, &variable);
                match var_type {
                    Type::Refinement(_, ref new_conds) => {
                        let (new_idents, mut new_constraints) = call_from_refinement_type(variable, var_scope, new_conds, context);
                        checked = m_union(checked, new_idents.clone());
                        variables = m_union(variables, new_idents);
                        constraint_strings.append(&mut new_constraints);
                    },
                    Type::i32 => {},
                    x => panic!("Can't have a variable of type {:?} in a constraint", x)
                }
                checked.insert(variable.clone());
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


impl ToPython for Refinement {
    fn construct_condition(&self,context: &Context) -> (HashSet<Identifier>, String) {
        let (l_set, l_str) = self.left.construct_condition(context);
        let (r_set, r_str) = self.right.construct_condition(context);
        let f_set = m_union(l_set, r_set);
        return (f_set, format!("({}) {} ({})", l_str, self.operator.to_string(), r_str));
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