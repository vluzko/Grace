/// Low-level representation of WebAssembly.
use std::collections::HashMap;
use petgraph::{graph::Neighbors, Outgoing, visit::EdgeRef};
use cfg::{Cfg, CfgVertex, CfgStmt};
use expression::{Node, Module, Expr, Stmt, BinaryOperator, ComparisonOperator, Identifier};
use scoping::{Context, GetContext};
use std::convert::From;
use typing::Type;

/// A representation of a WASM module.
/// Includes function declarations, imports, and memory declarations.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct WASMModule {
    imports: Vec<String>,
    functions: Vec<WASMFunc>
}

/// A WASM function declaration
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct WASMFunc {
    name: String, 
    args: Vec<(String, WASMType)>, 
    locals: Vec<(String, WASMType)>, 
    result: WASMType, 
    code: Vec<WASM>
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WASM {
    Block,
    Loop,
    End,
    If,
    Else,
    Operation(WASMOperator, WASMType),
    Const(WASMType, String),
    Call(String),
    Branch(usize),
    BranchIf(usize),
    Get(String),
    Set(String),
    Tee(String),
    Load(WASMType),
    Store(WASMType),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WASMOperator {
    Add,
    Sub,
    Mult,
    Div,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WASMType {
    i64,
    i32,
    f64,
    f32
}


pub fn module_to_llr(module: &Node<Module>, context: &Context, cfg_map: &HashMap<Identifier, Cfg>) -> WASMModule {
    for declaration in &module.data.declarations {
        match declaration.data {
            Stmt::FunctionDecStmt{ref name, ref args, ref kwargs, ref block, ref return_type} => {
                let local_variables = declaration.get_true_declarations(context);
                let locals_with_wasm_types: Vec<(String, WASMType)> = local_variables.iter().map(
                    |(n, t)| (n.name.clone(), WASMType::from(t))).collect();
                let cfg = cfg_map.get(name).unwrap();
                let block_llr = cfg.to_llr(context);
                let wasm_args: Vec<(String, WASMType)>  = args.iter().map(|(name, t)| (name.name.clone(), WASMType::from(t))).collect();
                let wasm_func = WASMFunc {
                    name: name.name.clone(),
                    args: wasm_args,
                    locals: locals_with_wasm_types,
                    result: WASMType::from(return_type),
                    code: block_llr
                };
            },
            _ => {}
        }
    }
    panic!()
}

pub trait ToLLR {
    fn to_llr(&self, context: &Context) -> Vec<WASM>;
}

impl ToLLR for Cfg {
    fn to_llr(&self, context: &Context) -> Vec<WASM> {
        let mut wasm = vec!();

        let mut unvisited = vec!(self.entry_index.unwrap());

        while let Some(current_index) = unvisited.pop() {
            let node = self.graph.node_weight(current_index).unwrap();
            wasm.append(&mut node.to_llr(context));
            match node {
                CfgVertex::Block(_) | CfgVertex::Else | CfgVertex::End | CfgVertex::Entry => {
                    let mut n_count = 0;
                    let edges = self.graph.edges_directed(current_index, Outgoing);
                    for edge in edges {
                        unvisited.push(edge.target());
                        n_count += 1;
                        assert_eq!(n_count, 1);
                    }
                },
                CfgVertex::IfStart(_) | CfgVertex::LoopStart(_) => {
                    let mut n_count = 0;
                    let edges = self.graph.edges_directed(current_index, Outgoing);
                    let mut false_block = None;
                    let mut true_block = None;
                    for edge in edges {
                        if edge.weight() == &true {
                            true_block = Some(edge.target());
                        } else {
                            false_block = Some(edge.target());
                        }
                        n_count += 1;
                    }
                    assert_eq!(n_count, 2);

                    match false_block {
                        Some(x) => unvisited.push(x),
                        None => panic!()
                    };

                    match true_block {
                        Some(x) => unvisited.push(x),
                        None => panic!()
                    }
                },
                CfgVertex::Break(_) | CfgVertex::Continue(_) | CfgVertex::Exit => {}
            };
        }
        return wasm;
    }
}

impl ToLLR for CfgVertex {
    fn to_llr(&self, context: &Context) -> Vec<WASM> {
        return match &self {
            CfgVertex::Block(statements) => {
                let mut wasm = vec!();
                for stmt in statements {
                    wasm.append(&mut stmt.to_llr(context));
                }
                wasm
            },
            CfgVertex::IfStart(expression) => {
                let mut wasm = expression.to_llr(context);
                wasm.push(WASM::If);
                wasm
            },
            CfgVertex::LoopStart(expression) => {
                let mut wasm = expression.to_llr(context);
                wasm.push(WASM::Loop);
                wasm
            },
            CfgVertex::Break(block) => {
                let mut wasm = vec!();
                for stmt in block {
                    wasm.append(&mut stmt.to_llr(context));
                }
                wasm.push(WASM::Branch(0));
                wasm
            },
            CfgVertex::Continue(block) => {
                let mut wasm = vec!();
                for stmt in block {
                    wasm.append(&mut stmt.to_llr(context));
                }
                wasm.push(WASM::Branch(1));
                wasm
            },
            CfgVertex::Else => vec!(WASM::Else),
            CfgVertex::End => vec!(WASM::End),
            CfgVertex::Entry | CfgVertex::Exit => vec!(),
        } ;
    }
}

impl ToLLR for Node<CfgStmt> {
    fn to_llr(&self, context: &Context) -> Vec<WASM> {
        return match self.data {
            CfgStmt::Assignment {ref name, ref expression} | CfgStmt::Let {ref name, ref expression} => {
                let mut expr_wasm = expression.to_llr(context);
                expr_wasm.push(WASM::Set(name.name.clone()));
                expr_wasm
            },
            CfgStmt::Return (ref val) | CfgStmt::Yield (ref val) | CfgStmt::Branch (ref val) => {
                val.to_llr(context)
            }
        }
    }
}

impl ToLLR for Node<Expr> {
    fn to_llr(&self, context: &Context) -> Vec<WASM> {
        return match self.data {
            Expr::BinaryExpr{ref left, ref right, ref operator} => {
                let mut llr = left.to_llr(context);
                llr.append(&mut right.to_llr(context));
                let left_id_type = context.get_node_type(left.id);
                let left_wasm_type = WASMType::from(&left_id_type);
                let right_id_type = context.get_node_type(right.id);
                let right_wasm_type = WASMType::from(&right_id_type);
                assert_eq!(left_wasm_type, right_wasm_type);
                llr.push(WASM::Operation(WASMOperator::from(operator), left_wasm_type));
                llr
            },
            Expr::ComparisonExpr{ref left, ref right, ref operator} => {
                let mut llr = left.to_llr(context);
                llr.append(&mut right.to_llr(context));
                let left_id_type = context.get_node_type(left.id);
                let left_wasm_type = WASMType::from(&left_id_type);
                let right_id_type = context.get_node_type(right.id);
                let right_wasm_type = WASMType::from(&right_id_type);
                assert_eq!(left_wasm_type, right_wasm_type);
                // Convert operator to a wasm operator
                llr.push(WASM::Operation(WASMOperator::from(operator), left_wasm_type));
                llr
            },
            Expr::FunctionCall{ref function, ref args, ref kwargs} => {
                let mut llr = vec!();
                for arg in args {
                    llr.append(&mut arg.to_llr(context));
                }
                match function.data {
                    Expr::IdentifierExpr(ref name) => {
                        llr.push(WASM::Call(name.name.clone()));
                    },
                    _ => panic!()
                }
                llr
            },
            Expr::Int(ref value) | Expr::Float(ref value) => {
                let id_type = context.get_node_type(self.id);
                let wasm_type = WASMType::from(&id_type);
                vec!(WASM::Const(wasm_type, value.clone()))
            },
            Expr::Bool(ref value) => {
                return match value {
                    true => vec!(WASM::Const(WASMType::i32, "true".to_string())),
                    false => vec!(WASM::Const(WASMType::i32, "false".to_string()))
                }
            },
            Expr::IdentifierExpr(ref identifier) => vec!(WASM::Get(identifier.name.clone())),
            _ => panic!()
        }
    }
}

impl From<&Type> for WASMType {
    fn from(input: &Type) -> Self {
        match input {
            &Type::i32 => WASMType::i32,
            &Type::i64 => WASMType::i64,
            &Type::f32 => WASMType::f32,
            &Type::f64 => WASMType::f64,
            &Type::boolean => WASMType::i32,
            _ => panic!()
        }
    }
}

impl From<&BinaryOperator> for WASMOperator {
    fn from(input: &BinaryOperator) -> Self {
        return match input {
            BinaryOperator::Add => WASMOperator::Add,
            BinaryOperator::Sub => WASMOperator::Sub,
            BinaryOperator::Mult => WASMOperator::Mult,
            BinaryOperator::Div => WASMOperator::Div,
            _ => panic!()
        };
    }
}

impl From<&ComparisonOperator> for WASMOperator {
    fn from(input: &ComparisonOperator) -> Self {
        return match input {
            ComparisonOperator::Equal => WASMOperator::Eq,
            _ => panic!()
        };
    }
}

#[cfg(test)]
mod tests {
use super::*;

    #[cfg(test)]
    mod exprs {
        use super::*;

        #[test]
        fn test_constants() {
            
        }
    }

}