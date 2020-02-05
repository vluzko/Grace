/// Low-level representation of WebAssembly.
use std::collections::HashMap;
use petgraph::{graph::Neighbors, Outgoing, visit::EdgeRef};
use cfg::{Cfg, CfgVertex, CfgStmt};
use expression::{Node, Module, Expr, BinaryOperator, Identifier};
use scoping::Context;
use typing::Type;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WASM {
    Block,
    Loop,
    End,
    If,
    Else,
    Operation(WASMOperator),
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
    Add(WASMType),
    Sub(WASMType),
    Mult(WASMType),
    Div(WASMType),
    Eq(WASMType),
    Ne(WASMType),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WASMType {
    i64,
    i32,
    f64,
    f32
}

pub struct WASMFunc {
    name: String, 
    args: Vec<(String, WASMType)>, 
    locals: Vec<(String, WASMType)>, 
    result: WASMType, 
    code: Vec<WASM>
}

pub struct WASMModule {
    imports: Vec<String>,
    functions: Vec<WASMFunc>
}

pub fn module_to_llr(module: &Node<Module>, context: &Context, cfg_map: &HashMap<Identifier, Cfg>) -> WASMModule {
    panic!()
}

pub trait ToLLR {
    fn to_llr(&self, context: &Context) -> Vec<WASM>;
}

impl ToLLR for Cfg {
    fn to_llr(&self, context: &Context) -> Vec<WASM> {
        let mut count = 0;
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
            CfgVertex::End => {
                vec!(WASM::End)
            },
            _ => panic!()
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
                llr.append(&mut operator.to_llr(context));
                llr
            },
            Expr::FunctionCall{ref function, ref args, ref kwargs} => {
                let mut wasm = vec!();
                for arg in args {
                    wasm.append(&mut arg.to_llr(context));
                }
                match function.data {
                    Expr::IdentifierExpr(ref name) => {
                        wasm.push(WASM::Call(name.name.clone()));
                    },
                    _ => panic!()
                }
                wasm
            },
            _ => panic!()
        }
    }
}

impl ToLLR for BinaryOperator {
    fn to_llr(&self, context: &Context) -> Vec<WASM> {
        return match self {
            _ => panic!()
        };
    }
}

#[cfg(test)]
mod tests {

}