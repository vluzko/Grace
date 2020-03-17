/// Low-level representation of WebAssembly.
use itertools::join;
use std::collections::HashMap;
use std::convert::From;
use std::fmt;

use petgraph::{graph::Neighbors, Outgoing, visit::EdgeRef};


use cfg::{Cfg, CfgVertex, CfgStmt};
use expression::{Node, Module, Expr, Stmt, BinaryOperator, ComparisonOperator, Identifier};
use scoping::{Context, GetContext};
use typing::Type;

/// A representation of a WASM module.
/// Includes function declarations, imports, and memory declarations.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct WASMModule {
    pub imports: Vec<WASMImport>,
    pub functions: Vec<WASMFunc>
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct WASMImport {
    pub path: String,
    pub value: String,
    pub internal_name: String,
    pub params: Vec<(String, WASMType)>,
    pub return_type: WASMType,

}

/// A WASM function declaration
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct WASMFunc {
    pub name: String, 
    pub args: Vec<(String, WASMType)>, 
    pub locals: Vec<(String, WASMType)>, 
    pub result: WASMType, 
    pub code: Vec<WASM>
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WASM {
    Block,
    Loop,
    End,
    If,
    Else,
    Branch(usize),
    BranchIf(usize),
    Operation(WASMOperator, WASMType),
    Const(WASMType, String),
    Call(String),
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
    let mut functions = vec!();
    let mut imports = vec!();

    for import in &module.data.imports {
        let typing_info = context.get_node_type(import.id);
        for value in &import.values {
            //get params and return type
            let (wasm_args, wasm_return) = match typing_info.resolve_attribute(value) {
                // convert everything to WASMTypes
                Type::Function(ref args, ref return_type) => {
                    let wasm_args = args.iter().map( |(n, t) | (n.name.clone(), WASMType::from(t))).collect();
                    let wasm_return = WASMType::from(&**return_type); 
                    (wasm_args, wasm_return)
                },
                Type::Named(name) => {
                    let full_name = format!("{}.{}", import.string_ref(), name);
                    let actual_type = context.get_defined_type(&Identifier::from(full_name));
                    let (args, return_type) = actual_type.get_constructor_type();
                    let wasm_args = args.iter().map( |(n, t) | (n.name.clone(), WASMType::from(t))).collect();
                    let wasm_return = WASMType::from(&return_type); 
                    (wasm_args, wasm_return)
                },
                x => panic!("Wrong import type: {:?}", x)
            };
            let joined_path = join(import.path.iter().map(|x| x.name.clone()), ".");
            let internal_name = format!(".{}.{}", joined_path, value);
            let wasm_import = WASMImport {
                path: joined_path,
                value: value.name.clone(),
                internal_name: internal_name,
                params: wasm_args,
                return_type: wasm_return
            };
            imports.push(wasm_import);
        }
    }

    for declaration in &module.data.declarations {
        match declaration.data {
            Stmt::FunctionDecStmt{ref name, ref args, ref kwargs, ref return_type, ..} => {
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
                functions.push(wasm_func)
            },
            _ => {}
        }
    }
    return WASMModule {
        imports: imports,
        functions: functions
    };
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
        return match &self.data {
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
            Expr::FunctionCall{ref function, ref args, ..} => {
                let mut llr = vec!();
                for arg in args {
                    llr.append(&mut arg.to_llr(context));
                }
                match &function.data {
                    Expr::IdentifierExpr(ref name) => {
                        llr.push(WASM::Call(name.name.clone()));
                    },
                    Expr::ModuleAccess(ref id, ref path) => {
                        let func_name = join(path.iter().map(|x| x.name.clone()), ".");
                        llr.push(WASM::Call(format!(".{}", func_name.clone())));
                    },
                    x => panic!("FunctionCall to_llr not implemented for :{:?}", x)
                }
                llr
            },
            Expr::StructLiteral{ref base, ref fields} => {
                let mut llr = vec!();
                for field in fields {
                    llr.append(&mut field.to_llr(context));
                }
                llr.append(&mut base.to_llr(context));
                llr
            },
            Expr::ModuleAccess(ref id, ref fields) => {
                panic!()
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
            x => panic!("to_llr not implemented for: {:?}", x)
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
            &Type::Sum(..) => WASMType::i32,
            &Type::Named(..) => WASMType::i32,
            x => panic!("Tried to convert {:?} to WASM", x)
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

impl fmt::Display for WASMType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            WASMType::i32 => "i32",
            WASMType::i64 => "i64",
            WASMType::f32 => "f32",
            WASMType::f64 => "f64"
        })
    }
}

impl fmt::Display for WASMOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            WASMOperator::Add => "add",
            WASMOperator::Sub => "sub",
            WASMOperator::Mult => "mul",
            WASMOperator::Div => "div",
            WASMOperator::Eq => "eq",
            WASMOperator::Ne => "ne",
            x => panic!("Display not implemented for WASMOperator: {:?}", x)
        })
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