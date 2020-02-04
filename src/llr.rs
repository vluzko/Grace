/// Low-level representation of WebAssembly.
use cfg::{Cfg, CfgVertex, CfgStmt};
use expression::{Node, Expr, BinaryOperator};
use scoping::Context2;
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

pub trait ToLLR {
    fn to_llr(&self, context: &Context2) -> Vec<WASM>;
}

impl ToLLR for CfgVertex {
    fn to_llr(&self, context: &Context2) -> Vec<WASM> {
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
    fn to_llr(&self, context: &Context2) -> Vec<WASM> {
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
    fn to_llr(&self, context: &Context2) -> Vec<WASM> {
        return match self.data {
            Expr::BinaryExpr{ref left, ref right, ref operator} => {
                let mut llr = left.to_llr(context);
                llr.append(&mut right.to_llr(context));
                llr.append(&mut operator_to_llr(operator));
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

pub fn cfg_to_llr(cfg: &Cfg) -> Vec<WASM> {
    panic!()
}

pub fn vertex_to_llr(vertex: &CfgVertex) -> Vec<WASM> {
    panic!()
}

pub fn operator_to_llr(operator: &BinaryOperator) -> Vec<WASM> {
    panic!()
}

#[cfg(test)]
mod tests {

}