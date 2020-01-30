/// Low-level representation of WebAssembly.
use cfg::{Cfg, CfgVertex, CfgStmt};
use expression::{Node, Expr, BinaryOperator};
use typing::Type;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WASM {
    Block,
    Loop,
    Operation(WASMOperator),
    Const(WASMType, String),
    Call(String),
    Branch,
    BranchIf,
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
    functions: Vec<WASM>
}

#[cfg(test)]
mod tests {

}

pub fn cfg_to_llr(cfg: &Cfg) -> Vec<WASM> {
    panic!()
}

pub fn vertex_to_llr(vertex: &CfgVertex) -> Vec<WASM> {
    panic!()
}


pub fn stmt_to_llr(stmt: &CfgStmt) -> Vec<WASM> {
    return match stmt {
        CfgStmt::Assignment {ref name, ref expression} | CfgStmt::Let {ref name, ref expression} => {
            let mut expr_wasm = expr_to_llr(expression);
            expr_wasm.push(WASM::Set(name.name.clone()));
            expr_wasm
        },
        CfgStmt::Return (ref val) | CfgStmt::Yield (ref val) | CfgStmt::Branch (ref val) => {
            expr_to_llr(val)
        }
    }
}

pub fn expr_to_llr(expr: &Node<Expr>) -> Vec<WASM> {
    return match expr.data {
        Expr::BinaryExpr{ref left, ref right, ref operator} => {
            let mut llr = expr_to_llr(left);
            llr.append(&mut expr_to_llr(right));
            llr.append(&mut operator_to_llr(operator));
            llr
        },
        Expr::FunctionCall{ref function, ref args, ref kwargs} => {
            let wasm = vec!();
            for arg in args {
                wasm.append(&mut expr_to_llr(arg));
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

pub fn operator_to_llr(operator: &BinaryOperator) -> Vec<WASM> {
    panic!()
}