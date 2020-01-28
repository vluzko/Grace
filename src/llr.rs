/// Low-level representation of WebAssembly.


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WASM {
    Import,
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

#[cfg(test)]
mod tests {

}