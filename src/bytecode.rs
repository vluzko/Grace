use expression::*;

pub trait ASTNode: Display{
    fn generate_bytecode(&self) -> String;
}


/// ASTNode implementations

impl ASTNode for Block {
    fn generate_bytecode(&self) {
        panic!()
    }
}
impl ASTNode for Stmt {
    fn generate_bytecode(&self) {
        panic!()
    }
}
impl ASTNode for Expr {
    fn generate_bytecode(&self) {
        panic!()
    }
}
impl ASTNode for Identifier {
    fn generate_bytecode(&self) {
        panic!()
    }
}
impl ASTNode for IntegerLiteral {
    fn generate_bytecode(&self) {
        panic!()
    }
}
impl ASTNode for FloatLiteral {
    fn generate_bytecode(&self) {
        panic!()
    }
}


