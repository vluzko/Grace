use std::fmt::Display;
use expression::*;
use utils::*;
use parser;

pub trait ASTNode: Display{
    fn generate_bytecode(&self) -> String;
}


/// ASTNode implementations

impl ASTNode for Block {
    fn generate_bytecode(&self) -> String {
        panic!()
    }
}
impl ASTNode for Stmt {
    fn generate_bytecode(&self) -> String {
        panic!()
    }
}
impl ASTNode for Expr {
    fn generate_bytecode(&self) -> String {
        let bytecode_rep = match self {
            &Expr::Int(ref int_lit) => int_lit.generate_bytecode(),
            &Expr::BinaryExpr {ref operator, ref left, ref right} => {
                let template = operator.generate_bytecode();
                let with_first = template.replace("first", left.generate_bytecode().as_str());
                let full_expr = with_first.replace("second", right.generate_bytecode().as_str());
                return full_expr;
            },
            _ => panic!()
        };
        return bytecode_rep;
    }
}

impl BinaryOperator {
    fn generate_bytecode(&self) -> String {
        return match self {
            &BinaryOperator::Add => "first\nsecond\ni32.add\n".to_string(),
            _ => panic!()
        };
    }
}

impl ASTNode for Identifier {
    fn generate_bytecode(&self) -> String {
        panic!()
    }
}
impl ASTNode for IntegerLiteral {
    fn generate_bytecode(&self) -> String {
        return format!("i32.const {}", self.string_rep);
    }
}
impl ASTNode for FloatLiteral {
    fn generate_bytecode(&self) -> String {
        panic!()
    }
}

pub fn node_to_file(file_name: String, node: Box<ASTNode>) {

}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    pub fn test_generate_add() {
        let add_expr = parser::expression("5 + 6".as_bytes());
        assert_eq!(output(add_expr).generate_bytecode(), "i32.const 5\ni32.const 6\ni32.add\n".to_string());
    }

    #[test]
    pub fn test_integer_literal_generation() {
        let int_lit = IntegerLiteral::from(5);
        assert_eq!(int_lit.generate_bytecode(), "i32.const 5".to_string());
    }
}
