trait AST {
    fn generate_wast(&self) -> &str;
}

enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    And,
    Or,
    Xor,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor
}


//#[test]
//fn test_ast() {
//    let x = AST{children: vec!()};
//}
