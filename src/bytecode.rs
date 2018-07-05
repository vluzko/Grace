use std::fmt::Display;
use expression::*;
use parser;
use utils::*;


extern crate itertools;


pub trait ASTNode: Display{
    fn generate_bytecode(&self) -> String;
}


/// ASTNode implementations

impl ASTNode for Module {
    fn generate_bytecode(&self) -> String {
        let decls = self.declarations.iter().map(|x| x.generate_bytecode());
        let joined = itertools::join(decls, "\n");
        return format!("(module\n {}\n)", joined).to_string();
    }
}

impl ASTNode for Block {
    fn generate_bytecode(&self) -> String {
        return "".to_string();
    }
}
impl ASTNode for Stmt {
    fn generate_bytecode(&self) -> String {
        let bytecode = match self {
            &Stmt::FunctionDecStmt {ref name, ref args, ref keyword_args, ref vararg, ref varkwarg, ref body, ref return_type} => {
                let body_bytecode = body.generate_bytecode();
                let params = itertools::join(args.iter().map(|x| format!("(param ${} i64)", x.name.to_string())), " ");
                let func_dec = format!("(func ${func_name} {params} (result i64)\n{body})\n(export \"{func_name}\" (func ${func_name}))\n",
                    func_name = name.to_string(),
                    params = params,
                    body = body_bytecode
                );
                func_dec
            },
            &Stmt::AssignmentStmt {ref identifier, ref operator, ref expression} => {
                "".to_string()
            }
            _ => panic!()
        };

        return bytecode;
    }
}
impl ASTNode for Expr {
    fn generate_bytecode(&self) -> String {
        let bytecode_rep = match self {
            &Expr::BinaryExpr {ref operator, ref left, ref right} => {
                let template = operator.generate_bytecode();
                let with_first = template.replace("first", left.generate_bytecode().as_str());
                let full_expr = with_first.replace("second", right.generate_bytecode().as_str());
                full_expr
            },
            &Expr::FunctionCall {ref func_expr, ref args, ref kwargs} => {
                let arg_load = itertools::join(args.iter().map(|x| x.generate_bytecode()), "\n");
                let call = match &**func_expr {
                    &Expr::IdentifierExpr {ref ident} => format!("call ${func_name}", func_name=ident.to_string()),
                    _ => panic!()
                };
                format!("{loads}\n{call}\n", loads=arg_load, call=call)
            },
            &Expr::IdentifierExpr {ref ident} => {
                format!("get_local ${ident}", ident=ident.to_string())
            },
            &Expr::Int(ref int_lit) => int_lit.generate_bytecode(),
            _ => panic!()
        };
        return bytecode_rep;
    }
}

impl BinaryOperator {
    fn generate_bytecode(&self) -> String {
        return match self {
            &BinaryOperator::Add => "first\nsecond\ni64.add\n".to_string(),
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
        return format!("i64.const {}", self.string_rep);
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
    pub fn test_generate_function_call() {
        let function_call = parser::expression("a(b)".as_bytes());
        let bytecode = "get_local $b\ncall $a\n".to_string();
        assert_eq!(output(function_call).generate_bytecode(), bytecode);
    }

    #[test]
    pub fn test_generate_module() {
        let module = parser::module("fn a(b):\n x = 5 + 6\n".as_bytes());
        let mod_bytecode = "(module\n (func $a (param $b i64) (result i64)\n)\n(export \"a\" (func $a))\n\n)".to_string();
        assert_eq!(output(module).generate_bytecode(), mod_bytecode);
    }

    #[test]
    pub fn test_generate_function() {
        let func_dec = parser::statement("fn a(b):\n x = 5 + 6\n".as_bytes(), 0);
        let func_bytecode = "(func $a (param $b i64) (result i64)\n)\n(export \"a\" (func $a))\n".to_string();
        assert_eq!(output(func_dec).generate_bytecode(), func_bytecode);
    }

    #[test]
    pub fn test_generate_add() {
        let add_expr = parser::expression("5 + 6".as_bytes());
        assert_eq!(output(add_expr).generate_bytecode(), "i64.const 5\ni64.const 6\ni64.add\n".to_string());
    }

    #[test]
    pub fn test_integer_literal_generation() {
        let int_lit = IntegerLiteral::from(5);
        assert_eq!(int_lit.generate_bytecode(), "i64.const 5".to_string());
    }
}
