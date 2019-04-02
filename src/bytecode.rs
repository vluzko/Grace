use std::hash::Hash;
use std::collections::HashSet;
extern crate itertools;

use expression::*;
use scoping::*;
use typing::*;

pub trait ToBytecode: Hash  {
    /// Generate WAST bytecode from an AST
    /// CONVENTIONS:
    ///     Generated bytecode should not start or end with a newline, *except* for modules / other
    ///     top level functions. Starting and ending newlines are the responsibility of the parent
    ///     node. We don't have to do it this way, but it's important to have a single convention.
    ///     Nodes *do* need to handle newlines at the start and end of their *children*, obviously.
    fn generate_bytecode(&self) -> String;
}

impl <T> ToBytecode for Node<T> where T: ToBytecode {
    fn generate_bytecode(&self) -> String {
        return self.data.generate_bytecode();
    }
}

impl ToBytecode for Module {
    fn generate_bytecode(&self) -> String {
        let decls = self.declarations.iter().map(|x| x.generate_bytecode());
        let joined = itertools::join(decls, "\n");
        return format!("(module\n\
(import 'memory_management' 'alloc_words' (func $alloc_words (param $a i32) (result i32)))
(import 'memory_management' 'free_chunk' (func $free_chunk (param $a i32) (result i32)))
(import 'memory_management' 'copy_many' (func $copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
(import 'memory_management' 'mem' (memory (;0;) 1))
{}\n)\n", joined).to_string();
    }
}

impl ToBytecode for Block {
    fn generate_bytecode(&self) -> String {
        let statement_bytecode = self.statements.iter().map(
            |x| x.generate_bytecode());
        let bytecode = itertools::join(statement_bytecode, "\n");
        return bytecode;
    }

}

impl ToBytecode for Node<Stmt> {
    fn generate_bytecode(&self) -> String {
        let bytecode = match &self.data {
            &Stmt::FunctionDecStmt {ref name, ref args, ref block, ..} => {
                // Scope check
                // unsafe {
                    let mut local_var_declarations = HashSet::new();
                    for key in self.get_true_declarations() {
                        unsafe {
                            local_var_declarations.insert((**key).to_string());
                        }
                    }
                    // let declarations: HashSet<Identifier> = self.scope.declarations.keys().map(|x| (**x).name).collect();
                
                    let body_bytecode = block.generate_bytecode();
                    let params = itertools::join(args.iter().map(|x| format!("(param ${} i32)", x.name.to_string())), " ");

                    let local_vars = itertools::join(local_var_declarations.iter().map(|x| format!("(local ${} i32)", x)), " ");
                    let func_dec = format!("(func ${func_name} {params} (result i32) {local_vars}\n{body}\n)\n(export \"{func_name}\" (func ${func_name}))",
                        func_name = name.to_string(),
                        params = params,
                        local_vars = local_vars,
                        body = body_bytecode
                    );
                    func_dec
                // }
            },
            &Stmt::AssignmentStmt {ref name, ref operator, ref expression, ..} => {
                match operator {
                    Assignment::Normal => {
                        let identifier_bytecode = name.generate_bytecode();
                        let expression_bytecode = expression.generate_bytecode();
                        let assignment_bytecode = format!("{value}\nset_local ${identifier}",
                        value = expression_bytecode,
                        identifier = identifier_bytecode);
                        assignment_bytecode
                    },
                    _ => panic!()
                }
	        },
	        // Only handles if x {foo}, no elifs or else
	        &Stmt::IfStmt {ref condition, ref block, ref else_block, ..} => {
	            let condition_bytecode = condition.generate_bytecode();
	            let main_block_bytecode = block.generate_bytecode();
	            let else_block_bytecode = match else_block {
	                Some(content) => content.generate_bytecode(),
	                None => "".to_string()
	            };
	            let if_bytecode = format!("{condition_bytecode}\nif (result i32)\n{main_block_bytecode}\nelse\n{else_block_bytecode}\nend",
	            condition_bytecode = condition_bytecode, main_block_bytecode = main_block_bytecode,
	            else_block_bytecode = else_block_bytecode);
	            if_bytecode
	        },
            &Stmt::ReturnStmt (ref value) => {
                value.generate_bytecode()
            },
            &Stmt::LetStmt {ref typed_name, ref expression, ..} => {
	        let identifier_bytecode = typed_name.name.generate_bytecode();
                let expression_bytecode = expression.generate_bytecode();
                let assignment_bytecode = format!("{value}\nset_local ${identifier}",
                value = expression_bytecode,
                identifier = identifier_bytecode);
                assignment_bytecode
            },
            &Stmt::WhileStmt {ref condition, ref block, ..} => {
                let block_bytecode = block.generate_bytecode();
                let condition_bytecode = condition.generate_bytecode();
                let while_bytecode = format!("loop $void\nblock $void1\n{condition}\ni32.eqz\nbr_if 0\n\n{block}\nbr 1\nend\nend\n", condition=condition_bytecode, block=block_bytecode);
                while_bytecode
            },
	        _ => panic!()
        };

        return bytecode;
    }

}

impl ToBytecode for Expr {
    fn generate_bytecode(&self) -> String {
        let bytecode_rep = match self {
            &Expr::ComparisonExpr {ref operator, ref left, ref right, ..} => {
                let first = left.generate_bytecode();
                let second = right.generate_bytecode();
                let operator = operator.generate_bytecode();
                format!("{}\n{}\n{}", first, second, operator)
            },
            &Expr::BinaryExpr {ref operator, ref left, ref right, ..} => {
                let operator_bytecode = operator.generate_typed_bytecode(&operator.get_return_type(&left.get_type(), &right.get_type()));
                let first = left.generate_bytecode();
                let second = right.generate_bytecode();
                format!("{}\n{}\n{}", first, second, operator_bytecode)
            },
            &Expr::UnaryExpr {ref operator, ref operand, ..} => {
                let operator_bytecode = operator.generate_typed_bytecode(&operand.get_type());
                let operand_bytecode = operand.generate_bytecode();
                format!("{}\n{}", operand_bytecode, operator_bytecode)
            },
            &Expr::FunctionCall {ref function, ref args, ..} => {
                let arg_load = itertools::join(args.iter().map(|x| x.generate_bytecode()), "\n");
                let call = match &function.data {
                    &Expr::IdentifierExpr (ref ident) => format!("call ${func_name}", func_name=ident.to_string()),
                    _ => panic!()
                };
                format!("{loads}\n{call}", loads=arg_load, call=call)
            },
            &Expr::IdentifierExpr (ref ident) => {
                format!("get_local ${ident}", ident=ident.to_string())
            },
            &Expr::Int(ref int_lit) => {
                format!("i32.const {}", int_lit)
            },
            &Expr::Float(ref float_lit) => {
                format!("f64.const {}", float_lit)
            },
            &Expr::Bool(ref bool_lit) => {
                match bool_lit {
                    true => "1".to_string(),
                    false => "0".to_string()
                }
            },
            _ => panic!()
        };
        return bytecode_rep;
    }

}


impl ToBytecode for ComparisonOperator {
    fn generate_bytecode(&self) -> String {
        return match self {
            &ComparisonOperator::Equal => "i32.eq".to_string(),
            &ComparisonOperator::Unequal => "i32.ne".to_string(),
            &ComparisonOperator::LessEqual => "i32.le_s".to_string(),
            &ComparisonOperator::Less => "i32.lt_s".to_string(),
            &ComparisonOperator::GreaterEqual => "i32.ge_s".to_string(),
            &ComparisonOperator::Greater => "i32.gt_s".to_string()
        }
    }


}

impl BinaryOperator {
    fn generate_typed_bytecode(&self, return_type: &Type) -> String {
        let type_bytecode = return_type.wast_name();
        let untyped = match self {
            &BinaryOperator::Add => "add",
            &BinaryOperator::Sub => "sub",
            &BinaryOperator::Mult => "mul",
            &BinaryOperator::Div => "div",
            &BinaryOperator::Mod => "rem_u",
            _ => panic!()
        };
        if self.requires_sign() {
            let sign = return_type.sign();
            format!("{}.{}{}", type_bytecode, untyped, sign)
        } else {
            format!("{}.{}", type_bytecode, untyped)
        }

    }
}

impl UnaryOperator {
    fn generate_typed_bytecode(&self, operand_type: &Type) -> String {
        match (&self, operand_type) {
            (&UnaryOperator::ToI64, &Type::i32) => "i64.extend_s".to_string(),
            (&UnaryOperator::ToF64, &Type::i32) => "f64.convert_s".to_string(),
            (&UnaryOperator::ToF64, &Type::f32) => "f64.promote".to_string(),
            (&UnaryOperator::ToI64, &Type::i64) => "".to_string(),
            (&UnaryOperator::ToF64, &Type::f64) => "".to_string(),
            _ => panic!()
        }
    }
}

impl ToBytecode for Identifier {
    fn generate_bytecode(&self) -> String {
        let name = self.name.clone();
	    return name;
    }

}

#[cfg(test)]
mod tests {

    use super::*;
    use parser;
    use parser_utils::*;
    use typing::Typed;

    #[test]
    pub fn test_generate_function_call() {
        let function_call = parser::expression("a(b)".as_bytes());
        let bytecode = "get_local $b\ncall $a".to_string();
        assert_eq!(output(function_call).generate_bytecode(), bytecode);
    }

    #[test]
   pub fn test_generate_module() {
       let module = output(parser::module("fn a(b):\n let x = 5 + 6\n return x\n".as_bytes())).gen_scopes(&empty_scope());
       let mod_bytecode = r#"(module
(import 'memory_management' 'alloc_words' (func $alloc_words (param $a i32) (result i32)))
(import 'memory_management' 'free_chunk' (func $free_chunk (param $a i32) (result i32)))
(import 'memory_management' 'copy_many' (func $copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
(import 'memory_management' 'mem' (memory (;0;) 1))
(func $a (param $b i32) (result i32) (local $x i32)
i32.const 5
i32.const 6
i32.add
set_local $x
get_local $x
)
(export "a" (func $a))
)
"#;
       assert_eq!(module.generate_bytecode(), mod_bytecode);
   }

    #[test]
    pub fn test_generate_function() {
        let func_dec = output(parser::statement("fn a(b):\n let x = 5 + 6\n return x\n".as_bytes(), 0)).gen_scopes(&empty_scope());
        let func_bytecode = r#"(func $a (param $b i32) (result i32) (local $x i32)
i32.const 5
i32.const 6
i32.add
set_local $x
get_local $x
)
(export "a" (func $a))"#;
        assert_eq!(func_dec.generate_bytecode(), func_bytecode);
    }

    #[test]
    pub fn test_generate_add() {
        let add_expr = parser::expression("5 + 6".as_bytes());
        assert_eq!(output(add_expr).generate_bytecode(), "i32.const 5\ni32.const 6\ni32.add".to_string());
        let add_expr = parser::expression("5.0 + 6".as_bytes());
        assert_eq!(output(add_expr).generate_bytecode(), "f64.const 5.0\ni32.const 6\nf64.convert_s\nf64.add".to_string());
        let add_expr = parser::expression("5.0 + 6.0".as_bytes());
        assert_eq!(output(add_expr).generate_bytecode(), "f64.const 5.0\nf64.const 6.0\nf64.add".to_string());
    }

    #[test]
    pub fn test_generate_subtract() {
        let sub_expr = parser::expression("1 - 2".as_bytes());
        assert_eq!(output(sub_expr).generate_bytecode(), "i32.const 1\ni32.const 2\ni32.sub".to_string());
    }

    #[test]
    pub fn test_generate_multiply() {
        let mult_expr = parser::expression("3 * 4".as_bytes());
        assert_eq!(output(mult_expr).generate_bytecode(), "i32.const 3\ni32.const 4\ni32.mul".to_string());
    }

    #[test]
    pub fn test_generate_divide() {
        let div_expr = output( parser::expression("8 / 9".as_bytes())).type_based_rewrite();
        assert_eq!(div_expr.generate_bytecode(), "i32.const 8\nf64.convert_s\ni32.const 9\nf64.convert_s\nf64.div".to_string());
    }

    #[test]
    pub fn test_generate_rem() {
        let rem_expr = parser::expression("5 % 6".as_bytes());
        assert_eq!(output(rem_expr).generate_bytecode(), "i32.const 5\ni32.const 6\ni32.rem_u".to_string());
    }

    #[test]
    pub fn test_assignment_generation() {
        let assignment_stmt = parser::assignment_stmt("foo = 3".as_bytes());
        assert_eq!(output(assignment_stmt).generate_bytecode(), "i32.const 3\nset_local $foo".to_string());
    }
}
