use std::hash::Hash;
use std::collections::HashMap;
extern crate itertools;

use expression::*;
use scoping::*;
use typing;
use typing::Type;

pub trait ToBytecode: Hash  {
    /// Generate WAST bytecode from an AST
    /// CONVENTIONS:
    ///     Generated bytecode should not start or end with a newline, *except* for modules / other
    ///     top level functions. Starting and ending newlines are the responsibility of the parent
    ///     node. We don't have to do it this way, but it's important to have a single convention.
    ///     Nodes *do* need to handle newlines at the start and end of their *children*, obviously.
    fn generate_bytecode(&self, context: &Context, type_map: &mut HashMap<usize, typing::Type>) -> String;
}

// impl <T> ToBytecode for Node<T> where T: ToBytecode {
    // fn generate_bytecode(&self, context: &Context, type_map: &mut HashMap<usize, typing::Type>) -> String {
        // return self.data.generate_bytecode(context, type_map);
    // }
// }

impl ToBytecode for Node<Module> {
    fn generate_bytecode(&self, context: &Context, type_map: &mut HashMap<usize, typing::Type>) -> String {
        let decls = self.data.declarations.iter().map(|x| x.generate_bytecode(context, type_map));
        let joined = itertools::join(decls, "\n");
        return format!("(module\n\
(import \"memory_management\" \"alloc_words\" (func $alloc_words (param $a i32) (result i32)))
(import \"memory_management\" \"free_chunk\" (func $free_chunk (param $a i32) (result i32)))
(import \"memory_management\" \"copy_many\" (func $copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
(import \"memory_management\" \"mem\" (memory (;0;) 1))
{}\n)\n", joined).to_string();
    }
}

impl ToBytecode for Node<Block> {
    fn generate_bytecode(&self, context: &Context, type_map: &mut HashMap<usize, typing::Type>) -> String {
        let statement_bytecode = self.data.statements.iter().map(
            |x| x.generate_bytecode(context, type_map));
        let bytecode = itertools::join(statement_bytecode, "\n");
        return bytecode;
    }

}

impl ToBytecode for Node<Stmt> {
    fn generate_bytecode(&self, context: &Context, type_map: &mut HashMap<usize, typing::Type>) -> String {
        let bytecode = match &self.data {
            &Stmt::FunctionDecStmt {ref name, ref args, ref block, ..} => {

                let (_, ret) = match type_map.get(&self.id).unwrap() {
                    typing::Type::Function(x, y) => (x.clone(), y.clone()),
                    x => panic!("Wrong type for function {:?}.\nShould be a function type, found: {:?}", self, x)
                };

                
            
                let body_bytecode = block.generate_bytecode(context, type_map);
                
                let params = itertools::join(args.iter().map(|x| format!("(param ${} {})", x.0.to_string(), x.1.wast_name())), " ");

                let return_bytecode = match *ret {
                    Type::empty => "".to_string(),
                    x => format!("(result {})", x.wast_name())
                };

                // Handle local variables. They must be declared in the header.
                let mut local_var_declarations: Vec<String> = vec!();
                for name in self.get_true_declarations(context) {
                    let local_var_type = context.get_type(block.scope, &name, type_map);
                    local_var_declarations.push(format!("(local ${} {})", name.to_string(), local_var_type.wast_name()));
                }
                let local_vars = itertools::join(local_var_declarations.iter(), " ");


                let func_dec = format!("(func ${func_name} {params} {return_type} {local_vars}\n{body}\n)\n(export \"{func_name}\" (func ${func_name}))",
                    func_name = name.to_string(),
                    params = params,
                    local_vars = local_vars,
                    body = body_bytecode,
                    return_type = return_bytecode
                );
                func_dec
            },
            &Stmt::AssignmentStmt {ref name, ref operator, ref expression, ..} => {
                match operator {
                    Assignment::Normal => {
                        let identifier_bytecode = name.generate_bytecode(context, type_map);
                        let expression_bytecode = expression.generate_bytecode(context, type_map);
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
	            let condition_bytecode = condition.generate_bytecode(context, type_map);
	            let main_block_bytecode = block.generate_bytecode(context, type_map);
	            let else_block_bytecode = match else_block {
	                Some(content) => content.generate_bytecode(context, type_map),
	                None => "".to_string()
	            };
	            let if_bytecode = format!("{condition_bytecode}\nif (result i32)\n{main_block_bytecode}\nelse\n{else_block_bytecode}\nend",
	            condition_bytecode = condition_bytecode, main_block_bytecode = main_block_bytecode,
	            else_block_bytecode = else_block_bytecode);
	            if_bytecode
	        },
            &Stmt::ReturnStmt (ref value) => {
                value.generate_bytecode(context, type_map)
            },
            &Stmt::LetStmt {ref typed_name, ref expression, ..} => {
	        let identifier_bytecode = typed_name.name.generate_bytecode(context, type_map);
                let expression_bytecode = expression.generate_bytecode(context, type_map);
                let assignment_bytecode = format!("{value}\nset_local ${identifier}",
                value = expression_bytecode,
                identifier = identifier_bytecode);
                assignment_bytecode
            },
            &Stmt::WhileStmt {ref condition, ref block, ..} => {
                let block_bytecode = block.generate_bytecode(context, type_map);
                let condition_bytecode = condition.generate_bytecode(context, type_map);
                let while_bytecode = format!("loop $void\nblock $void1\n{condition}\ni32.eqz\nbr_if 0\n\n{block}\nbr 1\nend\nend\n", condition=condition_bytecode, block=block_bytecode);
                while_bytecode
            },
	        _ => panic!()
        };

        return bytecode;
    }

}

impl ToBytecode for Node<Expr> {
    fn generate_bytecode(&self, context: &Context, type_map: &mut HashMap<usize, typing::Type>) -> String {
        let bytecode_rep = match &self.data {
            &Expr::ComparisonExpr {ref operator, ref left, ref right, ..} => {
                let first = left.generate_bytecode(context, type_map);
                let second = right.generate_bytecode(context, type_map);
                let operator = operator.generate_bytecode(context, type_map);
                format!("{}\n{}\n{}", first, second, operator)
            },
            &Expr::BinaryExpr {ref operator, ref left, ref right, ..} => {
                let return_type = typing::choose_return_type(type_map.get(&self.id).unwrap());
                // TODO: This should probably be handled in type rewrites?
                let operator_bytecode = operator.generate_typed_bytecode(&return_type);
                let first = left.generate_bytecode(context, type_map);
                let second = right.generate_bytecode(context, type_map);
                format!("{}\n{}\n{}", first, second, operator_bytecode)
            },
            &Expr::UnaryExpr {ref operator, ref operand, ..} => {
                let operand_type = typing::choose_return_type(&type_map.get(&operand.id).unwrap().clone());
                let operator_bytecode = operator.generate_typed_bytecode(&operand_type);
                let operand_bytecode = operand.generate_bytecode(context, type_map);
                format!("{}\n{}", operand_bytecode, operator_bytecode)
            },
            &Expr::FunctionCall {ref function, ref args, ..} => {
                let arg_load = itertools::join(args.iter().map(|x| x.generate_bytecode(context, type_map)), "\n");
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
                let t = type_map.get(&self.id).unwrap();
                format!("{}.const {}", t.wast_name(), int_lit)
            },
            &Expr::Float(ref float_lit) => {
                let t = type_map.get(&self.id).unwrap();
                format!("{}.const {}", t.wast_name(), float_lit)
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
    fn generate_bytecode(&self, _context: &Context, _type_map: &mut HashMap<usize, typing::Type>) -> String {
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
    fn generate_typed_bytecode(&self, return_type: &typing::Type) -> String {
        let type_bytecode = return_type.wast_name();
        let untyped = match self {
            &BinaryOperator::Add => "add",
            &BinaryOperator::Sub => "sub",
            &BinaryOperator::Mult => "mul",
            &BinaryOperator::Div => "div",
            &BinaryOperator::Mod => "rem_u",
            &BinaryOperator::And => "and",
            &BinaryOperator::Or => "or",
            &BinaryOperator::Xor => "xor",
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
    fn generate_typed_bytecode(&self, operand_type: &typing::Type) -> String {
        match (&self, operand_type) {
            (&UnaryOperator::ToI64, &typing::Type::i32) => "i64.extend_s".to_string(),
            (&UnaryOperator::ToF64, &typing::Type::i32) => "f64.convert_s/i32".to_string(),
            (&UnaryOperator::ToF64, &typing::Type::f32) => "f64.promote/f32".to_string(),
            (&UnaryOperator::ToI64, &typing::Type::i64) => "".to_string(),
            (&UnaryOperator::ToF64, &typing::Type::f64) => "".to_string(),
            (&UnaryOperator::ToF32, &typing::Type::i32) => "f32.convert_s/i32".to_string(),
            (&UnaryOperator::ToF32, &typing::Type::f32) => "".to_string(),
            (&UnaryOperator::ToBool, &typing::Type::i32) => "".to_string(),
            _ => {
                println!("Unary operator not implemented: {:?}, {:?}", self, operand_type);
                panic!()
            }
        }
    }
}

impl ToBytecode for Identifier {
    fn generate_bytecode(&self, _context: &Context, _type_map: &mut HashMap<usize, typing::Type>) -> String {
        let name = self.name.clone();
	    return name;
    }

}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    pub fn test_generate_function_call() {
        let (function_call, context, mut type_map) = compiler_layers::to_type_rewrites::<Node<Block>>(
            "fn a(b:i32) -> i32:\n return 1\nlet x = a(1)".as_bytes());
        let bytecode = "(func $a (param $b i32) (result i32) \ni32.const 1\n)\n(export \"a\" (func $a))\ni32.const 1\ncall $a\nset_local $x".to_string();
        assert_eq!(function_call.generate_bytecode(&context, &mut type_map), bytecode);
    }

    #[test]
   pub fn test_generate_module() {
       let (module, context, mut type_map) = 
       compiler_layers::to_type_rewrites::<Node<Module>>("fn a(b: i32) -> i32:\n let x = 5 + 6\n return x\n".as_bytes());
       let mod_bytecode = r#"(module
(import "memory_management" "alloc_words" (func $alloc_words (param $a i32) (result i32)))
(import "memory_management" "free_chunk" (func $free_chunk (param $a i32) (result i32)))
(import "memory_management" "copy_many" (func $copy_many (param $a i32) (param $b i32) (param $size i32) (result i32)))
(import "memory_management" "mem" (memory (;0;) 1))
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
       assert_eq!(module.generate_bytecode(&context, &mut type_map), mod_bytecode);
   }

    #[test]
    pub fn test_generate_function() {
        let (func_stmt, context, mut type_map) = 
        compiler_layers::to_type_rewrites::<Node<Stmt>>("fn a(b: i32) -> i32:\n let x = 5 + 6\n return x\n".as_bytes());
        let bytecode = func_stmt.generate_bytecode(&context, &mut type_map);
        assert_eq!(bytecode, r#"(func $a (param $b i32) (result i32) (local $x i32)
i32.const 5
i32.const 6
i32.add
set_local $x
get_local $x
)
(export "a" (func $a))"#);
    }

    #[test]
    pub fn test_generate_add() {
        let (add_expr, context, mut type_map) = compiler_layers::to_type_rewrites::<Node<Expr>>("5 + 6".as_bytes());
        let bytecode = add_expr.generate_bytecode(&context, &mut type_map);
        assert_eq!(bytecode, "i32.const 5\ni32.const 6\ni32.add".to_string());
        let (add_expr, context, mut type_map) = compiler_layers::to_type_rewrites::<Node<Expr>>("5.0 + 6".as_bytes());
        let bytecode = add_expr.generate_bytecode(&context, &mut type_map);
        assert_eq!(bytecode, "f32.const 5.0\nf64.promote/f32\ni32.const 6\nf64.convert_s/i32\nf64.add".to_string());
        let (add_expr, context, mut type_map) = compiler_layers::to_type_rewrites::<Node<Expr>>("5 + 6.0".as_bytes());
        let bytecode = add_expr.generate_bytecode(&context, &mut type_map);
        assert_eq!(bytecode, "i32.const 5\nf64.convert_s/i32\nf32.const 6.0\nf64.promote/f32\nf64.add".to_string());
        let (add_expr, context, mut type_map) = compiler_layers::to_type_rewrites::<Node<Expr>>("5.0 + 6.0".as_bytes());
        let bytecode = add_expr.generate_bytecode(&context, &mut type_map);
        assert_eq!(bytecode, "f32.const 5.0\nf32.const 6.0\nf32.add".to_string());
    }

    #[test]
    pub fn test_generate_subtract() {
        let (sub_expr, context, mut type_map) = compiler_layers::to_type_rewrites::<Node<Expr>>("1 - 2".as_bytes());
        let bytecode = sub_expr.generate_bytecode(&context, &mut type_map);
        assert_eq!(bytecode, "i32.const 1\ni32.const 2\ni32.sub".to_string());
    }

    #[test]
    pub fn test_generate_multiply() {
        let (mult_expr, context, mut type_map) = compiler_layers::to_type_rewrites::<Node<Expr>>("3 * 4".as_bytes());
        let bytecode = mult_expr.generate_bytecode(&context, &mut type_map);
        assert_eq!(bytecode, "i32.const 3\ni32.const 4\ni32.mul".to_string());
    }

    #[test]
    pub fn test_generate_divide() {
        let (div_expr, context, mut type_map) = compiler_layers::to_type_rewrites::<Node<Expr>>("8 / 9".as_bytes());
        let bytecode = div_expr.generate_bytecode(&context, &mut type_map);
        assert_eq!(bytecode, "i32.const 8\nf64.convert_s/i32\ni32.const 9\nf64.convert_s/i32\nf64.div".to_string());
    }

    #[test]
    pub fn test_generate_rem() {
        let (rem_expr, context, mut type_map) = compiler_layers::to_type_rewrites::<Node<Expr>>("5 % 6".as_bytes());
        let bytecode = rem_expr.generate_bytecode(&context, &mut type_map);
        assert_eq!(bytecode, "i32.const 5\ni32.const 6\ni32.rem_u".to_string());
    }

    #[test]
    pub fn test_generate_assignment() {
        // let assignment_stmt = parser::assignment_stmt("foo = 3".as_bytes());
        let (stmt, context, mut type_map) = compiler_layers::to_type_rewrites::<Node<Stmt>>("foo = 3".as_bytes());
        let bytecode = stmt.generate_bytecode(&context, &mut type_map);
        assert_eq!(bytecode, "i32.const 3\nset_local $foo".to_string());
    }
}
