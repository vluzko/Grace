use std::convert::From;

use expression::*;
use type_checking::types::Type;
use type_checking::scope::{get_convert_expr, choose_return_type};
use type_checking::context::Context;

pub trait TypeRewritable<T> {
    fn type_based_rewrite(self, context: &mut Context) -> T;
}

impl TypeRewritable<Node<Module>> for Node<Module> {
    fn type_based_rewrite(self, context: &mut Context) -> Node<Module> {
        let vec_stmt_rewrite = |x: Box<Node<Stmt>>| Box::new(x.type_based_rewrite(context));

        let new_func_decs: Vec<Box<Node<Stmt>>> = self.data.functions.into_iter().map(Box::new(vec_stmt_rewrite)).collect();
        let vec_stmt_rewrite_2= |x: Box<Node<Stmt>>| Box::new(x.type_based_rewrite(context));
        let new_struct_decs: Vec<Box<Node<Stmt>>> = self.data.structs.into_iter().map(Box::new(vec_stmt_rewrite_2)).collect();

        let mut new_impls = vec!();
        for (trait_name, type_name, functions) in self.data.trait_implementations.into_iter() {
            let rewritten_functions: Vec<Node<Stmt>> = functions.into_iter().map(|x| x.type_based_rewrite(context)).collect();
            new_impls.push((trait_name, type_name, rewritten_functions));
        }

        return Node{
            id: self.id,
            data: Module{functions: new_func_decs, structs: new_struct_decs, imports: self.data.imports, traits: self.data.traits, trait_implementations: new_impls},
            scope: self.scope
        };
    }
}

impl TypeRewritable<Node<Block>> for Node<Block> {
    fn type_based_rewrite(self, context: &mut Context) -> Node<Block> {
        let new_stmts = self.data.statements.into_iter().map(|x| Box::new(x.type_based_rewrite(context))).collect();

        let new_block = Node{
            id: self.id,
            data: Block{statements: new_stmts},
            scope: self.scope
        };

        for stmt in &new_block.data.statements {
            match &stmt.data {
                Stmt::FunctionDecStmt{ref name, ..} | Stmt::LetStmt{ref name, ..} | Stmt::StructDec{ref name, ..}  => {
                    context.append_declaration(self.scope, name, &stmt);
                },
                _ => {}
            };
        }
        return new_block;
    }
}

impl TypeRewritable<Node<Stmt>> for Node<Stmt> {
    fn type_based_rewrite(self, context: &mut Context) -> Node<Stmt> {
        let new_stmt = match self.data {
            Stmt::LetStmt {name, type_annotation, expression} => {
                let returned_type = context.get_node_type(expression.id);
                let base = expression.type_based_rewrite(context);
                let expr = match &type_annotation {
                    Some(x) => get_convert_expr(&returned_type, &x, base, context),
                    None => base
                };
                Stmt::LetStmt {name, type_annotation, expression: expr}
            },
            Stmt::AssignmentStmt {expression, name} => {
                let expected_type = context.get_node_type(self.id);
                let actual_type = context.get_node_type(expression.id);
                let base = expression.type_based_rewrite(context);
                let expr = get_convert_expr(&actual_type, &expected_type, base, context);
                Stmt::AssignmentStmt {name, expression: expr}
            },
            Stmt::FunctionDecStmt {name, block, args, kwargs, return_type} => {
                Stmt::FunctionDecStmt {block: block.type_based_rewrite(context), name, args, kwargs, return_type}
            },
            Stmt::IfStmt {condition, block, else_block} => {
                let new_else_block = match else_block {
                    None => None,
                    Some(block) => Some(block.type_based_rewrite(context))
                };
                Stmt::IfStmt {
                    condition: condition.type_based_rewrite(context), block: block.type_based_rewrite(context), else_block: new_else_block
                }
            },
            Stmt::WhileStmt {condition, block} => {
                Stmt::WhileStmt {condition: condition.type_based_rewrite(context), block: block.type_based_rewrite(context)}
            },
            Stmt::ReturnStmt (value) => {
                let exp_type = context.get_type(self.scope, &Identifier::from("$ret"));
                let ret_type = context.get_node_type(value.id);
                let base = value.type_based_rewrite(context);
                assert!(ret_type.is_compatible(&exp_type));

                let expr = match ret_type == exp_type {
                    true => base,
                    false => get_convert_expr(&ret_type, &exp_type, base, context)
                };
                Stmt::ReturnStmt(expr)
            },
            _ => self.data
        };
        return Node {
            id: self.id, 
            data: new_stmt,
            scope: self.scope 
        };
    }
}

impl TypeRewritable<Node<Expr>> for Node<Expr> {
    fn type_based_rewrite(self, context: &mut Context) -> Node<Expr> {
        let new_expr = match self.data {
            Expr::ComparisonExpr {left, right, operator} => {
                let left = Box::new(left.type_based_rewrite(context));
                let right = Box::new(right.type_based_rewrite(context));
                Expr::ComparisonExpr {left, right, operator}
            },
            Expr::BinaryExpr {operator, left, right} => {
                let left_type = context.get_node_type(left.id);
                let right_type = context.get_node_type(right.id);
                let new_left = left.type_based_rewrite(context);
                let new_right = right.type_based_rewrite(context);

                // If neither type is gradual, proceed as normal
                if !left_type.is_gradual() && !right_type.is_gradual() {
                    // If the left type is not primitive, call trait method corresponding to this operator
                    if !left_type.is_primitive() {
                        panic!("Operator -> trait method call not implemented yet.")
                    } else {
                        // If the left type is primitive, it stays an operator
                        let merged_type = operator.get_return_types(&left_type, &right_type);
                        let converted_left = get_convert_expr(&left_type, &merged_type, new_left, context);
                        let converted_right = get_convert_expr(&right_type, &merged_type, new_right, context);
                        Expr::BinaryExpr{operator, left: Box::new(converted_left), right: Box::new(converted_right)}
                    }
                } else {
                    // If either is gradual, we have to call the gradual version of the operator. 
                    let func_name_expr = wrap(Expr::from(".gradual_binary_ops.call_gradual"));

                    // Create the function table pointer.
                    let op_ptr = operator.gradual_index();
                    let op_ptr_expr = Node::from(Expr::from(op_ptr));
                    context.add_type(op_ptr_expr.id, Type::i32);

                    // Set up the function call
                    let args = vec!(op_ptr_expr, new_left, new_right);
                    let kwargs = vec!();
                    let func_call = Expr::FunctionCall{function: func_name_expr, args, kwargs};
                    func_call
                }
            },
            Expr::FunctionCall {function, args, kwargs} => {
                let new_func_expr = Box::new(function.type_based_rewrite(context));
                let new_args = args.into_iter().map(|x| x.type_based_rewrite(context)).collect();
                let new_kwargs = kwargs.into_iter().map(|x| (x.0, x.1.type_based_rewrite(context))).collect();
                Expr::FunctionCall {function: new_func_expr, args: new_args, kwargs: new_kwargs}
            },
            Expr::AttributeAccess {base, attribute} => {
                let rewritten_base = base.type_based_rewrite(context);

                let base_type = context.get_node_type(rewritten_base.id);
                let trait_info = context.trait_information(&base_type, &attribute);

                match trait_info {
                    Some(trait_name) => Expr::TraitAccess {
                        base: Box::new(rewritten_base),
                        trait_name: trait_name,
                        attribute: attribute
                    },
                    None => Expr::AttributeAccess{base: Box::new(rewritten_base), attribute: attribute}
                }
            },
            Expr::Int(_) | Expr::Float(_) => {
                let current_type = context.get_node_type(self.id);
                context.add_type(self.id, choose_return_type(&current_type));
                self.data
            },
            _ => self.data
        };

        return Node {
            id: self.id,
            data: new_expr,
            scope: self.scope
        };
    }
}


#[cfg(test)]
mod test {

    use super::*;
    use compiler_layers;

    #[cfg(test)]
    mod types {

        use super::*;

        #[test]
        fn test_flatten() {
            let idents = vec!(Identifier::from("a"), Identifier::from("b"));
            let bottom_map = btreemap!{
                Identifier::from("c") => Type::Function(vec!((Identifier::from("x"), Type::i32)), Box::new(Type::i64)),
            };
            let record_type = Type::flatten_to_record(&idents, bottom_map.clone());
            let second_map = btreemap!{
                Identifier::from("b") => Type::Record(vec!(Identifier::from("c")), bottom_map),
            };
            assert_eq!(record_type, Type::Record(vec!(Identifier::from("b")), second_map));
        }
    }

    #[cfg(test)]
    mod statements {
        use super::*;
        use compiler_layers;
        #[test]
        fn if_stmt_typing() {
            let (fun, context) = compiler_layers::to_context::<Node<Stmt>>(if_stmt_fixture());
            match fun.data {
                Stmt::FunctionDecStmt{ref block, ..} => {
                    let if_stmt = block.data.statements.get(2).unwrap();
                    assert_eq!(context.type_map.get(&if_stmt.id).unwrap(), &Type::i32);
                },
                _ => panic!()
            };
            
        }
    }

    #[test]
    fn test_identifier_resolution() {
        let block_str = "let a = 1\nlet b = a";
        let (parsed, context) = compiler_layers::to_context::<Node<Block>>(block_str.as_bytes());
        assert_eq!(context.get_node_type(parsed.id), Type::empty);
        let id2 = parsed.data.statements[1].id;
        assert_eq!(context.get_node_type(id2), Type::i32);
    }

    fn if_stmt_fixture<'a>() -> &'a [u8] {
        let if_stmt = r#"fn a() -> i32:
            let a = 1
            let b = 1
            if false:
                return a
            else::
                return b"#;
        return if_stmt.as_bytes(); 
    }
}
