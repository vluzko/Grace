use std::collections::{BTreeMap, HashSet, HashMap};
use std::iter::FromIterator;

use expression::*;
use general_utils;
use type_checking::types::{Type};
use type_checking::context::Context;
use type_checking::scope::{Scope, CanModifyScope};
use grace_error::GraceError;

type TypeCheckRes = Result<(Context, Type), GraceError>;

/// A trait for everything that can be type-checked / lives in a context.
pub trait GetContext {

    /// Compute the scopes and types for an AST node.
    fn scopes_and_types(&mut self, parent_id: usize, context: Context) -> TypeCheckRes;
}


impl GetContext for Node<Module> {
    fn scopes_and_types(&mut self, parent_id: usize, mut context: Context) -> TypeCheckRes {
        let mut new_scope = Scope::child(parent_id);
        
        for stmt in &self.data.functions {
            new_scope.append_declaration(&stmt.data.get_name(), &stmt);
        }
        
        for stmt in &self.data.structs {
            new_scope.append_declaration(&stmt.data.get_name(), &stmt);
        }

        let scope_id = context.new_scope(new_scope);
        self.scope = scope_id;
        let mut new_context = context;

        for stmt in self.data.structs.iter_mut() {
            new_context = stmt.scopes_and_types(scope_id, new_context)?.0;
        }

        // Add all traits to the context.
        for (k, v) in &self.data.traits {
            new_context.traits.insert(k.clone(), v.clone());
        }

        // Add all trait implementations to the context.
        for (trait_name, struct_name, func_impls) in self.data.trait_implementations.iter_mut() {
            assert!(self.data.traits.contains_key(&trait_name));

            // Create the scope for this trait implementation
            let implementation_scope = Scope::child_trait_impl(scope_id, struct_name, trait_name);
            let impl_scope_id = new_context.new_scope(implementation_scope);

            let trait_dec = self.data.traits.get(&trait_name).unwrap();

            // The names of functions the trait needs implementations for.
            let mut need_impl = HashSet::<&Identifier>::from_iter(self.data.traits[trait_name].functions.keys());

            let mut func_types = HashMap::new();

            for dec in func_impls.iter_mut() {
                let res = dec.scopes_and_types(impl_scope_id, new_context)?;
                new_context = res.0;
                let func_type = res.1;
                let func_name = dec.data.get_name();

                // Check that this function declaration is an actual method of the trait.
                assert!(need_impl.contains(&func_name));
                // Check that the declaration type and the expected type are the same.
                let expected_type = trait_dec.functions.get(&func_name).unwrap();

                match (expected_type, &func_type) {
                    (Type::Function(ref args_1, ref ret_1), Type::Function(ref args_2, ref ret_2)) => {
                        for ((_, t1), (_, t2)) in args_1.iter().zip(args_2.iter()) {
                            match (t1, t2) {
                                (Type::self_type(ref b1), _) => assert!(**b1 == Type::Undetermined,
                                    "TYPE ERROR: A self type inside a trait definition should be undetermined. Got {:?}", args_1),
                                (x, y) => assert!(x == y,
                                    "TYPE ERROR: Incompatible function types. Called function with type {:?}, received {:?}", expected_type, func_type)
                            };
                        }
                        match (&**ret_1, &**ret_2) {
                            (Type::self_type(ref b1), _) => assert!(**b1 == Type::Undetermined,
                                "TYPE ERROR: A self type inside a trait definition should be undetermined. Got {:?}", args_1),
                            (x, y) => assert!(x == y,
                                "TYPE ERROR: Incompatible function types. Called function with type {:?}, received {:?}", expected_type, func_type)
                        };
                    },
                    x => panic!("TYPE ERROR: Somehow got a non function type: {:?}", x)
                }

                // Add the function type to the map
                func_types.insert(func_name.clone(), func_type.clone());

                // Remove this function from the set of functions that need to be implemented.
                need_impl.remove(&func_name);
            }
            // Demand that all methods of the trait have implementations.
            assert!(need_impl.len() == 0);

            let alias_type = Type::Named(struct_name.clone());
            new_context.trait_implementations.insert((trait_name.clone(), alias_type), func_types);
        }

        for stmt in self.data.functions.iter_mut() {
            new_context = stmt.scopes_and_types(scope_id, new_context)?.0;
        }

        return Ok((new_context, Type::empty));
    }
}


impl GetContext for Node<Block> {
    fn scopes_and_types(&mut self, parent_id: usize, mut context: Context) -> TypeCheckRes {
        let new_scope = Scope::child(parent_id);
        let scope_id = context.new_scope(new_scope);
        self.scope = scope_id;

        let mut block_type = Type::Undetermined;

        let mut new_context = context;
        for stmt in self.data.statements.iter_mut() {

            // Get scopes and types for the current statement.
            let res = stmt.scopes_and_types(scope_id, new_context)?;
            new_context = res.0;

            // Update the block type if it's a return statement.
            block_type = match stmt.data {
                Stmt::ReturnStmt(_) | Stmt::YieldStmt(_) => {
                    block_type.merge(&res.1)
                },
                Stmt::BreakStmt | Stmt::ContinueStmt => {
                    block_type.merge(&Type::empty);
                    break;
                },
                Stmt::IfStmt{..} | Stmt::WhileStmt{..} => {
                    if res.1 != Type::empty {
                        block_type.merge(&res.1)
                    } else {
                        block_type
                    }
                },
                _ => block_type
            };

            // Add declarations to scope.
            match &stmt.data {
                Stmt::FunctionDecStmt{ref name, ..} | Stmt::LetStmt{ref name, ..} | Stmt::StructDec{ref name, ..}  => {
                    new_context.append_declaration(self.scope, name, &stmt);
                },
                _ => {}
            };
        }

        // Blocks with no return statement have the empty type.
        if block_type == Type::Undetermined {
            block_type = Type::empty;
        }

        new_context.add_type(self.id, block_type.clone());

        return Ok((new_context, block_type));
    }
}


impl GetContext for Node<Stmt> {
    fn scopes_and_types(&mut self, parent_id: usize, mut context: Context) -> TypeCheckRes {
        self.scope = parent_id;
        let final_res: TypeCheckRes = match self.data {
            Stmt::LetStmt{ref mut expression, ref type_annotation, ..} => {
                let (mut c, t) = expression.scopes_and_types(parent_id, context)?;
                match &type_annotation {
                    Some(ref x) => {
                        let actual_type = c.resolve_self_type(x, self.scope);
                        assert!(c.check_subtype(&expression, &t, &actual_type));
                    },
                    None => {}
                };

                Ok((c, t))
            },
            Stmt::AssignmentStmt{ref mut expression, ref mut name} => {
                let (mut c, t) = expression.scopes_and_types(parent_id, context)?;
                let expected_type = c.get_type(self.scope, name);
                assert!(c.check_subtype(&expression, &t, &expected_type));
                Ok((c, t))
            },
            Stmt::FunctionDecStmt{ref args, ref mut kwargs, ref mut block, ref return_type, ..} => {
                // TODO: Type checking
                let mut new_scope = Scope::child(parent_id);

                // Copy the types for non-keyword arguments
                let mut arg_types = vec!();

                for (key, t) in args.iter() {
                    let resolved = context.resolve_self_type(t, self.scope);
                    arg_types.push((key.clone(), resolved.clone()));

                    // Add kwargs to
                    let modification = CanModifyScope::Argument(resolved);
                    new_scope.append_modification(key, modification);
                }

                // Evaluate scopes and types for all keyword expressions.
                // This must be done *before* any arguments are actually added to scope,
                // because the expressions cannot references the arguments.
                for (key, t, ref mut val) in kwargs.iter_mut() {

                    // Get context for each expression
                    let res = val.scopes_and_types(parent_id, context)?;
                    context = res.0;

                    // Check kwarg expression type matches annotation
                    assert_eq!(*t, res.1);

                    // Add the type to the function type.
                    let resolved = context.resolve_self_type(t, self.scope);
                    arg_types.push((key.clone(), resolved.clone()));

                    // Add kwargs to
                    let modification = CanModifyScope::Argument(resolved);
                    new_scope.append_modification(key, modification);
                }

                let resolved_return_t = context.resolve_self_type(return_type, self.scope);

                let function_type = Type::Function(arg_types, Box::new(resolved_return_t));

                context.add_type(self.id, function_type.clone());

                let ret_modification = CanModifyScope::Return(return_type.clone());
                new_scope.append_modification(&Identifier::from("$ret"), ret_modification);
                
                let scope_id = context.new_scope(new_scope);
                self.scope = scope_id;

                let (block_context, block_type) = block.scopes_and_types(scope_id, context)?;

                assert!(return_type.is_compatible(&block_type), "{:?} not compatible with {:?}", return_type, block_type);

                Ok((block_context, function_type))
            },
            Stmt::WhileStmt{ref mut condition, ref mut block} => {
                let (mut condition_context, condition_type) = condition.scopes_and_types(parent_id, context)?;
                assert_eq!(condition_type, Type::boolean);

                let block_scope = Scope::child(parent_id);

                let scope_id = condition_context.new_scope(block_scope);
                block.scopes_and_types(scope_id, condition_context)
            },
            Stmt::IfStmt{ref mut condition, ref mut block, ref mut else_block} => {
                let (mut new_context, condition_type) = condition.scopes_and_types(parent_id, context)?;
                assert_eq!(condition_type, Type::boolean);

                let block_scope = Scope::child(parent_id); 
                let scope_id = new_context.new_scope(block_scope);
                let res = block.scopes_and_types(scope_id, new_context);

                match res {
                    Ok(v) => {
                        new_context = v.0;
                        let mut if_type = v.1;
        
                        match else_block {
                            Some(b) => {
                                let else_scope = Scope::child(parent_id);
                                let else_scope_id = new_context.new_scope(else_scope);
                                let (else_context, else_type) = b.scopes_and_types(else_scope_id, new_context)?;
                                if_type = if_type.merge(&else_type);
                                Ok((else_context, if_type))
                            },
                            None => Ok((new_context, if_type))
                        }
                    },
                    Err(e) => Err(e)
                }
            },
            Stmt::StructDec{ref name, ref fields} => {
                let mut order = vec!();
                let mut records = BTreeMap::new();
                for (n, t) in fields {
                    order.push(n.clone());
                    records.insert(n.clone(), t.clone());
                }
                let record = Type::Record(order, records);
                context.define_type(name.clone(), record);
                Ok((context, Type::Named(name.clone())))
            },
            Stmt::ReturnStmt(ref mut expression) => {
                let ret_name = Identifier::from("$ret");
                let exp_type = context.get_type(self.scope, &ret_name);
                let (mut new_c, new_t) = expression.scopes_and_types(parent_id, context)?;
                assert!(new_c.check_subtype(expression, &new_t, &exp_type));
                
                Ok((new_c, new_t))
            },
            Stmt::ContinueStmt | Stmt::BreakStmt | Stmt::PassStmt => Ok((context, Type::empty)),
            _ => panic!("scopes_and_types not implemented for {:?}", self.data)
        };
        return match final_res {
            Ok((mut final_c, final_t)) => {
                final_c.add_type(self.id, final_t.clone());
                Ok((final_c, final_t))
            },
            Err(e) => Err(e)
        }

    }
}


impl GetContext for Node<Expr> {
    fn scopes_and_types(&mut self, parent_id: usize, mut context: Context) -> TypeCheckRes {
        self.scope = parent_id;
        let final_res: TypeCheckRes = match self.data {
            Expr::ComparisonExpr{ref mut left, ref mut right, ..} => {
                let (left_c, left_t) = left.scopes_and_types(parent_id, context)?;
                let (right_c, right_t) = right.scopes_and_types(parent_id, left_c)?;
                assert_eq!(left_t, right_t);
                Ok((right_c, Type::boolean))
            },
            // TODO: Type checking
            Expr::BinaryExpr{ref operator, ref mut left, ref mut right} => {
                let (left_c, left_t) = left.scopes_and_types(parent_id, context)?;
                let (right_c, right_t) = right.scopes_and_types(parent_id, left_c)?;

                let (trait_name, method_name) = operator.get_builtin_trait();
                let return_type = match !left_t.is_gradual() || !right_t.is_gradual() {
                    true  => right_c.check_trait_method_call(&trait_name, &method_name, &left_t, vec!(&left_t, &right_t)),
                    false => Type::Gradual(general_utils::get_next_grad())
                };

                // let return_type = right_c.bin_op_ret_type(operator, &left_t, &right_t);
                Ok((right_c, return_type))
            },
            Expr::UnaryExpr{ref mut operand, ..} => {
                operand.scopes_and_types(parent_id, context)
            }
            // TODO: Type checking
            Expr::FunctionCall{ref mut function, ref mut args, ref mut kwargs} => {
                let (mut new_c, wrapped_func) = function.scopes_and_types(parent_id, context)?;
                let (arg_types, ret) = match wrapped_func {
                    Type::Function(a, b) => (a, *b.clone()),
                    x => panic!("Expected function type. Got: {:?}", x)
                };
                for (i, arg) in args.into_iter().enumerate() {
                    let res = arg.scopes_and_types(parent_id, new_c)?;
                    new_c = res.0;
                    let arg_t = res.1;

                    let expected_type = arg_types[i].1.add_constraint(&arg_types[i].0, arg);

                    assert!(new_c.check_subtype(&arg, &arg_t, &expected_type));
                    // assert_eq!(arg_types.get(i).unwrap().1, arg_t);
                }

                for (_, value) in kwargs {
                    let res = value.scopes_and_types(parent_id, new_c)?;
                    new_c = res.0;
                    let _kwarg_t = res.1;
                }
                Ok((new_c, ret))
            },
            // TODO: Type checking
            Expr::StructLiteral{ref mut base, ref mut fields} => {
                let (new_c, base_t) = base.scopes_and_types(parent_id, context)?;
                let element_checker = |aggregate: Result<(Context, Vec<Type>), GraceError>, expr: &mut Node<Expr>| {
                    match aggregate {
                        Ok((new_c, mut vec_t)) => {
                            let res = expr.scopes_and_types(parent_id, new_c);
                            match res {
                                Ok((new_c, t)) => {
                                    vec_t.push(t);
                                    Ok((new_c, vec_t))
                                },
                                Err(x) => Err(x)
                            }
                        },
                        x => x
                    }
                };
                let init = Ok((new_c, vec!()));
                let res = fields.iter_mut().fold(init, element_checker);
                match res {
                    Ok((new_c, _)) => {
                        Ok((new_c, base_t.clone()))
                    },
                    Err(x) => Err(x)
                }

            },
            Expr::AttributeAccess{ref mut base, ref attribute} => {
                let (new_c, base_t) = base.scopes_and_types(parent_id, context)?;
                let attr_t = new_c.resolve_attribute(&base_t, attribute);
                Ok((new_c, attr_t))
            },
            Expr::ModuleAccess(ref id, ref mut names) => {
                let module_type = context.get_node_type(*id);
                let t = module_type.resolve_nested_record(&names[1..].to_vec());
                context.add_type(self.id, t.clone());
                Ok((context, t))
            },
            Expr::Index{ref mut base, ..} => {
                let (_new_c, _base_t) = base.scopes_and_types(parent_id, context)?;
                panic!()
            },
            Expr::IdentifierExpr(ref mut name) => {
                let t = match context.safe_get_type(self.scope, name) {
                    Some(t2) => t2,
                    None => {
                        context.get_type(self.scope, name)
                    }
                };
                context.add_type(self.id, t.clone());
                Ok((context, t))
            },
            Expr::Int(_) => {
                // let t = Numeric();
                context.add_type(self.id, Type::i32);
                Ok((context, Type::i32))
            },
            Expr::Float(_) => {
                // let t = FloatingPoint();
                context.add_type(self.id, Type::f32);
                Ok((context, Type::f32))
            },
            Expr::String(_) => {
                let t = Type::string;
                context.add_type(self.id, t.clone());
                Ok((context, t))
            },
            Expr::Bool(_) => {
                let t = Type::boolean;
                context.add_type(self.id, t.clone());
                Ok((context, t))
            },
            Expr::VecLiteral(ref mut exprs) => {
                let element_checker = |aggregate: Result<(Context, Type), GraceError>, expr: &mut Node<Expr>| {
                    match aggregate {
                        Ok((new_c, mut vec_t)) => {
                            let res = expr.scopes_and_types(parent_id, new_c);
                            match res {
                                Ok((new_c, t)) => {
                                    vec_t = vec_t.merge(&t);
                                    Ok((new_c, vec_t))
                                },
                                Err(x) => Err(x)
                            }
                        },
                        x => x
                    }
                };
                let init: Result<(Context, Type), GraceError> = Ok((context, Type::Undetermined));
                let res = exprs.iter_mut().fold(init, element_checker);
                match res {
                    Ok((new_c, vec_t)) => {
                        let t = Type::Vector(Box::new(vec_t));
                        Ok((new_c, t))
                    },
                    Err(x) => Err(x)
                }

            },
            Expr::SetLiteral(ref mut exprs) => {
                let element_checker = |aggregate: Result<(Context, Type), GraceError>, expr: &mut Node<Expr>| {
                    match aggregate {
                        Ok((new_c, mut set_t)) => {
                            let res = expr.scopes_and_types(parent_id, new_c);
                            match res {
                                Ok((new_c, t)) => {
                                    set_t = set_t.merge(&t);
                                    Ok((new_c, set_t))
                                },
                                Err(x) => Err(x)
                            }
                        },
                        x => x
                    }
                };
                let init: Result<(Context, Type), GraceError> = Ok((context, Type::Undetermined));
                let res = exprs.iter_mut().fold(init, element_checker);
                match res {
                    Ok((new_c, set_t)) => {
                        let t = Type::Parameterized(Identifier::from("Set"), vec!(set_t));
                        Ok((new_c, t))
                    },
                    Err(x) => Err(x)
                }
             },
            //     let mut set_t = Type::Undetermined;
            //     let mut new_c = context;
            //     let mut error = None;
            //     for expr in exprs {
            //         let res = expr.scopes_and_types(parent_id, new_c);
            //         match res {
            //             Ok(v) => {
            //                 new_c = v.0;
            //                 set_t = set_t.merge(&v.1);
            //             },
            //             Err(e) => {
            //                 error = Some(e);
            //                 break;
            //             }
            //         }

            //     }
            //     match error {
            //         None => Ok((new_c, Type::Parameterized(Identifier::from("Set"), vec!(set_t)))),
            //         Some(e) => Err(e)
            //     }
            // },
            Expr::TupleLiteral(ref mut exprs) => {
                let element_checker = |aggregate: Result<(Context, Vec<Type>), GraceError>, expr: &mut Node<Expr>| {
                    match aggregate {
                        Ok((new_c, mut types)) => {
                            let res = expr.scopes_and_types(parent_id, new_c);
                            match res {
                                Ok((new_c, t)) => {
                                    types.push(t);
                                    Ok((new_c, types))
                                },
                                Err(x) => Err(x)
                            }
                        },
                        x => x
                    }
                };
                let init:Result<(Context, Vec<Type>), GraceError> = Ok((context, vec!()));
                let res = exprs.iter_mut().fold(init, element_checker); 
                match res {
                    Ok((new_c, types)) => {
                        let t = Type::Product(types);
                        Ok((new_c, t))
                    },
                    Err(x) => Err(x)
                }

            },
            _ => panic!()
        };
        return match final_res {
            Ok((mut final_c, final_t)) => {
                final_c.add_type(self.id, final_t.clone());
                Ok((final_c, final_t))
            },
            Err(e) => Err(e)
        }
    }
}


#[cfg(test)]
mod tests{
    use super::*;
    use compiler_layers;
    use std::fs::File;
    use std::io::Read;

    #[cfg(test)]
    mod expected_failures {
        use super::*;
        #[test]
        #[should_panic]
        fn add_incompatible() {
            let input = "fn a():\n   let x = \"a\" + 0";
            compiler_layers::Compilation::compile_from_string(&input.to_string());
        }
    }

    #[test]
    fn test_basic_grace_function_dec() {
        let file_name = "test_data/basic_grace.gr".to_string();
        let compilation = compiler_layers::Compilation::compile(&file_name);
        let compiled_module = compilation.modules.get(&"basic_grace".to_string()).unwrap();
        let first_func_id = compiled_module.ast.data.functions.get(0).unwrap().id;
        let actual_type = compiled_module.context.type_map.get(&first_func_id).unwrap();
        let expected_type = Type::Function(vec!((Identifier::from("arg"), Type::i32)), Box::new(Type::i32));
        assert_eq!(&expected_type, actual_type);
    }

    #[test]
    // One trait, one struct, one implmentation block that uses self, and a function that uses it
    fn traits_and_self() {
        let mut f = File::open("test_data/trait_impl_self_test.gr").expect("File not found");
        let mut file_contents = String::new();
        f.read_to_string(&mut file_contents).unwrap();

        let _compilation = compiler_layers::to_context::<Node<Module>>(file_contents.as_bytes());
        panic!("Unfinished test")
    }

    #[cfg(test)]
    mod exprs {
        use super::*;

        /// Test scope modifications from literals.
        /// All should be empty.
        #[test]
        fn test_literal_scope() {
            let mut literals: Vec<Node<Expr>> = vec![
                Node::from(1), 
                Node::from(0.5), 
                Node::from(Expr::String("asdf".to_string())), 
                Node::from(true)
            ];
            for _ in literals.iter_mut() {
                let context = Context::builtin();
                let _id = context.root_id;
                panic!("Unfinished test.")
                // context = literal.scopes_and_types(id, context)?.0;
                // assert_eq!(context.scopes.get(&context.root_id).unwrap(), &Scope::empty());
            }
        }
    }
}