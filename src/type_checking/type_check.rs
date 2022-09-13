use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter::FromIterator;

use itertools::join;

use expression::*;
use general_utils;
use grace_error::GraceError;
use type_checking::context::Context;
use type_checking::scope::{CanModifyScope, Scope};
use type_checking::types::Type;

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
            let mut need_impl =
                HashSet::<&Identifier>::from_iter(self.data.traits[trait_name].functions.keys());

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
                    (
                        Type::Function(ref args_1, ref kwargs_1, ref ret_1),
                        Type::Function(ref args_2, ref kwargs_2, ref ret_2),
                    ) => {
                        for ((_, t1), (_, t2)) in args_1.iter().zip(args_2.iter()) {
                            match (t1, t2) {
                                (Type::self_type(ref b1), _) => assert!(**b1 == Type::Undetermined,
                                    "TYPE ERROR: A self type inside a trait definition should be undetermined. Got {:?}", args_1),
                                (x, y) => assert!(x == y,
                                    "TYPE ERROR: Incompatible function types. Called function with type {:?}, received {:?}", expected_type, func_type)
                            };
                        }
                        for ((_, t1), (_, t2)) in kwargs_1.iter().zip(kwargs_2.iter()) {
                            match (t1, t2) {
                                (Type::self_type(ref b1), _) => assert!(**b1 == Type::Undetermined,
                                    "TYPE ERROR: A self type inside a trait definition should be undetermined. Got {:?}", kwargs_1),
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
                    }
                    x => panic!("TYPE ERROR: Somehow got a non function type: {:?}", x),
                }

                // Add the function type to the map
                func_types.insert(func_name.clone(), func_type.clone());

                // Remove this function from the set of functions that need to be implemented.
                need_impl.remove(&func_name);
            }
            // Demand that all methods of the trait have implementations.
            assert!(need_impl.len() == 0);

            let alias_type = Type::Named(struct_name.clone());
            new_context
                .trait_implementations
                .insert((trait_name.clone(), alias_type), func_types);
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
                Stmt::ReturnStmt(_) | Stmt::YieldStmt(_) => block_type.merge(&res.1),
                Stmt::BreakStmt | Stmt::ContinueStmt => {
                    block_type.merge(&Type::empty);
                    break;
                }
                Stmt::IfStmt { .. } | Stmt::WhileStmt { .. } => {
                    if res.1 != Type::empty {
                        block_type.merge(&res.1)
                    } else {
                        block_type
                    }
                }
                _ => block_type,
            };

            // Add declarations to scope.
            match &stmt.data {
                Stmt::FunctionDecStmt { ref name, .. }
                | Stmt::LetStmt { ref name, .. }
                | Stmt::StructDec { ref name, .. } => {
                    new_context.append_declaration(self.scope, name, &stmt);
                }
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
            Stmt::LetStmt {
                ref mut expression,
                ref type_annotation,
                ..
            } => {
                let (mut c, t) = expression.scopes_and_types(parent_id, context)?;
                match &type_annotation {
                    Some(ref x) => {
                        let actual_type = c.resolve_self_type(x, self.scope);
                        assert!(c.check_subtype(&expression, &t, &actual_type));
                    }
                    None => {}
                };

                Ok((c, t))
            }
            Stmt::AssignmentStmt {
                ref mut expression,
                ref mut name,
            } => {
                let (mut c, t) = expression.scopes_and_types(parent_id, context)?;
                let expected_type = c.get_type(self.scope, name);
                assert!(c.check_subtype(&expression, &t, &expected_type));
                Ok((c, t))
            }
            Stmt::FunctionDecStmt {
                ref args,
                ref mut kwargs,
                ref mut block,
                ref return_type,
                ..
            } => {
                // TODO: Type checking
                let mut new_scope = Scope::child(parent_id);

                // Copy the types for arguments
                let mut arg_types = vec![];
                let mut kwarg_types = vec![];

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
                    kwarg_types.push((key.clone(), resolved.clone()));

                    // Add kwargs to
                    let modification = CanModifyScope::Argument(resolved);
                    new_scope.append_modification(key, modification);
                }

                let resolved_return_t = context.resolve_self_type(return_type, self.scope);

                let function_type = Type::Function(arg_types, kwarg_types, Box::new(resolved_return_t));

                context.add_type(self.id, function_type.clone());

                let ret_modification = CanModifyScope::Return(return_type.clone());
                new_scope.append_modification(&Identifier::from("$ret"), ret_modification);

                let scope_id = context.new_scope(new_scope);
                self.scope = scope_id;

                let (block_context, block_type) = block.scopes_and_types(scope_id, context)?;

                assert!(
                    return_type.is_compatible(&block_type),
                    "{:?} not compatible with {:?}",
                    return_type,
                    block_type
                );

                Ok((block_context, function_type))
            }
            Stmt::WhileStmt {
                ref mut condition,
                ref mut block,
            } => {
                let (mut condition_context, condition_type) =
                    condition.scopes_and_types(parent_id, context)?;
                assert_eq!(condition_type, Type::boolean);

                let block_scope = Scope::child(parent_id);

                let scope_id = condition_context.new_scope(block_scope);
                block.scopes_and_types(scope_id, condition_context)
            }
            Stmt::IfStmt {
                ref mut condition,
                ref mut block,
                ref mut else_block,
            } => {
                let (mut new_context, condition_type) =
                    condition.scopes_and_types(parent_id, context)?;
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
                                let (else_context, else_type) =
                                    b.scopes_and_types(else_scope_id, new_context)?;
                                if_type = if_type.merge(&else_type);
                                Ok((else_context, if_type))
                            }
                            None => Ok((new_context, if_type)),
                        }
                    }
                    Err(e) => Err(e),
                }
            }
            Stmt::StructDec {
                ref name,
                ref fields,
            } => {
                let mut order = vec![];
                let mut records = BTreeMap::new();
                for (n, t) in fields {
                    order.push(n.clone());
                    records.insert(n.clone(), t.clone());
                }
                let record = Type::Record(order, records);
                context.define_type(name.clone(), record);
                Ok((context, Type::Named(name.clone())))
            }
            Stmt::ReturnStmt(ref mut expression) => {
                let ret_name = Identifier::from("$ret");
                let exp_type = context.get_type(self.scope, &ret_name);
                let (mut new_c, new_t) = expression.scopes_and_types(parent_id, context)?;
                assert!(new_c.check_subtype(expression, &new_t, &exp_type));

                Ok((new_c, new_t))
            }
            Stmt::ContinueStmt | Stmt::BreakStmt | Stmt::PassStmt => Ok((context, Type::empty)),
            _ => panic!("scopes_and_types not implemented for {:?}", self.data),
        };
        return match final_res {
            Ok((mut final_c, final_t)) => {
                final_c.add_type(self.id, final_t.clone());
                Ok((final_c, final_t))
            }
            Err(e) => Err(e),
        };
    }
}

impl GetContext for Node<Expr> {
    fn scopes_and_types(&mut self, parent_id: usize, context: Context) -> TypeCheckRes {
        return match self._scopes_and_types(parent_id, context) {
            Ok(v) => Ok(v),
            Err(e) => Err(self.annotate_error(e)),
        };
    }
}

impl Node<Expr> {
    fn _scopes_and_types(&mut self, parent_id: usize, context: Context) -> TypeCheckRes {
        self.scope = parent_id;
        let (mut final_c, final_t) = match self.data {
            Expr::BinaryExpr {
                ref operator,
                ref mut left,
                ref mut right,
            } => {
                let (left_c, left_t) = left.scopes_and_types(parent_id, context)?;
                let (right_c, right_t) = right.scopes_and_types(parent_id, left_c)?;

                let (trait_name, method_name) = operator.get_builtin_trait();
                let return_type = match !left_t.is_gradual() || !right_t.is_gradual() {
                    true => right_c.check_trait_method_call(
                        &trait_name,
                        &method_name,
                        &left_t,
                        vec![&left_t, &right_t],
                    ),
                    false => Ok(Type::Gradual(general_utils::get_next_grad())),
                }?;

                Ok((right_c, return_type))
            }
            Expr::UnaryExpr {
                ref mut operand,
                ref operator,
            } => {
                let (operand_c, operand_t) = operand.scopes_and_types(parent_id, context)?;
                let (trait_name, method_name) = operator.get_builtin_trait();
                let return_type = match !operand_t.is_gradual() {
                    true => operand_c.check_trait_method_call(
                        &trait_name,
                        &method_name,
                        &operand_t,
                        vec![&operand_t],
                    ),
                    false => Ok(Type::Gradual(general_utils::get_next_grad())),
                }?;
                Ok((operand_c, return_type))
            }
            Expr::FunctionCall {
                ref mut function,
                ref mut args,
                ref mut kwargs,
            } => {
                let (mut new_c, wrapped_func) = function.scopes_and_types(parent_id, context)?;
                let (arg_types, _kwarg_types, ret) = match wrapped_func {
                    Type::Function(a, b, c) => Ok((a, b, *c.clone())),
                    x => Err(GraceError::type_error(format!("Somehow got a non-function type {:?}", x)))
                }?;

                // Check argument length
                // If there's a self type argument, we don't check it.
                // TODO: Cleanup: In pre_cfg_rewrites rewrite args to include the self parameter.
                let arg_types_to_match = match args.len() > 1 && arg_types[0].1 == Type::self_type(Box::new(Type::Undetermined)) {
                    true => {
                        if args.len() != arg_types.len() - 1 {
                            return Err(GraceError::type_error(format!(
                                "Function call to {:?} has {} arguments, but expected {}",
                                function,
                                args.len(),
                                arg_types.len() - 1
                            )));
                        }
                        &arg_types[1..]

                    },
                    false => {
                        if args.len() != arg_types.len() {
                            return Err(GraceError::type_error(format!(
                                "Function call to {:?} has {} arguments, but expected {}",
                                function,
                                args.len(),
                                arg_types.len()
                            )));
                        }
                        &arg_types
                    }
                };

                for (i, arg) in args.into_iter().enumerate() {
                    let res = arg.scopes_and_types(parent_id, new_c)?;
                    new_c = res.0;
                    let arg_t = res.1;

                    let expected_type = arg_types_to_match[i].1.add_constraint(&arg_types[i].0, arg);

                    // TODO: Type checking: Pass error
                    match new_c.check_subtype(&arg, &arg_t, &expected_type) {
                        true => {}
                        false => {
                            return Err(GraceError::type_error(format!("Argument type mismatch")))
                        }
                    }
                }

                for (name, value) in kwargs {
                    let res = value.scopes_and_types(parent_id, new_c)?;
                    new_c = res.0;
                    let kwarg_t = res.1;

                    fn find_kwarg_index(
                        types: &Vec<(Identifier, Type)>,
                        n: &Identifier,
                    ) -> Result<usize, GraceError> {
                        for (i, (ident, _)) in types.iter().enumerate() {
                            if ident == n {
                                return Ok(i);
                            }
                        }
                        return Err(GraceError::type_error(format!(
                            "Invalid kwarg name. Passed {:?}, must be in {:?}",
                            types, n
                        )));
                    }

                    let i = find_kwarg_index(&arg_types, &name)?;

                    let expected_type = arg_types[i].1.add_constraint(&arg_types[i].0, value);

                    // // TODO: Type checking: Pass error
                    match new_c.check_subtype(&value, &kwarg_t, &expected_type) {
                        true => {}
                        false => {
                            return Err(GraceError::type_error(format!("Argument type mismatch")))
                        }
                    }
                }
                Ok((new_c, ret))
            }
            Expr::StructLiteral {
                ref mut base,
                ref mut fields,
            } => {
                let (new_c, base_t) = base.scopes_and_types(parent_id, context)?;
                let element_checker =
                    |aggregate: Result<(Context, Vec<Type>), GraceError>,
                     (expr, expected_type): (&mut Node<Expr>, &Type)| {
                        let (new_c, mut vec_t) = aggregate?;
                        let (mut expr_c, t) = expr.scopes_and_types(parent_id, new_c)?;

                        if expr_c.check_subtype(&expr, &t, expected_type) {
                            vec_t.push(t);
                            Ok((expr_c, vec_t))
                        } else {
                            // TODO: Error messages: More info
                            Err(GraceError::type_error(format!("Wrong type for attribute.")))
                        }
                    };

                // TODO: cleanup: The record type checker should be separated out.
                match base_t {
                    Type::Record(_, ref field_types) => {
                        let init = Ok((new_c, vec![]));
                        let zipped = fields.iter_mut().zip(field_types.values());
                        let (new_c, _types) = zipped.fold(init, element_checker)?;
                        // TODO: Type checking: Decide if we should return base_t or the true type
                        Ok((new_c, base_t.clone()))
                    },
                    Type::Named(ref name) => {
                        let underlying_type = new_c.get_defined_type(name)?;
                        match underlying_type {
                            Type::Record(_, ref field_types) => {
                                let init = Ok((new_c, vec![]));
                                let zipped = fields.iter_mut().zip(field_types.values());
                                let (new_c, _types) = zipped.fold(init, element_checker)?;
                                // TODO: Type checking: Decide if we should return base_t or the true type
                                Ok((new_c, base_t.clone()))
                            },
                            x => Err(GraceError::type_error(format!("Expected a record type or named type, got {:?}", x)))
                        }
                    },
                    x => Err(GraceError::type_error(format!("Expected a record type or named type, got {:?}", x)))
                }
            }
            Expr::AttributeAccess {
                ref mut base,
                ref attribute,
            } => {
                let (new_c, base_t) = base.scopes_and_types(parent_id, context)?;
                let attr_t = new_c.resolve_attribute(&base_t, attribute)?;
                Ok((new_c, attr_t))
            }
            // TODO: cleanup: maybe move the renaming into resolve_nested_record?
            Expr::ModuleAccess(ref id, ref mut names) => {
                let module_type = context.get_node_type(*id);
                let t = module_type.resolve_nested_record(&names[1..].to_vec())?;
                // Named types have to be remapped to include the module access
                let renamed_t = match t {
                    Type::Named(_) => Type::Named(
                        Identifier::from(
                            join(names.iter().map(|x| x.name.clone()), ".")
                        )
                    ),
                    x => x
                };
                Ok((context, renamed_t))
            }
            Expr::Index { ref mut base, .. } => {
                let (_new_c, _base_t) = base.scopes_and_types(parent_id, context)?;
                panic!("Not implemented")
            }
            Expr::IdentifierExpr(ref mut name) => match context.safe_get_type(self.scope, name) {
                Some(t) => Ok((context, t)),
                None => Err(GraceError::type_error("Failed to locate identifier in scope".to_string()))
            },
            Expr::Int(_) => Ok((context, Type::i32)),
            Expr::Float(_) => Ok((context, Type::f32)),
            Expr::String(_) => Ok((context, Type::string)),
            Expr::Bool(_) => Ok((context, Type::boolean)),
            Expr::VecLiteral(ref mut exprs) => {
                let element_checker =
                    |aggregate: Result<(Context, Type), GraceError>, expr: &mut Node<Expr>| {
                        let (c, mut vec_t) = aggregate?;
                        let (new_c, t) = expr.scopes_and_types(parent_id, c)?;
                        // TODO: Type checking: Use context level merge.
                        vec_t = vec_t.merge(&t);
                        Ok((new_c, vec_t))
                    };
                let init: Result<(Context, Type), GraceError> = Ok((context, Type::Undetermined));
                let (new_c, vec_t) = exprs.iter_mut().fold(init, element_checker)?;
                let t = Type::Vector(Box::new(vec_t));
                Ok((new_c, t))
            }
            Expr::SetLiteral(ref mut exprs) => {
                let element_checker =
                    |aggregate: Result<(Context, Type), GraceError>, expr: &mut Node<Expr>| {
                        let (new_c, mut set_t) = aggregate?;
                        let (new_c, t) = expr.scopes_and_types(parent_id, new_c)?;
                        set_t = set_t.merge(&t);
                        Ok((new_c, set_t))
                    };
                let init: Result<(Context, Type), GraceError> = Ok((context, Type::Undetermined));
                let (new_c, set_t) = exprs.iter_mut().fold(init, element_checker)?;
                let t = Type::Parameterized(Identifier::from("Set"), vec![set_t]);
                Ok((new_c, t))
            }
            Expr::TupleLiteral(ref mut exprs) => {
                let element_checker =
                    |aggregate: Result<(Context, Vec<Type>), GraceError>, expr: &mut Node<Expr>| {
                        let (new_c, mut types) = aggregate?;
                        let (new_c, t) = expr.scopes_and_types(parent_id, new_c)?;
                        types.push(t);
                        Ok((new_c, types))
                    };
                let init: Result<(Context, Vec<Type>), GraceError> = Ok((context, vec![]));
                let (new_c, types) = exprs.iter_mut().fold(init, element_checker)?;
                Ok((new_c, Type::Product(types)))
            }
            _ => panic!(),
        }?;
        final_c.add_type(self.id, final_t.clone());
        Ok((final_c, final_t))
    }
}

#[cfg(test)]
mod scope_tests {
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
    #[should_panic(expected = "Argument type mismatch")]
    // One trait, one struct, one implementation block that uses self, and a function that uses it
    fn traits_and_self() {
        let mut f = File::open("tests/test_data/trait_impl_self_test.gr").expect("File not found");
        let mut file_contents = String::new();
        f.read_to_string(&mut file_contents).unwrap();
        

        let (_, context) = compiler_layers::to_context::<Node<Module>>(file_contents.as_bytes());
        // println!("{:?}", compilation);
        context.print_all_variables();
        for (k, t) in context.print_all_types() {
            println!("{:?}: {:?}", k, t);
        }
        // println!("{:?}", context.print_all_types());
        // panic!("Unfinished test")
    }

    #[cfg(test)]
    mod exprs {
        use super::*;

        /// Test scope modifications from literals.
        /// All should be empty.
        // TODO: Should be a proptest.
        #[test]
        fn test_literal_scope() {
            let original = Context::builtin();
            let inputs = vec![
                Node::<Expr>::from(5),
                Node::<Expr>::from(5.0),
                Node::<Expr>::from(true),
            ];
            // let mut input = Node::<Expr>::from(5);
            for mut input in inputs {
                let context = Context::builtin();
                let (new_c, _) = input.scopes_and_types(0, context).unwrap();
                assert_eq!(new_c.scopes, original.scopes);
            }
        }
    }
}

#[cfg(test)]
mod type_tests {
    use super::*;

    #[cfg(test)]
    mod exprs {
        use super::*;
        use type_checking::types::Trait;
        use grace_error::ErrorDetails;
        use test_utils;

        /// Check that an expression has the desired type.
        fn check_expr(context: Context, mut expr: Node<Expr>, expected_type: Type) {
            let (_, t) = expr.scopes_and_types(0, context).unwrap();
            assert_eq!(t, expected_type);
        }

        /// Check that an expression has the desired type in a trivial context
        fn simple_check_expr(expr: Node<Expr>, expected_type: Type) {
            let context = Context::builtin();
            check_expr(context, expr, expected_type);
        }

        fn fail_check_expr(context: Context, mut expr: Node<Expr>) {
            let res = expr.scopes_and_types(0, context);
            match res {
                Ok((_, t)) => panic!("Expected failed type check. Expr has type {:?}", t),
                Err(e) => match e.underlying {
                    ErrorDetails::TypeError { .. } => {}
                    x => panic!("Got non-type error: {:?}", x),
                },
            }
        }

        #[test]
        fn test_literals() {
            simple_check_expr(Node::<Expr>::from(5), Type::i32);
            simple_check_expr(Node::<Expr>::from(5.0), Type::f32);
            simple_check_expr(Node::<Expr>::from(true), Type::boolean);
        }

        #[test]
        fn type_check_function_call() {
            // Create function type
            let context = Context::builtin();
            let (with_func, _) =
            test_utils::add_function_to_context(context, "foo", vec![Type::i32], vec!(), Type::i32);
            let expr = Node::from(Expr::FunctionCall {
                function: wrap(Expr::from("foo")),
                args: vec![Node::from(1)],
                kwargs: vec![],
            });

            check_expr(with_func, expr, Type::i32);
        }

        #[test]
        fn function_call_wrong_args() {
            let context = Context::builtin();
            let (with_func, _) =
                test_utils::add_function_to_context(context, "foo", vec![Type::i32], vec!(), Type::i32);
            let expr = Node::from(Expr::FunctionCall {
                function: wrap(Expr::from("foo")),
                args: vec![Node::from(true)],
                kwargs: vec![],
            });
            fail_check_expr(with_func, expr);
        }

        #[test]
        fn type_check_comparison_exprs() {
            let operand = Node::from(0);
            let comparisons = vec![BinaryOperator::Equal, BinaryOperator::Unequal];

            for comp in comparisons {
                let expr = Node::from(Expr::BinaryExpr {
                    operator: comp,
                    left: Box::new(operand.clone()),
                    right: Box::new(operand.clone()),
                });
                simple_check_expr(expr, Type::boolean);
            }
        }

        #[test]
        fn type_check_unary_exprs() {
            let operand = Node::from(true);
            let operators = vec![UnaryOperator::Not];
            for op in operators {
                let expr = Node::from(Expr::UnaryExpr {
                    operator: op,
                    operand: Box::new(operand.clone()),
                });
                simple_check_expr(expr, Type::boolean);
            }
        }

        #[test]
        fn type_check_unary_exprs_expect_fail() {
            let context = Context::builtin();
            let operand = Node::from(7);
            let operator = UnaryOperator::Not;
            let expr = Node::from(Expr::UnaryExpr {
                operator: operator,
                operand: Box::new(operand.clone()),
            });
            fail_check_expr(context, expr);
        }

        #[test]
        fn type_check_binary_exprs() {
            let operand = Node::from(0);
            let comparisons = vec![BinaryOperator::Equal, BinaryOperator::Unequal];

            for comp in comparisons {
                let expr = Node::from(Expr::BinaryExpr {
                    operator: comp,
                    left: Box::new(operand.clone()),
                    right: Box::new(operand.clone()),
                });
                simple_check_expr(expr, Type::boolean);
            }
        }

        #[test]
        fn type_check_complex_literals() {
            // panic!("To implement: Tests of complex literals type checking: vector, set, tuple")
        }

        #[test]
        fn type_check_struct_literals() {
            let context = Context::builtin();
            let mut attr_map = BTreeMap::new();
            attr_map.insert(Identifier::from("a"), Type::i32);

            let new_context = test_utils::add_struct_to_context(context, "A", attr_map.clone());

            let expr = Node::from(Expr::StructLiteral {
                base: Box::new(Node::from("A")),
                fields: vec![Node::from(4)],
            });

            let record_t = Type::Record(vec![Identifier::from("a")], attr_map);

            check_expr(new_context, expr, record_t);
        }

        #[test]
        fn type_check_struct_literal_fail() {
            let context = Context::builtin();
            let mut attr_map = BTreeMap::new();
            attr_map.insert(Identifier::from("a"), Type::i32);

            let new_context = test_utils::add_struct_to_context(context, "A", attr_map);

            let expr = Node::from(Expr::StructLiteral {
                base: Box::new(Node::from("A")),
                fields: vec![Node::from(true)],
            });

            fail_check_expr(new_context, expr);
        }

        #[test]
        fn type_check_attribute_access() {
            let context = Context::builtin();
            let mut attr_map = BTreeMap::new();
            attr_map.insert(Identifier::from("a"), Type::i32);

            let new_context = test_utils::add_struct_to_context(context, "A", attr_map);

            let base = Node::from(Expr::StructLiteral {
                base: Box::new(Node::from("A")),
                fields: vec![Node::from(4)],
            });
            let expr = Node::from(Expr::AttributeAccess {
                base: Box::new(base.clone()),
                attribute: Identifier::from("a"),
            });
            check_expr(new_context, expr, Type::i32);
        }

        #[test]
        fn type_check_nonexistent_attribute_access() {
            let context = Context::builtin();
            let base = Node::from(0);
            let attribute = Identifier::from("string");
            let expr = Node::from(Expr::AttributeAccess {
                base: Box::new(base.clone()),
                attribute: attribute,
            });
            fail_check_expr(context, expr);
        }

        #[test]
        fn type_check_trait_access() {
            let mut context = Context::builtin();
            // make a trait
            let trait_functions = hashmap! {Identifier::from("test_funcname")=>Type::Function(vec!(), vec!(), Box::new(Type::i32))};
            let test_trait = Trait {
                name: Identifier::from("test_trait"),
                functions: trait_functions.clone(),
            };
            context
                .traits
                .insert(Identifier::from("test_trait"), test_trait);
            // make a struct
            let test_struct_map = btreemap! {Identifier::from("test_identifier")=>Type::i32};
            let mut test_context = test_utils::add_struct_to_context(context, "test_struct", test_struct_map);
            // implement that trait for that struct
            let trait_impl_key = (
                Identifier::from("test_trait"),
                Type::Named(Identifier::from("test_struct")),
            );
            test_context
                .trait_implementations
                .insert(trait_impl_key, trait_functions);
            // call it, expect OK;
            let base = Node::from(Expr::StructLiteral {
                base: Box::new(Node::from("test_struct")),
                fields: vec![Node::from(4)],
            });
            let expr = Node::from(Expr::AttributeAccess {
                base: Box::new(base.clone()),
                attribute: Identifier::from("test_funcname"),
            });
            check_expr(
                test_context,
                expr,
                Type::Function(vec![], vec![], Box::new(Type::i32)),
            );
        }

        #[test]
        fn type_check_module_access() {
            let mut context = Context::builtin();

            // Add module type to context
            let func_type = Type::func_no_args(Type::i32);
            let type_map = btreemap! {Identifier::from("a") => func_type.clone()};
            let module_type = Type::module_from_map(type_map);
            let id = general_utils::get_next_id();
            context.add_type(id, module_type);

            let module_access = Node::from(Expr::ModuleAccess(
                id,
                vec![Identifier::from("A"), Identifier::from("a")],
            ));

            check_expr(context, module_access, func_type);
        }

        #[test]
        fn type_check_module_access_fail() {
            let mut context = Context::builtin();

            // Add module type to context
            let func_type = Type::func_no_args(Type::i32);
            let type_map = btreemap! {Identifier::from("a") => func_type.clone()};
            let module_type = Type::module_from_map(type_map);
            let id = general_utils::get_next_id();
            context.add_type(id, module_type);

            let module_access = Node::from(Expr::ModuleAccess(
                id,
                vec![Identifier::from("A"), Identifier::from("b")],
            ));

            fail_check_expr(context, module_access);
        }

        #[test]
        fn type_check_identifier() {
            let init = Context::builtin();
            let context = test_utils::add_identifier_to_context(init, "a", Expr::from(0));
            let expr = Node::from("a");
            check_expr(context, expr, Type::i32);
        }
    }
}
