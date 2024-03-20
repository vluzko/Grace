//! Type checking for Grace.
use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter::FromIterator;

use itertools::join;

use expression::*;
use general_utils;
use grace_error::GraceError;
use type_checking::context::Context;
use type_checking::scope::{CanModifyScope, SetScope};
use type_checking::types::Type;

type TypeCheckRes = Result<(Context, Type), GraceError>;

/// A trait for everything that can be type-checked / lives in a context.
pub trait GetContext: SetScope {
    /// Compute the scopes and types for an AST node.
    fn add_to_context(&self, context: Context) -> TypeCheckRes;
}

/// Recursively call add_to_context on functions, structs, and traits.
impl GetContext for Node<Module> {
    fn add_to_context(&self, context: Context) -> TypeCheckRes {
        let mut new_context = context;

        for stmt in self.data.structs.iter() {
            new_context = stmt.add_to_context(new_context)?.0;
        }

        // Add all traits to the context.
        for (k, v) in &self.data.traits {
            new_context.traits.insert(k.clone(), v.clone());
        }

        // Add all trait implementations to the context.
        for (trait_name, struct_name, func_impls) in self.data.trait_implementations.iter() {
            match self.data.traits.get(trait_name) {
                Some(trait_dec) => {
                    // Check that all functions are implemented.
                    let need_impl = HashSet::<Identifier>::from_iter(
                        self.data.traits[trait_name].functions.keys().cloned(),
                    );
                    let has_impl = func_impls
                        .iter()
                        .map(|x| x.data.get_name())
                        .collect::<HashSet<_>>();
                    if need_impl != has_impl {
                        return Err(GraceError::type_error(format!(
                            "Trait {} has implementations for functions {:?} but needs implementations for functions {:?}.",
                            trait_name, has_impl, need_impl
                        )));
                    }

                    let mut func_types = HashMap::new();

                    for dec in func_impls.iter() {
                        // Insert self type data
                        let local_self_type = Type::Named(struct_name.clone());
                        new_context.self_types.insert(dec.scope, local_self_type);

                        // Add the function to the context.
                        let res = dec.add_to_context(new_context)?;
                        new_context = res.0;
                        let func_type = res.1;
                        let func_name = dec.data.get_name();

                        // Check that this function declaration is an actual method of the trait.
                        // Check that the declaration type and the expected type are the same.
                        let expected_type = trait_dec.functions.get(&func_name).expect("Compiler error: function not found in trait, but function names already checked.");
                        new_context.check_grad_and_ref_equality(
                            dec.scope,
                            &func_type,
                            expected_type,
                        )?;

                        // Add the function type to the map
                        func_types.insert(func_name.clone(), func_type.clone());
                    }

                    let alias_type = Type::Named(struct_name.clone());
                    new_context
                        .trait_implementations
                        .insert((trait_name.clone(), alias_type), func_types);
                }
                None => {
                    return Err(GraceError::type_error(format!(
                        "Trait {} not found.",
                        trait_name
                    )));
                }
            }
        }

        for stmt in self.data.functions.iter() {
            new_context = stmt.add_to_context(new_context)?.0;
        }

        Ok((new_context, Type::empty))
    }
}

/// Add all statements to context. Determine type returned by the block.
impl GetContext for Node<Block> {
    fn add_to_context(&self, context: Context) -> TypeCheckRes {
        let mut block_type = Type::Undetermined;

        let mut new_context = context;
        for stmt in self.data.statements.iter() {
            // Get scopes and types for the current statement.
            let res = stmt.add_to_context(new_context)?;
            new_context = res.0;

            // Update the block type if it's a return statement.
            block_type = match stmt.data {
                Stmt::ReturnStmt(_) | Stmt::YieldStmt(_) => {
                    new_context.merge(&block_type, &res.1)?
                }
                Stmt::BreakStmt | Stmt::ContinueStmt => {
                    new_context.merge(&block_type, &Type::empty)?
                }
                Stmt::IfStmt { .. } | Stmt::WhileStmt { .. } => {
                    if res.1 != Type::empty {
                        new_context.merge(&block_type, &res.1)?
                    } else {
                        block_type
                    }
                }
                _ => block_type,
            };
        }

        // Blocks with no return statement have the empty type.
        if block_type == Type::Undetermined {
            block_type = Type::empty;
        }

        new_context.add_type(self.id, block_type.clone());

        Ok((new_context, block_type))
    }
}

/// Add a let statement to the context.
fn _add_let_to_context(
    scope_id: usize,
    context: Context,
    expression: &Node<Expr>,
    type_annotation: &Option<Type>,
) -> TypeCheckRes {
    let (mut c, t) = expression.add_to_context(context)?;
    match &type_annotation {
        Some(ref x) => {
            let actual_type = c.resolve_self_type(x, scope_id)?;
            c.check_grad_and_ref_equality(expression.scope, &t, &actual_type)?;
        }
        None => {}
    };

    Ok((c, t))
}

/// Add a function declaration to context.
fn _add_function_declaration_to_context(
    stmt_id: usize,
    scope_id: usize,
    args: &[(Identifier, Type)],
    kwargs: &[(Identifier, Type, Node<Expr>)],
    block: &Node<Block>,
    mut context: Context,
    return_type: &Type,
) -> TypeCheckRes {
    // Copy the types for arguments
    let mut arg_types = vec![];
    let mut kwarg_types = vec![];
    let mut modifications = vec![];
    for (key, t) in args.iter() {
        let resolved = context.resolve_self_type(t, scope_id)?;
        arg_types.push((key.clone(), resolved.clone()));

        let modification = CanModifyScope::Argument(resolved);
        modifications.push((key, modification));
    }

    // Evaluate scopes and types for all keyword expressions.
    // This must be done *before* any arguments are actually added to scope,
    // because the expressions cannot reference the arguments.
    for (key, t, ref val) in kwargs.iter() {
        // Get context for each expression
        let res = val.add_to_context(context)?;
        context = res.0;

        // Check kwarg expression type matches annotation
        context.check_grad_and_ref_equality(scope_id, &res.1, t)?;

        // Add the type to the function type.
        let resolved = context.resolve_self_type(t, scope_id)?;
        kwarg_types.push((key.clone(), resolved.clone()));

        // Add kwargs to
        let modification = CanModifyScope::Argument(resolved);
        modifications.push((key, modification));
    }
    let binding = Identifier::from("$ret");
    modifications.push((&binding, CanModifyScope::Return(return_type.clone())));

    let new_scope = context.get_mut_scope(scope_id)?;

    for (k, m) in modifications {
        new_scope.append_modification(k, m);
    }

    let resolved_return_t = context.resolve_self_type(return_type, scope_id)?;
    let function_type = Type::Function(arg_types, kwarg_types, Box::new(resolved_return_t.clone()));
    context.add_type(stmt_id, function_type.clone());

    let (mut block_context, block_type) = block.add_to_context(context)?;
    block_context.check_grad_and_ref_equality(scope_id, &resolved_return_t, &block_type)?;

    Ok((block_context, function_type))
}

/// Call add_to_context on all expressions.
impl GetContext for Node<Stmt> {
    fn add_to_context(&self, mut context: Context) -> TypeCheckRes {
        let final_res: TypeCheckRes = match self.data {
            Stmt::LetStmt {
                ref name,
                ref expression,
                ref type_annotation,
            } => {
                let (mut new_context, t) =
                    _add_let_to_context(self.scope, context, expression, type_annotation)?;
                new_context.append_declaration(self.scope, name, self);
                Ok((new_context, t))
            }
            Stmt::AssignmentStmt {
                ref expression,
                ref name,
            } => {
                let (mut c, t) = expression.add_to_context(context)?;
                let expected_type = c.get_type(self.scope, name)?;
                c.check_grad_and_ref_equality(expression.scope, &t, &expected_type)?;
                Ok((c, t))
            }
            Stmt::FunctionDecStmt {
                ref args,
                ref kwargs,
                ref block,
                ref return_type,
                ..
            } => _add_function_declaration_to_context(
                self.id,
                self.scope,
                args,
                kwargs,
                block,
                context,
                return_type,
            ),
            Stmt::WhileStmt {
                ref condition,
                ref block,
            } => {
                let (condition_context, condition_type) = condition.add_to_context(context)?;
                match condition_type {
                    Type::boolean => block.add_to_context(condition_context),
                    x => Err(GraceError::type_error(format!(
                        "Non boolean condition: {:?}",
                        x
                    ))),
                }
            }
            Stmt::IfStmt {
                ref condition,
                ref block,
                ref else_block,
            } => {
                let (new_context, condition_type) = condition.add_to_context(context)?;
                if condition_type != Type::boolean {
                    return Err(GraceError::type_error(format!(
                        "Non boolean condition: {:?}",
                        condition_type
                    )));
                }
                let (new_context, if_type) = block.add_to_context(new_context)?;

                match else_block {
                    Some(b) => {
                        let (new_context, else_type) = b.add_to_context(new_context)?;
                        let merged_t = new_context.merge(&if_type, &else_type)?;
                        Ok((new_context, merged_t))
                    }
                    None => Ok((new_context, if_type)),
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
            Stmt::ReturnStmt(ref expression) => expression.add_to_context(context),
            Stmt::ContinueStmt | Stmt::BreakStmt | Stmt::PassStmt => Ok((context, Type::empty)),
            _ => panic!("add_to_context not implemented for {:?}", self.data),
        };
        match final_res {
            Ok((mut final_c, final_t)) => {
                final_c.add_type(self.id, final_t.clone());
                Ok((final_c, final_t))
            }
            Err(e) => Err(e),
        }
    }
}

impl GetContext for Node<Expr> {
    fn add_to_context(&self, context: Context) -> TypeCheckRes {
        // We separate out the actual function call so we can use annotate_error and still use the ? operator.
        match self._add_to_context(context) {
            Ok(v) => Ok(v),
            Err(e) => Err(self.annotate_error(e)),
        }
    }
}

/// Add a function call to the context.
fn _add_function_call_to_context(
    function: &Node<Expr>,
    args: &Vec<Node<Expr>>,
    kwargs: &Vec<(Identifier, Node<Expr>)>,
    context: Context,
) -> TypeCheckRes {
    let (mut new_c, wrapped_func) = function.add_to_context(context)?;
    let (arg_types, _kwarg_types, ret) = match wrapped_func {
        Type::Function(a, b, c) => Ok((a, b, *c)),
        x => Err(GraceError::type_error(format!(
            "Somehow got a non-function type {:?}",
            x
        ))),
    }?;

    // Check argument length
    // If there's a self type argument, we don't check it.
    // TODO: Cleanup: In pre_cfg_rewrites rewrite args to include the self parameter.
    let arg_types_to_match = match args.len() > 1 && arg_types[0].1 == Type::SelfT {
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
        }
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

    for (i, arg) in args.iter().enumerate() {
        let res = arg.add_to_context(new_c)?;
        new_c = res.0;
        let arg_t = res.1;

        let expected_type = arg_types_to_match[i].1.add_constraint(&arg_types[i].0, arg);

        // TODO: Type checking: Pass error
        new_c.check_grad_and_ref_equality(arg.scope, &arg_t, &expected_type)?;
    }

    for (name, value) in kwargs {
        let res = value.add_to_context(new_c)?;
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
            Err(GraceError::type_error(format!(
                "Invalid kwarg name. Passed {:?}, must be in {:?}",
                types, n
            )))
        }

        let i = find_kwarg_index(&arg_types, name)?;

        let expected_type = arg_types[i].1.add_constraint(&arg_types[i].0, value);

        // // TODO: Type checking: Pass error
        new_c.check_grad_and_ref_equality(value.scope, &kwarg_t, &expected_type)?;
    }
    Ok((new_c, ret))
}

impl Node<Expr> {
    fn _add_to_context(&self, context: Context) -> TypeCheckRes {
        let (mut final_c, final_t) = match self.data {
            Expr::BinaryExpr {
                ref operator,
                ref left,
                ref right,
            } => {
                let (left_c, left_t) = left.add_to_context(context)?;
                let (mut right_c, right_t) = right.add_to_context(left_c)?;

                let (trait_name, method_name) = operator.get_builtin_trait();
                let return_type = match !left_t.is_gradual() || !right_t.is_gradual() {
                    true => right_c.check_trait_method_call(
                        self.scope,
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
                ref operand,
                ref operator,
            } => {
                let (mut operand_c, operand_t) = operand.add_to_context(context)?;
                let (trait_name, method_name) = operator.get_builtin_trait();
                let return_type = match !operand_t.is_gradual() {
                    true => operand_c.check_trait_method_call(
                        self.scope,
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
                ref function,
                ref args,
                ref kwargs,
            } => _add_function_call_to_context(function, args, kwargs, context),
            Expr::StructLiteral {
                ref base,
                ref fields,
            } => {
                let (new_c, base_t) = base.add_to_context(context)?;
                let element_checker =
                    |aggregate: Result<(Context, Vec<Type>), GraceError>,
                     (expr, expected_type): (&Node<Expr>, &Type)| {
                        let (new_c, mut vec_t) = aggregate?;
                        let (mut expr_c, t) = expr.add_to_context(new_c)?;

                        expr_c.check_grad_and_ref_equality(expr.scope, &t, expected_type)?;
                        vec_t.push(t);
                        Ok((expr_c, vec_t))
                    };

                // TODO: cleanup: The record type checker should be separated out.
                match base_t {
                    Type::Record(_, ref field_types) => {
                        let init = Ok((new_c, vec![]));
                        let zipped = fields.iter().zip(field_types.values());
                        let (new_c, _types) = zipped.fold(init, element_checker)?;
                        // TODO: Type checking: Decide if we should return base_t or the true type
                        Ok((new_c, base_t.clone()))
                    }
                    Type::Named(ref name) => {
                        let underlying_type = new_c.get_defined_type(name)?;
                        match underlying_type {
                            Type::Record(_, ref field_types) => {
                                let init = Ok((new_c, vec![]));
                                let zipped = fields.iter().zip(field_types.values());
                                let (new_c, _types) = zipped.fold(init, element_checker)?;
                                // TODO: Type checking: Decide if we should return base_t or the true type
                                Ok((new_c, base_t.clone()))
                            }
                            x => Err(GraceError::type_error(format!(
                                "Expected a record type or named type, got {:?}",
                                x
                            ))),
                        }
                    }
                    x => Err(GraceError::type_error(format!(
                        "Expected a record type or named type, got {:?}",
                        x
                    ))),
                }
            }
            Expr::AttributeAccess {
                ref base,
                ref attribute,
            } => {
                let (new_c, base_t) = base.add_to_context(context)?;
                let attr_t = new_c.resolve_attribute(&base_t, attribute)?;
                Ok((new_c, attr_t))
            }
            // TODO: cleanup: maybe move the renaming into resolve_nested_record?
            Expr::ModuleAccess(ref id, ref names) => {
                let module_type = context.get_node_type(*id)?;
                let t = module_type.resolve_nested_record(&names[1..].to_vec())?;
                // Named types have to be remapped to include the module access
                let renamed_t = match t {
                    Type::Named(_) => Type::Named(Identifier::from(join(
                        names.iter().map(|x| x.name.clone()),
                        ".",
                    ))),
                    x => x,
                };
                Ok((context, renamed_t))
            }
            Expr::Index { ref base, .. } => {
                let (_new_c, _base_t) = base.add_to_context(context)?;
                panic!("Not implemented")
            }
            Expr::IdentifierExpr(ref name) => {
                let t = context.get_type(self.scope, name)?;
                Ok((context, t))
            }
            Expr::Int(_) => Ok((context, Type::i32)),
            Expr::Float(_) => Ok((context, Type::f32)),
            Expr::String(_) => Ok((context, Type::string)),
            Expr::Bool(_) => Ok((context, Type::boolean)),
            Expr::VecLiteral(ref exprs) => {
                let (new_c, vec_t) = _folded_check(exprs, context)?;
                let t = Type::Vector(Box::new(vec_t));
                Ok((new_c, t))
            }
            Expr::TupleLiteral(ref exprs) => {
                let element_checker = |aggregate: Result<(Context, Vec<Type>), GraceError>,
                                       expr: &Node<Expr>| {
                    let (new_c, mut types) = aggregate?;
                    let (new_c, t) = expr.add_to_context(new_c)?;
                    types.push(t);
                    Ok((new_c, types))
                };
                let init: Result<(Context, Vec<Type>), GraceError> = Ok((context, vec![]));
                let (new_c, types) = exprs.iter().fold(init, element_checker)?;
                Ok((new_c, Type::Product(types)))
            }
            Expr::TraitAccess { .. } => Err(GraceError::compiler_error(
                "Found a trait access in type checking".to_string(),
            )),
        }?;
        final_c.add_type(self.id, final_t.clone());
        Ok((final_c, final_t))
    }
}

/// Fold over the expressions, adding them to context and merging the types.
fn _folded_check(exprs: &[Node<Expr>], context: Context) -> Result<(Context, Type), GraceError> {
    fn _agg(
        aggregate: Result<(Context, Type), GraceError>,
        expr: &Node<Expr>,
    ) -> Result<(Context, Type), GraceError> {
        let (new_c, mut merged_t) = aggregate?;
        let (new_c, t) = expr.add_to_context(new_c)?;
        merged_t = new_c.merge(&merged_t, &t)?;
        Ok((new_c, merged_t))
    }
    let init: Result<(Context, Type), GraceError> = Ok((context, Type::Undetermined));
    let (new_c, t) = exprs.iter().fold(init, _agg)?;
    Ok((new_c, t))
}

#[cfg(test)]
mod trait_tests {
    use super::*;
    use compiler_layers;
    use std::fs::File;
    use std::io::Read;
    use testing::minimal_examples;

    #[ignore]
    #[test]
    #[should_panic(expected = "Argument type mismatch")]
    // One trait, one struct, one implementation block that uses self, and a function that uses it
    fn traits_and_self() {
        let mut f = File::open("tests/test_data/trait_impl_self_test.gr").expect("File not found");
        let mut file_contents = String::new();
        f.read_to_string(&mut file_contents).unwrap();

        let (_, _context) = compiler_layers::to_context::<Node<Module>>(file_contents.as_bytes());

        panic!("Unfinished test")
    }

    #[test]
    fn trait_creation() {
        let context = Context::empty();
        let mut module = minimal_examples::trait_module();
        let scoped = module.set_scope(context.root_id, context);
        let (w_types, _) = module.add_to_context(scoped).unwrap();
        assert_eq!(w_types.traits.len(), 1);
        let found_t = &w_types.traits[&minimal_examples::minimal_identifier()];
        let expected_t = &minimal_examples::minimal_trait();
        assert_eq!(found_t, expected_t);
    }
}

#[cfg(test)]
mod expr_type_tests;

#[cfg(test)]
mod stmt_type_tests;
