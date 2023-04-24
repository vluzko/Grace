//! The context object.
use std::collections::HashMap;

use expression::*;
use general_utils;
use grace_error::GraceError;
use type_checking::refinements::check_constraints;
use type_checking::scope::{CanModifyScope, Scope};
use type_checking::types::{Trait, Type};

/// Generate a single binary trait.
fn binary_trait(trait_name: Identifier, method_name: Identifier) -> Trait {
    return Trait {
        name: trait_name,
        functions: hashmap! {
            method_name => Type::Function(
                vec!(
                    (Identifier::from("left"), Type::self_type(Box::new(Type::Undetermined))),
                    (Identifier::from("right"), Type::self_type(Box::new(Type::Undetermined)))
                ), vec!(),
                Box::new(Type::self_type(Box::new(Type::Undetermined)))
            )
        },
    };
}

/// Generate a single unary trait.
fn unary_trait(trait_name: Identifier, method_name: Identifier) -> Trait {
    return Trait {
        name: trait_name,
        functions: hashmap! {
            method_name => Type::Function(
                vec!(
                    (Identifier::from("operand"), Type::self_type(Box::new(Type::Undetermined)))
                ), vec!(),
                Box::new(Type::self_type(Box::new(Type::Undetermined)))
            )
        },
    };
}

fn builtin_numeric() -> Vec<(Identifier, Identifier)> {
    return vec![
        ("Add", "add"),
        ("Sub", "sub"),
        ("Mult", "mult"),
        ("Div", "div"),
    ]
    .into_iter()
    .map(|(a, b)| (Identifier::from(a), Identifier::from(b)))
    .collect();
}

fn builtin_binary_bool() -> Vec<(Identifier, Identifier)> {
    return vec![("And", "and"), ("Or", "or")]
        .into_iter()
        .map(|(a, b)| (Identifier::from(a), Identifier::from(b)))
        .collect();
}

fn builtin_comparison() -> Vec<(Identifier, Identifier)> {
    return vec![
        BinaryOperator::Equal.get_builtin_trait(),
        BinaryOperator::Unequal.get_builtin_trait(),
        BinaryOperator::Greater.get_builtin_trait(),
        BinaryOperator::Less.get_builtin_trait(),
        BinaryOperator::GreaterEqual.get_builtin_trait(),
        BinaryOperator::LessEqual.get_builtin_trait(),
    ];
}

fn builtin_unary() -> Vec<(Identifier, Identifier)> {
    return vec![("Not", "not")]
        .into_iter()
        .map(|(a, b)| (Identifier::from(a), Identifier::from(b)))
        .collect();
}

/// Generate all the builtin binary traits.
fn builtin_binary_traits() -> HashMap<Identifier, Trait> {
    let mut traits = HashMap::new();
    for (tn, mn) in builtin_numeric().into_iter() {
        let trt = binary_trait(tn.clone(), mn);
        traits.insert(tn, trt);
    }

    for (tn, mn) in builtin_binary_bool().into_iter() {
        let trt = binary_trait(tn.clone(), mn);
        traits.insert(tn, trt);
    }

    for (tn, mn) in builtin_comparison().into_iter() {
        let trt = binary_trait(tn.clone(), mn);
        traits.insert(tn, trt);
    }
    return traits;
}

fn builtin_unary_traits() -> HashMap<Identifier, Trait> {
    let mut map = HashMap::new();
    for (tn, mn) in builtin_unary().into_iter() {
        let tr = unary_trait(tn.clone(), mn);
        map.insert(Identifier::from(tn), tr);
    }
    return map;
}

/// Generate all the builtin trait implementations
fn builtin_trait_implementations() -> HashMap<(Identifier, Type), HashMap<Identifier, Type>> {
    let mut impls = HashMap::new();
    for (tn, mn) in builtin_numeric().into_iter() {
        for t in vec![Type::i32, Type::i64, Type::f32, Type::f64] {
            let func_t = Type::Function(
                vec![
                    (Identifier::from("left"), t.clone()),
                    (Identifier::from("right"), t.clone()),
                ],
                vec![],
                Box::new(t.clone()),
            );
            let func_types = hashmap! {mn.clone() => func_t};
            impls.insert((tn.clone(), t), func_types);
        }
    }
    for (tn, mn) in builtin_binary_bool().into_iter() {
        for t in vec![Type::boolean] {
            let func_t = Type::Function(
                vec![
                    (Identifier::from("left"), t.clone()),
                    (Identifier::from("right"), t.clone()),
                ],
                vec![],
                Box::new(t.clone()),
            );
            let func_types = hashmap! {mn.clone() => func_t};
            impls.insert((tn.clone(), t), func_types);
        }
    }

    for (tn, mn) in builtin_comparison().into_iter() {
        for t in vec![Type::boolean, Type::i32, Type::i64, Type::f32, Type::f64] {
            let func_t = Type::Function(
                vec![
                    (Identifier::from("left"), t.clone()),
                    (Identifier::from("right"), t.clone()),
                ],
                vec![],
                Box::new(Type::boolean),
            );
            let func_types = hashmap! {mn.clone() => func_t};
            impls.insert((tn.clone(), t), func_types);
        }
    }

    for (tn, mn) in builtin_unary().into_iter() {
        for t in vec![Type::boolean] {
            let func_t = Type::Function(
                vec![(Identifier::from("operand"), t.clone())],
                vec![],
                Box::new(t.clone()),
            );
            let func_types = hashmap! {mn.clone() => func_t};
            impls.insert((tn.clone(), t), func_types);
        }
    }

    return impls;
}

/// The full scoping and typing context for a compilation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Context {
    // The ID of the root scope of the context.
    pub root_id: usize,
    // A map from Scope IDs to Scopes.
    pub scopes: HashMap<usize, Scope>,
    // A map from Node IDs to Scope IDs. Each node that modifies scope
    // maps to the scope it's contained in.
    pub containing_scopes: HashMap<usize, usize>,
    // A map from Node IDs to types.
    pub type_map: HashMap<usize, Type>,
    // The user-defined types
    pub defined_types: HashMap<Identifier, Type>,
    // A vector containing all the gradual types in context.
    pub gradual_constraints: HashMap<usize, Vec<Type>>,
    // A vector of all the traits in context.
    pub traits: HashMap<Identifier, Trait>,
    // The set of pairs (trait_name, type) available, where type implements trait_name
    // (or the trait corresponding to trait_name, rather).
    // The value is the map from method implementations to types.
    pub trait_implementations: HashMap<(Identifier, Type), HashMap<Identifier, Type>>,
}

/// Constructors
impl Context {
    /// Create a context containing all builtin types and functions.
    pub fn builtin() -> Context {
        let empty = Scope::empty();
        let mut init_scopes = HashMap::new();
        let id = 0;
        init_scopes.insert(id, empty);
        let context = Context {
            root_id: id,
            scopes: init_scopes,
            containing_scopes: HashMap::new(),
            type_map: HashMap::new(),
            defined_types: HashMap::new(),
            gradual_constraints: HashMap::new(),
            traits: builtin_binary_traits(),
            trait_implementations: builtin_trait_implementations(),
        };
        return context;
    }

    /// Create a context that contains only an empty scope.
    pub fn empty() -> Context {
        let root_id = general_utils::get_next_scope_id();
        let mut scopes = HashMap::new();
        scopes.insert(root_id, Scope::empty());
        return Context {
            root_id: root_id,
            scopes: scopes,
            containing_scopes: HashMap::new(),
            type_map: HashMap::new(),
            defined_types: HashMap::new(),
            gradual_constraints: HashMap::new(),
            traits: HashMap::new(),
            trait_implementations: HashMap::new(),
        };
    }

    /// Create a new Context.
    pub fn new_context(scope: Scope, type_map: HashMap<usize, Type>) -> Context {
        let root_id = general_utils::get_next_scope_id();
        let mut scope_map = HashMap::new();
        scope_map.insert(root_id, scope);
        return Context {
            root_id: root_id,
            scopes: scope_map,
            containing_scopes: HashMap::new(),
            type_map: type_map,
            defined_types: HashMap::new(),
            gradual_constraints: HashMap::new(),
            traits: HashMap::new(),
            trait_implementations: HashMap::new(),
        };
    }
}

/// Basic methods
impl Context {
    /// Record the type of a node.
    pub fn add_type(&mut self, id: usize, t: Type) {
        self.type_map.insert(id, t);
    }

    /// Define a named type.
    pub fn define_type(&mut self, name: Identifier, t: Type) {
        self.defined_types.insert(name.clone(), t);
    }

    /// Get the type associated with a particular name.
    pub fn get_defined_type(&self, name: &Identifier) -> Result<Type, GraceError> {
        let possible_type = self.defined_types.get(name);
        return match possible_type {
            Some(t) => Ok(t.clone()),
            None => Err(GraceError::type_error(format!(
                "No underlying type found for named type {:?}",
                name
            ))),
        };
    }

    /// Get the type of the identifier in the given scope.
    pub fn get_type(&self, scope_id: usize, name: &Identifier) -> Type {
        let scope_mod = self.get_declaration(scope_id, name).unwrap();
        let t = match scope_mod {
            CanModifyScope::Statement(_, ref id) => self.type_map.get(id).unwrap().clone(),
            CanModifyScope::Argument(ref t) | CanModifyScope::Return(ref t) => t.clone(),
            CanModifyScope::ImportedModule(ref _id) => panic!("Not implemented"),
        };
        return t;
    }

    /// Get the type of the identifier in the given scope, except it never panics.
    /// Use this one when checking whether to give an identifier a globally unique name.
    pub fn safe_get_type(&self, scope_id: usize, name: &Identifier) -> Option<Type> {
        let maybe_scope_mod = self.get_declaration(scope_id, name);
        return match maybe_scope_mod? {
            CanModifyScope::Statement(_, ref id) => Some(self.type_map.get(id).unwrap().clone()),
            CanModifyScope::Argument(ref t) | CanModifyScope::Return(ref t) => Some(t.clone()),
            CanModifyScope::ImportedModule(ref _id) => panic!("Not implemented"),
        };
    }

    /// Get the type of the given node.
    pub fn get_node_type(&self, node_id: usize) -> Type {
        return self.type_map.get(&node_id).unwrap().clone();
    }

    /// Pretty print
    pub fn pretty_print(&self) -> () {
        for (k, val) in self.type_map.iter() {
            println!("{:?}: {:?}", k, val);
        }
    }

    pub fn print_identifier_map(&self) -> () {
        for (scope_id, scope) in &self.scopes {
            for name in scope.names() {
                let t = self.get_type(*scope_id, &name);
                println!("{:?}: {:?}", name, t)
            }
        }
    }
}

/// Scoping
impl Context {
    /// Get a scope by its ID.
    pub fn get_scope(&self, scope_id: usize) -> &Scope {
        return self.scopes.get(&scope_id).unwrap();
    }

    /// Get a mutable reference to a Scope.
    pub fn get_mut_scope(&mut self, scope_id: usize) -> &mut Scope {
        return self.scopes.get_mut(&scope_id).unwrap();
    }

    /// Create a new scope, returning the ID.
    pub fn new_scope(&mut self, scope: Scope) -> usize {
        let scope_id = general_utils::get_next_scope_id();
        self.scopes.insert(scope_id, scope);
        return scope_id;
    }

    /// Add a statement to a scope.
    pub fn append_declaration(&mut self, scope_id: usize, name: &Identifier, stmt: &Node<Stmt>) {
        let scope = self.scopes.get_mut(&scope_id).unwrap();
        scope.append_declaration(name, stmt);
    }

    /// Add an import to a scope.
    pub fn append_import(&mut self, import: &Import) {
        let import_name = import.path.get(0).unwrap().clone();
        let scope_mod = CanModifyScope::ImportedModule(import.id);

        let scope = self.scopes.get_mut(&self.root_id).unwrap();
        scope
            .declaration_order
            .insert(import_name.clone(), scope.declaration_order.len() + 1);
        scope.declarations.insert(import_name.clone(), scope_mod);
    }

    pub fn get_declaration(&self, scope_id: usize, name: &Identifier) -> Option<&CanModifyScope> {
        let initial_scope = self.scopes.get(&scope_id).unwrap();
        if initial_scope.declarations.contains_key(name) {
            return initial_scope.declarations.get(name);
        } else {
            return match initial_scope.parent_id {
                Some(id) => self.get_declaration(id, name),
                None => None,
            };
        }
    }

    /// Safely access a scope.
    pub fn safe_get_scope(&self, scope_id: usize) -> Result<&Scope, GraceError> {
        match self.scopes.get(&scope_id) {
            Some(scope) => Ok(scope),
            None => Err(GraceError::scoping_error(format!(
                "No scope with id {} found.",
                scope_id
            ))),
        }
    }

    /// Get the scope that declares the given identifier.
    pub fn get_declaring_scope(
        &self,
        scope_id: usize,
        name: &Identifier,
    ) -> Result<usize, GraceError> {
        let initial_scope = self.safe_get_scope(scope_id)?;
        if initial_scope.declarations.contains_key(name) {
            return Ok(scope_id);
        } else {
            return match initial_scope.parent_id {
                Some(id) => self.get_declaring_scope(id, name),
                None => Err(GraceError::scoping_error(format!(
                    "No scope containing identifier {:?} found.",
                    name
                ))),
            };
        }
    }

    pub fn get_struct_and_trait(
        &self,
        scope_id: usize,
    ) -> (Option<Identifier>, Option<Identifier>) {
        let initial_scope = self.scopes.get(&scope_id).unwrap();
        return if initial_scope.maybe_struct.is_some() || initial_scope.maybe_trait.is_some() {
            (
                initial_scope.maybe_struct.clone(),
                initial_scope.maybe_trait.clone(),
            )
        } else {
            match initial_scope.parent_id {
                Some(id) => self.get_struct_and_trait(id),
                None => (None, None),
            }
        };
    }
}

/// Typechecking
impl Context {
    /// Resolve an attribute access within the current context.
    /// # Arguments:
    pub fn resolve_attribute(
        &self,
        base_type: &Type,
        name: &Identifier,
    ) -> Result<Type, GraceError> {
        // Check if this is a direct attribute access
        let unwrapped_self = match base_type {
            Type::self_type(x) => *x.clone(),
            x => x.clone(),
        };
        let attribute_type = match &unwrapped_self {
            Type::Record(_, ref attributes) => attributes.get(name),
            Type::Named(ref t_name) => {
                let record_t = self.defined_types.get(t_name).unwrap();
                match record_t {
                    Type::Record(_, ref attributes) => attributes.get(name),
                    _ => None,
                }
            }
            _ => None,
        };

        return match attribute_type {
            Some(x) => Ok(x.clone()),
            None => {
                // Check if this is a trait access
                let mut possible_traits = vec![];

                for (trait_name, trait_struct) in self.traits.iter() {
                    // Check if this trait has a function with the desired name.
                    if trait_struct.functions.contains_key(name) {
                        // Check if base_type implements this trait.
                        // OPT: We shouldn't be cloning these things. Everything's staying a reference.
                        if self
                            .trait_implementations
                            .contains_key(&(trait_name.clone(), unwrapped_self.clone()))
                        {
                            possible_traits.push(trait_struct);
                        }
                    }
                }

                // Just resolve the trait.
                if possible_traits.len() == 1 {
                    // Get the type of the trait function and return it.
                    return Ok(possible_traits[0].functions.get(name).unwrap().clone());
                }
                // TODO: Handle ambiguous traits.
                else if possible_traits.len() > 1 {
                    panic!("ATTRIBUTE ERROR: Ambiguous trait method call. Base type {:?} call to {:?} could reference any of {:?}.", base_type, name, possible_traits);
                } else {
                    return Err(GraceError::type_error(format!(
                        "ATTRIBUTE ERROR: No matching attribute found for: {:?}, {:?}",
                        base_type, name
                    )));
                }
            }
        };
    }

    /// Modify a Type::Self so it contains whatever Self actually is.
    pub fn resolve_self_type(&self, base_type: &Type, scope_id: usize) -> Type {
        return match base_type {
            Type::self_type(t) => {
                assert!(
                    matches!(**t, Type::Undetermined),
                    "TYPE ERROR: Matching against a self type that is not undetermined: {:?}",
                    base_type
                );
                let (struct_name, _) = self.get_struct_and_trait(scope_id);
                match struct_name {
                    Some(x) => Type::self_type(Box::new(Type::Named(x))),
                    None => panic!("TYPE ERROR: Self used outside of a method implementation."),
                }
            }
            t => t.clone(),
        };
    }

    /// Check if this is a trait method call or an attribute access.
    pub fn trait_information(&self, base_type: &Type, name: &Identifier) -> Option<Identifier> {
        let mut possible_traits = vec![];

        for (trait_name, trait_struct) in self.traits.iter() {
            // Check if this trait has a function with the desired name.
            if trait_struct.functions.contains_key(name) {
                // Check if base_type implements this trait.
                // OPT: We shouldn't be cloning these things. Everything's staying a reference.
                if self
                    .trait_implementations
                    .contains_key(&(trait_name.clone(), base_type.clone()))
                {
                    possible_traits.push(trait_struct);
                }
            }
        }

        return match possible_traits.len() {
            0 => None,
            1 => Some(possible_traits[0].name.clone()),
            _ => panic!("Ambiguous trait"),
        };
    }

    /// Check that a trait method call is valid, and get the return type.
    pub fn check_trait_method_call(
        &mut self,
        scope_id: usize,
        trait_name: &Identifier,
        method_name: &Identifier,
        implementing_type: &Type,
        arg_types: Vec<&Type>,
    ) -> Result<Type, GraceError> {
        let base_t = match implementing_type {
            Type::Refinement(t, _) => t,
            x => x,
        };
        let func_types = (match self
            .trait_implementations
            .get(&(trait_name.clone(), base_t.clone()))
        {
            Some(x) => Ok(x),
            None => Err(GraceError::type_error(format!(
                "No trait implementation found for trait {} and type {:?}",
                trait_name, implementing_type
            ))),
        }?)
        .clone();
        let method_type_result = func_types.get(method_name);
        return match method_type_result {
            Some(method_type) => {
                match method_type {
                    Type::Function(ref args, ref kwargs, ref return_type) => {

                        for ((_, expected_t), actual_t) in args.iter().zip(arg_types.iter()) {
                            self.check_grad_and_ref_equality(scope_id, actual_t, expected_t)?;
                        }

                        if kwargs.len() > 0 {
                            return Err(GraceError::compiler_error(format!(
                                "Trait method call to {}::{} has keyword arguments, which are not supported.",
                                trait_name, method_name
                            )));
                        }

                        Ok(*return_type.clone())
                    },
                    x => Err(GraceError::type_error(format!("Non-function type for a trait method. Trait and method are: {:?} and {:?}. Type is: {:?}", trait_name, method_name, x)))
                }
            }
            None => Err(GraceError::type_error(format!("Could not find implementation of trait method: {:?}.", method_name)))
        };
    }

    /// Get the return type of a binary operator.
    pub fn bin_op_ret_type(
        &self,
        op: &BinaryOperator,
        left: &Type,
        right: &Type,
    ) -> Result<Type, GraceError> {
        let err = Err(GraceError::type_error(format!(
            "TYPE ERROR. Tried to {:?} {:?} and {:?}",
            op, left, right
        )));
        return match op {
            // TODO: 540: Update left and right with an "addable" constraint.
            BinaryOperator::Add
            | BinaryOperator::Sub
            | BinaryOperator::Mult
            | BinaryOperator::Mod => match left {
                Type::Gradual(_) => match right {
                    Type::Gradual(_) => Ok(Type::Gradual(general_utils::get_next_grad())),
                    Type::i32 | Type::i64 | Type::f32 | Type::f64 => Ok(right.clone()),
                    _ => err,
                },
                x => match right {
                    Type::Gradual(_) => Ok(left.clone()),
                    y => x.merge(y),
                },
            },
            BinaryOperator::Div => Ok(Type::f64),
            BinaryOperator::And | BinaryOperator::Or | BinaryOperator::Xor => Ok(Type::boolean),
            _ => err,
        };
    }

    /// Update a gradual type with a new constraint.
    pub fn update_gradual(&mut self, gradual_id: usize, constraint: &Type) -> bool {
        let maybe_constraints = self.gradual_constraints.get_mut(&gradual_id);
        match maybe_constraints {
            Some(constraints) => {
                constraints.push(constraint.clone());
            }
            None => {
                self.gradual_constraints
                    .insert(gradual_id, vec![constraint.clone()]);
            }
        };

        return true;
    }

    pub fn check_function_types(
        &self,
        expected_type: &Type,
        func_type: &Type,
    ) -> Result<(), GraceError> {
        fn check_type_match(t1: &Type, t2: &Type) -> Result<(), GraceError> {
            let self_type_error = GraceError::type_error(format!(
                "A self type inside a trait definition should be undetermined. Got {:?}",
                t1
            ));
            let incompatible_error = GraceError::type_error(format!(
                "Incompatible function types. Called function with type {:?}, received {:?}",
                t1, t2
            ));
            match (t1, t2) {
                (Type::self_type(ref b1), _) => {
                    if **b1 != Type::Undetermined {
                        return Err(self_type_error);
                    }
                }
                (x, y) => {
                    if x != y {
                        return Err(incompatible_error);
                    }
                }
            }
            Ok(())
        }
        match (expected_type, &func_type) {
            (
                Type::Function(ref args_1, ref kwargs_1, ref ret_1),
                Type::Function(ref args_2, ref kwargs_2, ref ret_2),
            ) => {
                for ((_, t1), (_, t2)) in args_1.iter().zip(args_2.iter()) {
                    check_type_match(t1, t2)?;
                }
                for ((_, t1), (_, t2)) in kwargs_1.iter().zip(kwargs_2.iter()) {
                    check_type_match(t1, t2)?;
                }
                check_type_match(ret_1, ret_2)
            }
            x => Err(GraceError::type_error(format!(
                "Somehow got a non function type: {:?}",
                x
            ))),
        }
    }

    pub fn _type_matches(&mut self, scope_id: usize, expr_t: &Type, desired_type: &Type) -> bool {
        if expr_t == desired_type {
            return true;
        } else {
            let unwrapped_self = match expr_t {
                Type::self_type(x) => (**x).clone(),
                x => x.clone(),
            };
            return match desired_type {
                Type::Undetermined => true,
                Type::empty => false,
                Type::i32 | Type::i64 | Type::f32 | Type::f64 | Type::boolean | Type::string => {
                    match unwrapped_self {
                        Type::Refinement(ref base, ..) => {
                            self._type_matches(scope_id, base, desired_type)
                        }
                        Type::Gradual(ref id) => self.update_gradual(*id, desired_type),
                        x => x.has_simple_conversion(desired_type),
                    }
                }
                Type::Product(ref types) => match &unwrapped_self {
                    Type::Product(ref expr_types) => expr_types
                        .iter()
                        .enumerate()
                        .all(|(i, x)| self._type_matches(scope_id, x, &types[i])),
                    _ => false,
                },
                Type::Function(ref args, ref kwargs, ref ret) => match &unwrapped_self {
                    Type::Function(ref e_args, ref e_kwargs, ref e_ret) => {
                        let args_match = args
                            .iter()
                            .enumerate()
                            .all(|(i, x)| self._type_matches(scope_id, &e_args[i].1, &x.1));
                        let kwargs_match = kwargs
                            .iter()
                            .enumerate()
                            .all(|(i, x)| self._type_matches(scope_id, &e_kwargs[i].1, &x.1));
                        args_match && kwargs_match && self._type_matches(scope_id, ret, e_ret)
                    }
                    _ => false,
                },
                Type::Vector(ref t) => match &unwrapped_self {
                    Type::Vector(ref e_t) => self._type_matches(scope_id, e_t, t),
                    _ => false,
                },
                Type::Refinement(_, ref d_conds) => {
                    check_constraints(scope_id, self, d_conds.clone())
                }
                Type::Gradual(ref id) => self.update_gradual(*id, &unwrapped_self),
                Type::Named(..) => desired_type == &unwrapped_self,
                Type::self_type(x) => self._type_matches(scope_id, &unwrapped_self, x),
                _ => false,
            };
        }
    }

    /// Check if the type of expr is a subtype of desired_type.
    /// For types that do *not* include any gradual or refinement types this is equivalent to equality.
    /// For refinement types we have the additional requirement that the refinement constraints be satisfied.
    /// For gradual types *at least one* of the possible gradual types must be a subtype of the desired type.
    pub fn check_grad_and_ref_equality(
        &mut self,
        scope_id: usize,
        expr_t: &Type,
        desired_type: &Type,
    ) -> Result<(), GraceError> {
        match self._type_matches(scope_id, expr_t, desired_type) {
            false => {
                return Err(GraceError::type_error(format!(
                    "Tried to match type {:?} with {:?}",
                    expr_t, desired_type
                )))
            }
            true => Ok(()),
        }
    }
}

/// Helpers
impl Context {
    pub fn print_all_variables(&self) -> Vec<Identifier> {
        let mut all_variables = vec![];
        for scope in self.scopes.values() {
            for key in scope.declaration_order.keys() {
                all_variables.push(key.clone());
            }
        }
        return all_variables;
    }

    pub fn print_all_types(&self) -> Vec<(Identifier, Type)> {
        let mut all_variables = vec![];
        for (scope_id, scope) in self.scopes.iter() {
            for key in scope.declaration_order.keys() {
                let t = self.get_type(*scope_id, key);
                all_variables.push((key.clone(), t.clone()));
            }
        }
        return all_variables;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn literals_are_incompatible() {
        let mut context = Context::builtin();
        let bool_expr: Node<Expr> = Node::from(true);
        let a: i32 = 2;
        let i32_expr: Node<Expr> = Node::from(a);
        let string_expr = Node::from(Expr::String("foo".to_string()));
        assert!(context
            .check_grad_and_ref_equality(bool_expr.scope, &Type::boolean, &Type::boolean)
            .is_ok());
        assert!(context
            .check_grad_and_ref_equality(string_expr.scope, &Type::string, &Type::string)
            .is_ok());
        assert!(context
            .check_grad_and_ref_equality(i32_expr.scope, &Type::i32, &Type::i32)
            .is_ok());
        assert!(!(context.check_grad_and_ref_equality(
            bool_expr.scope,
            &Type::boolean,
            &Type::string
        ))
        .is_ok());
    }

    #[test]
    #[should_panic(
        expected = "Tried to match type Product([i32, i32, i32]) with Product([boolean, boolean, boolean])"
    )]
    fn product_types() {
        let mut context = Context::builtin();
        let bool_expr: Node<Expr> = Node::from(true);
        let product_i32 = Type::Product(vec![Type::i32, Type::i32, Type::i32]);
        let product_bool = Type::Product(vec![Type::boolean, Type::boolean, Type::boolean]);
        context
            .check_grad_and_ref_equality(bool_expr.scope, &product_i32, &product_bool)
            .unwrap();
    }

    #[test]
    #[should_panic(expected = "Tried to match type i32 with empty")]
    fn empty_desired_test() {
        let mut context = Context::builtin();
        context
            .check_grad_and_ref_equality(context.root_id, &Type::i32, &Type::empty)
            .unwrap();
    }

    #[test]
    #[should_panic(expected = "Tried to match type empty with i32")]
    fn empty_expr_type() {
        let mut context = Context::builtin();
        context
            .check_grad_and_ref_equality(context.root_id, &Type::empty, &Type::i32)
            .unwrap();
    }
}
