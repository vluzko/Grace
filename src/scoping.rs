use std::collections::BTreeSet;
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::collections::HashMap;
use std::iter::FromIterator;

use expression::*;
use general_utils;
use typing::{Type, Refinement, Trait};
use refinements::check_constraints;

/// A sum type for things that can modify scope.
/// Currently the only things that can do so are:
/// * import statements
/// * let statements
/// * function declarations
/// * comprehensions
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CanModifyScope {
    Statement(*const Node<Stmt>, usize),
    Argument(Type),
    Return(Type),
    ImportedModule(usize)
}

/// A single layer of scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scope {
    /// The id of the parent scope.
    pub parent_id: Option<usize>,
    // TODO: Consider replacing with an insertion ordered map.
    /// The identifiers declared in this scope, and raw pointers to the statements that created them.
    pub declarations: BTreeMap<Identifier, CanModifyScope>,
    /// The order in which each identifier was declared. (Important for blocks.)
    pub declaration_order: BTreeMap<Identifier, usize>
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
    // A vectgor of all the traits in context.
    pub traits: HashMap<Identifier, Trait>,
    // A map from tuples (struct_name, trait_name) to [maps from method name to method definition]
    pub trait_implementations: HashMap<(Identifier, Identifier), HashMap<Identifier, Node<Stmt>>>
}

pub trait GetContext {
    fn get_usages(&self) -> HashSet<Identifier>;

    fn scopes_and_types(&mut self, parent_id: usize, context: Context) -> (Context, Type);

    /// Get all *non-Argument* declarations.
    fn get_true_declarations(&self, context: &Context) -> HashSet<(Identifier, Type)>;
}


/// Create a Context containing all Grace builtins.
pub fn builtin_context() -> (usize, Context) {
    let empty = Scope::empty();
    let mut init_scopes = HashMap::new();
    let id = general_utils::get_next_scope_id();
    init_scopes.insert(id, empty);
    let context = Context {
        root_id: id, 
        scopes: init_scopes, 
        containing_scopes: HashMap::new(),
        type_map: HashMap::new(), 
        defined_types: HashMap::new(),
        gradual_constraints: HashMap::new(),
        traits: HashMap::new(),
        trait_implementations: HashMap::new()
    };
    return (id, context);
}

impl Scope {
    /// Create an empty scope.
    pub fn empty() -> Scope {
        return Scope{
            parent_id: None,
            declarations: BTreeMap::new(),
            declaration_order: BTreeMap::new()
        };
    }

    /// Create a child of the given parent
    pub fn child(parent_id: usize) -> Scope {
        return Scope{
            parent_id: Some(parent_id),
            declarations: BTreeMap::new(),
            declaration_order: BTreeMap::new()
        };
    }

    pub fn append_declaration(&mut self, name: &Identifier, stmt: &Box<Node<Stmt>>) {
        self.declaration_order.insert(name.clone(), self.declaration_order.len() + 1);
        let scope_mod = CanModifyScope::Statement(stmt.as_ref() as *const _, stmt.id);
        self.declarations.insert(name.clone(), scope_mod);
    }

    pub fn append_modification(&mut self, name: &Identifier, modification: CanModifyScope) {
        self.declaration_order.insert(name.clone(), self.declaration_order.len() + 1);
        self.declarations.insert(name.clone(), modification);
    }
}

// Scoping
impl Context {
    pub fn empty() -> Context {
        let root_id = general_utils::get_next_scope_id();
        let mut scopes = HashMap::new();
        scopes.insert(root_id, Scope::empty());
        return Context{
            root_id: root_id,
            scopes: scopes,
            containing_scopes: HashMap::new(),
            type_map: HashMap::new(),
            defined_types: HashMap::new(),
            gradual_constraints: HashMap::new(),
            traits: HashMap::new(),
            trait_implementations: HashMap::new()
        };
    }
    pub fn new_context(scope: Scope, type_map: HashMap<usize, Type>) -> Context {
        let root_id = general_utils::get_next_scope_id();
        let mut scope_map = HashMap::new();
        scope_map.insert(root_id, scope);
        return Context{
            root_id: root_id,
            scopes: scope_map,
            containing_scopes: HashMap::new(),
            type_map: type_map,
            defined_types: HashMap::new(),
            gradual_constraints: HashMap::new(),
            traits: HashMap::new(),
            trait_implementations: HashMap::new()
        };
    }

    /// Record the type of a node.
    pub fn add_type(&mut self, id: usize, t: Type) {
        self.type_map.insert(id, t);
    }

    /// Define a named type.
    pub fn define_type(&mut self, name: Identifier, t: Type) {
        self.defined_types.insert(name.clone(), t);
    }

    /// Get the type of the identifier in the given scope.
    pub fn get_type(&self, scope_id: usize, name: &Identifier) -> Type {
        let scope_mod = self.get_declaration(scope_id, name).unwrap();
        let t = match scope_mod {
            CanModifyScope::Statement(_, ref id) => {
                self.type_map.get(id).unwrap().clone()
            },
            CanModifyScope::Argument(ref t) | CanModifyScope::Return(ref t) => t.clone(),
            CanModifyScope::ImportedModule(ref _id) => panic!()
        };
        return t;
    }

    /// Get the type of the identifier in the given scope, except it never panics.
    /// Use this one when checking whether to give an identifier a globally unique name.
    pub fn safe_get_type(&self, scope_id: usize, name: &Identifier) -> Option<Type> {
        let maybe_scope_mod = self.get_declaration(scope_id, name);
        return match maybe_scope_mod ? {
            CanModifyScope::Statement(_, ref id) => {
                Some(self.type_map.get(id).unwrap().clone())
            },
            CanModifyScope::Argument(ref t) | CanModifyScope::Return(ref t) => Some(t.clone()),
            CanModifyScope::ImportedModule(ref _id) => panic!()
        };
    }

    /// Get the type of the given node.
    pub fn get_node_type(&self, node_id: usize) -> Type {
        return self.type_map.get(&node_id).unwrap().clone();
    }

    pub fn get_defined_type(&self, name: &Identifier) -> Type {
        return self.defined_types.get(name).unwrap().clone();
    }

    pub fn get_scope(&self, scope_id: usize) -> &Scope {
        return self.scopes.get(&scope_id).unwrap();
    }

    pub fn get_mut_scope(&mut self, scope_id: usize) -> &mut Scope {
        return self.scopes.get_mut(&scope_id).unwrap();
    }

    pub fn add_scope(&mut self, scope_id: usize, scope: Scope) {
        self.scopes.insert(scope_id, scope);
    }

    pub fn new_scope(&mut self, scope: Scope) -> usize {
        let scope_id = general_utils::get_next_scope_id();
        self.scopes.insert(scope_id, scope);
        return scope_id;
    }

    /// Add a statement to a scope.
    pub fn append_declaration(&mut self, scope_id: usize, name: &Identifier, stmt: &Box<Node<Stmt>>) {
        let scope = self.scopes.get_mut(&scope_id).unwrap();
        scope.append_declaration(name, stmt);
    }

    /// Add an import to a scope.
    pub fn append_import(&mut self, import: &Import) {
        let import_name =  import.path.get(0).unwrap().clone();
        let scope_mod = CanModifyScope::ImportedModule(import.id);

        let scope = self.scopes.get_mut(&self.root_id).unwrap();
        scope.declaration_order.insert(import_name.clone(), scope.declaration_order.len() + 1);
        scope.declarations.insert(import_name.clone(), scope_mod);
    }


    pub fn get_declaration(&self, scope_id: usize, name: &Identifier) -> Option<&CanModifyScope> {
        let initial_scope = self.scopes.get(&scope_id).unwrap();
        if scope_id == 0 {
            return None;
        } else if initial_scope.declarations.contains_key(name) {
            return initial_scope.declarations.get(name);
        } else {
            return match initial_scope.parent_id {
                Some(id) => {
                    self.get_declaration(id, name)
                },
                None => None
            };
        }
    }

    pub fn get_declaring_scope(&self, scope_id: usize, name: &Identifier) -> usize {
        let initial_scope = self.scopes.get(&scope_id).unwrap();
        if scope_id == 0 {
            panic!("Reached scope id 0 searching for {}", name);
        } else if initial_scope.declarations.contains_key(name) {
            return scope_id;
        } else {
            return match initial_scope.parent_id {
                Some(id) => {
                    self.get_declaring_scope(id, name)
                },
                None => panic!()
            };
        }
    }

    pub fn print_all_variables(&self) -> Vec<Identifier> {
        let mut all_variables = vec!();
        for scope in self.scopes.values() {
            for key in scope.declaration_order.keys() {
                all_variables.push(key.clone());
            }
        }
        return all_variables;
    }
}

/// Typechecking
impl Context {

    pub fn bin_op_ret_type(&self, op: &BinaryOperator, left: &Type, right: &Type) -> Type {
        return match op {
            // TODO: 540: Update left and right with an "addable" constraint.
            BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mult | BinaryOperator::Mod => match left {
                Type::Gradual(_) => match right {
                    Type::Gradual(_) => Type::Gradual(general_utils::get_next_grad()),
                    Type::i32 | Type::i64 | Type::f32 | Type::f64 => right.clone(),
                    _ => panic!("Type error. Tried to add {:?} and {:?}", left, right)
                }
                x => match right {
                    Type::Gradual(_) => left.clone(),
                    y => x.merge(y)
                }
            },
            BinaryOperator::Div => Type::f64,
            BinaryOperator::And | BinaryOperator::Or | BinaryOperator::Xor => Type::boolean,
            _ => panic!()
        };
    }

    pub fn update_gradual(&mut self, gradual_id: usize, constraint: &Type) -> bool {
        let maybe_constraints = self.gradual_constraints.get_mut(&gradual_id);
        match maybe_constraints {
            Some(constraints) => {constraints.push(constraint.clone());},
            None => {self.gradual_constraints.insert(gradual_id, vec!(constraint.clone()));}
        };
        
        return true;
    }

    /// Check if the type of expr is a subtype of desired_type.
    /// For types that do *not* include any gradual or refinement types this is equivalent to equality.
    /// For refinement types we have the additional requirement that the refinement constraints be satisfied.
    /// For gradual types *at least one* of the possible gradual types must be a subtype of the desired type.
    pub fn check_subtype(&mut self, expr: &Node<Expr>, expr_t: &Type, desired_type: &Type) -> bool {
        if expr_t == desired_type {
            return true;
        } else {
            return match desired_type {
                Type::Undetermined => true,
                Type::empty => false,
                Type::i32 | Type::i64 | Type::f32  | Type::f64 | Type::boolean | Type::string => match expr_t {
                    Type::Refinement(ref base, ..) => self.check_subtype(expr, base, desired_type),
                    Type::Gradual(id) => self.update_gradual(*id, desired_type),
                    x => x.has_simple_conversion(desired_type)
                },
                Type::Product(ref types) => match expr_t {
                    Type::Product(ref expr_types) => expr_types.iter().enumerate().all(|(i, x)| self.check_subtype(expr, x, &types[i])),
                    x => panic!("Can't get {:?} from {:?}", expr_t, x)
                },
                Type::Function(ref args, ref ret) => match expr_t {
                    Type::Function(ref e_args, ref e_ret) => {
                        let args_match = args.iter().enumerate().all(|(i, x)| self.check_subtype(expr, &e_args[i].1, &x.1));
                        args_match && self.check_subtype(expr, ret, e_ret)
                    },
                    x => panic!("Can't get {:?} from {:?}", expr_t, x)
                },
                Type::Vector(ref t) => match expr_t {
                    Type::Vector(ref e_t) => self.check_subtype(expr, e_t, t),
                    x => panic!("Can't get {:?} from {:?}", expr_t, x)
                },
                Type::Refinement(_, ref d_conds) => check_constraints(expr.scope, self, d_conds.clone()),
                Type::Gradual(id) => self.update_gradual(*id, expr_t),
                Type::Named(..) => desired_type == expr_t,
                _ => panic!()
            };
        }
    }

}

impl CanModifyScope {

    pub fn extract_stmt(&self) -> Stmt {
        return unsafe {
            match self {
                CanModifyScope::Statement(stmt_ptr, _) => (**stmt_ptr).data.clone(),
                _ => panic!()
            }
        };
    }

    pub fn get_id(&self) -> usize {
        return match self {
            CanModifyScope::Statement(ref _ptr, ref id) => *id,
            CanModifyScope::ImportedModule(ref id) => *id,
            CanModifyScope::Argument(..) | CanModifyScope::Return(..) => panic!()
        };
    }
}

impl GetContext for Node<Module> {

    fn get_usages(&self) -> HashSet<Identifier> {
        panic!();
    }

    fn scopes_and_types(&mut self, parent_id: usize, mut context: Context) -> (Context, Type) {
        let mut new_scope = Scope::child(parent_id);
        
        for stmt in &self.data.declarations {
            new_scope.append_declaration(&stmt.data.get_name(), &stmt);
        }

        let scope_id = context.new_scope(new_scope);
        self.scope = scope_id;

        let mut new_context = context;
        for stmt in self.data.declarations.iter_mut() {
            new_context = stmt.scopes_and_types(scope_id, new_context).0;
        }

        // Add all traits to the context.
        for (k, v) in &self.data.traits {
            new_context.traits.insert(k.clone(), v.clone());
        }

        // Add all trait implementations to the context.
        let mut ambiguous_names = HashMap::new();

        for (trait_name, struct_name, decs) in self.data.trait_implementations.iter_mut() {
            assert!(self.data.traits.contains_key(&trait_name));
            let trait_dec = self.data.traits.get(&trait_name).unwrap();
            let mut struct_type = new_context.get_type(scope_id, struct_name).clone();
            let (mut traits_implemented, mut attribute_providers) = match struct_type {
                Type::Record(_, _, x, y) => (x, y),
                _ => panic!()  // This should never happen
            };
            traits_implemented.add(trait_name.clone());
            let mut existing_attributes = struct_type.all_attributes();

            let mut need_impl = HashSet::<&Identifier>::from_iter(self.data.traits.keys());
            let mut decs_map = HashMap::new();
            for dec in decs.iter_mut() {
                let res = dec.scopes_and_types(scope_id, new_context);
                new_context = res.0;
                let func_type = res.1;
                let func_name = dec.data.get_name();

                // Check that this function declaration is an actual method of the trait.
                assert!(need_impl.contains(&func_name));
                // Check that the declaration type and the expected type are the same.
                let expected_type = trait_dec.functions.get(&func_name).unwrap();
                assert!(expected_type == &func_type);

                // Remove this function from the set of functions that need to be implemented.
                need_impl.remove(&func_name);

                decs_map.insert(func_name.clone(), dec.clone());

                struct_type.

                // If we have ambiguous names
                if existing_attributes.contains(&func_name) {
                    let mut struct_ambig_names = match ambiguous_names.remove(struct_name) {
                        Some(v) => v,
                        None => HashMap::new()
                    };
                    struct_ambig_names.insert(func_name.clone(), trait_name.clone());

                    if struct_type.has_attribute(&func_name) {
                        struct_ambig_names.insert(func_name.clone(), Identifier::from("Attrib"));
                    }

                    ambiguous_names.insert(struct_name.clone(), struct_ambig_names);

                } else {

                }
            }
            // Demand that all methods of the trait have implementations.
            assert!(need_impl.len() == 0);

            new_context.trait_implementations.insert((trait_name.clone(), struct_name.clone()), decs_map);
        }

        return (new_context, Type::empty);
    }

    fn get_true_declarations(&self, context: &Context) -> HashSet<(Identifier, Type)> {
        let top_level: HashSet<Identifier> = context.get_scope(self.scope).declarations.keys().map(|x| x.clone()).collect();
        let mut with_types = top_level.into_iter().map(|x| {
            let t = context.get_type(self.scope, &x);
            (x, t)
        }).collect();
        for stmt in &self.data.declarations {
            with_types = general_utils::m_union(with_types, stmt.get_true_declarations(context));
        }
        return with_types;
    }
}

impl GetContext for Node<Block> {
    fn get_usages(&self) -> HashSet<Identifier> {
        let mut usages = HashSet::new();
        for stmt in &self.data.statements {
            usages = general_utils::m_union(usages, stmt.get_usages());
        }
        return usages;
    }

    fn scopes_and_types(&mut self, parent_id: usize, mut context: Context) -> (Context, Type) {
        let new_scope = Scope::child(parent_id);
        let scope_id = context.new_scope(new_scope);
        self.scope = scope_id;

        let mut block_type = Type::Undetermined;

        let mut new_context = context;
        for stmt in self.data.statements.iter_mut() {

            // Get scopes and types for the current statement.
            let res = stmt.scopes_and_types(scope_id, new_context);
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

        return (new_context, block_type);
    }

    fn get_true_declarations(&self, context: &Context) -> HashSet<(Identifier, Type)> {
        let top_level: HashSet<Identifier> = context.get_scope(self.scope).declarations.keys().map(|x| x.clone()).collect();
        let mut with_types = top_level.into_iter().map(|x| {
            let t = context.get_type(self.scope, &x);
            (x, t)
        }).collect();
        for stmt in &self.data.statements {
            with_types = general_utils::m_union(with_types, stmt.get_true_declarations(context));
        }
        return with_types;
    }
}

impl GetContext for Node<Stmt> {
    
    fn get_usages(&self) -> HashSet<Identifier> {
        return match self.data {
            Stmt::IfStmt{ref condition, ref block, ref else_block} => {
                let usages = general_utils::m_union(condition.get_usages(), block.get_usages());
                match else_block {
                    Some(y) => general_utils::m_union(usages, y.get_usages()),
                    None => usages
                }
            },
            Stmt::WhileStmt{ref condition, ref block} => general_utils::m_union(condition.get_usages(), block.get_usages()),
            Stmt::FunctionDecStmt{ref block, ..} => block.get_usages(),
            Stmt::ReturnStmt(ref expression) | 
            Stmt::YieldStmt(ref expression) | 
            Stmt::LetStmt{ref expression, ..} |
            Stmt::AssignmentStmt{ref expression, ..} => expression.get_usages(),
            Stmt::BreakStmt | Stmt::ContinueStmt | Stmt::PassStmt => HashSet::new(),
            _ => panic!()
        };
    }

    fn scopes_and_types(&mut self, parent_id: usize, mut context: Context) -> (Context, Type) {
        self.scope = parent_id;
        let (mut final_c, final_t) = match self.data {
            Stmt::LetStmt{ref mut expression, ref type_annotation, ref mut name} => {
                let (c, t) = expression.scopes_and_types(parent_id, context);
                match &type_annotation {
                    Some(x) => assert!(t.is_compatible(x)),
                    None => {}
                };

                (c, t)
            },
            Stmt::AssignmentStmt{ref mut expression, ref mut name} => {
                let (c, t) = expression.scopes_and_types(parent_id, context);
                let expected_type = c.get_type(self.scope, name);
                assert!(t.is_compatible(&expected_type));
                (c, t)
            },
            Stmt::FunctionDecStmt{ref args, ref mut kwargs, ref mut block, ref return_type, ..} => {
                // TODO: Type checking
                let mut new_scope = Scope::child(parent_id);

                // Copy the types for non-keyword arguments
                let mut arg_types = args.clone();

                // Evaluate scopes and types for all keyword expressions.
                // This must be done *before* any arguments are actually added to scope,
                // because the expressions cannot references the arguments.
                for (key, t, ref mut val) in kwargs.iter_mut() {

                    // Get context for each expression
                    let res = val.scopes_and_types(parent_id, context);
                    context = res.0;

                    // Check kwarg expression type matches annotation
                    assert_eq!(*t, res.1);

                    // Add the type to the function type.
                    arg_types.push((key.clone(), t.clone()));
                }

                let function_type = Type::Function(arg_types, Box::new(return_type.clone()));

                context.add_type(self.id, function_type.clone());

                // Add arguments to declarations.
                for (key, t) in args {
                    let modification = CanModifyScope::Argument(t.clone());
                    new_scope.append_modification(key, modification);
                }

                for (key, t, _) in kwargs {
                    let modification = CanModifyScope::Argument(t.clone());
                    new_scope.append_modification(key, modification);
                }

                let ret_modification = CanModifyScope::Return(return_type.clone());
                new_scope.append_modification(&Identifier::from("$ret"), ret_modification);
                
                let scope_id = context.new_scope(new_scope);
                self.scope = scope_id;

                let (block_context, block_type) = block.scopes_and_types(scope_id, context);

                assert!(return_type.is_compatible(&block_type), "{:?} not compatible with {:?}", return_type, block_type);

                (block_context, function_type)
            },
            Stmt::WhileStmt{ref mut condition, ref mut block} => {
                let (mut condition_context, condition_type) = condition.scopes_and_types(parent_id, context);
                assert_eq!(condition_type, Type::boolean);

                let block_scope = Scope{
                    parent_id: Some(parent_id), declarations: BTreeMap::new(), declaration_order: BTreeMap::new()
                };
                let scope_id = condition_context.new_scope(block_scope);
                block.scopes_and_types(scope_id, condition_context)
            },
            Stmt::IfStmt{ref mut condition, ref mut block, ref mut else_block} => {
                let (mut new_context, condition_type) = condition.scopes_and_types(parent_id, context);
                assert_eq!(condition_type, Type::boolean);

                let block_scope = Scope{
                    parent_id: Some(parent_id), declarations: BTreeMap::new(), declaration_order: BTreeMap::new()
                };
                let scope_id = new_context.new_scope(block_scope);
                let res = block.scopes_and_types(scope_id, new_context);
                new_context = res.0;
                let mut if_type = res.1;

                match else_block {
                    Some(b) => {
                        let else_scope = Scope{
                            parent_id: Some(parent_id), declarations: BTreeMap::new(), declaration_order: BTreeMap::new()
                        };
                        let else_scope_id = new_context.new_scope(else_scope);
                        let (else_context, else_type) = b.scopes_and_types(else_scope_id, new_context);
                        if_type = if_type.merge(&else_type);
                        (else_context, if_type)
                    },
                    None => (new_context, if_type)
                }
            },
            Stmt::StructDec{ref name, ref fields} => {
                let mut order = vec!();
                let mut records = BTreeMap::new();
                for (n, t) in fields {
                    order.push(n.clone());
                    records.insert(n.clone(), t.clone());
                }
                let record = Type::Record(order, records, BTreeSet::new(), BTreeMap::new());
                context.define_type(name.clone(), record);
                (context, Type::Named(name.clone()))
            },
            Stmt::ReturnStmt(ref mut expression) => {
                let ret_name = Identifier::from("$ret");
                let exp_type = context.get_type(self.scope, &ret_name);
                let (mut new_c, new_t) = expression.scopes_and_types(parent_id, context);
                assert!(new_c.check_subtype(expression, &new_t, &exp_type));
                
                (new_c, new_t)
            },
            Stmt::ContinueStmt | Stmt::BreakStmt | Stmt::PassStmt => (context, Type::empty),
            _ => panic!("scopes_and_types not implemented for {:?}", self.data)
        };
        final_c.add_type(self.id, final_t.clone());
        return (final_c, final_t);
    }

    fn get_true_declarations(&self, context: &Context) -> HashSet<(Identifier, Type)> {
        return match self.data {
            Stmt::FunctionDecStmt{ref block, ..} => {
                block.get_true_declarations(context)
            },
            _ => HashSet::new()
        };
    }
}

impl GetContext for Node<Expr> {

    fn get_usages(&self) -> HashSet<Identifier> {
        return match self.data {
            Expr::BinaryExpr{ref left, ref right, ..} => {
                general_utils::m_union(left.get_usages(), right.get_usages())
            },
            Expr::ComparisonExpr{ref left, ref right, ..} => {
                general_utils::m_union(left.get_usages(), right.get_usages())
            },
            Expr::UnaryExpr{ref operand, ..} => {
                operand.get_usages()
            },
            Expr::FunctionCall{ref args, ref kwargs, ..} => {
                let mut usages = HashSet::new();
                for expr in args {
                    usages = general_utils::m_union(usages, expr.get_usages());
                }

                for (_, expr) in kwargs {
                    usages = general_utils::m_union(usages, expr.get_usages());
                }

                usages
            },
            Expr::Index{ref base, ref slices} => {
                let first_slice = slices.get(0).unwrap().clone();
                let first_usages = first_slice.0.unwrap().get_usages();
                general_utils::m_union(base.get_usages(), first_usages)
            },
            Expr::IdentifierExpr(ref name) => {
                let mut usages = HashSet::new();
                usages.insert(name.clone());
                usages
            },
            Expr::Int(_) | Expr::Bool(_) | Expr::String(_) | Expr::Float(_) => HashSet::new(),
            _ => panic!()
        };
    }

    fn scopes_and_types(&mut self, parent_id: usize, mut context: Context) -> (Context, Type) {
        self.scope = parent_id;
        let (mut final_c, final_t) = match self.data {
            Expr::ComparisonExpr{ref mut left, ref mut right, ..} => {
                let (left_c, left_t) = left.scopes_and_types(parent_id, context);
                let (right_c, right_t) = right.scopes_and_types(parent_id, left_c);
                assert_eq!(left_t, right_t);
                (right_c, Type::boolean)
            },
            // TODO: Type checking
            Expr::BinaryExpr{ref operator, ref mut left, ref mut right} => {
                let (left_c, left_t) = left.scopes_and_types(parent_id, context);
                let (right_c, right_t) = right.scopes_and_types(parent_id, left_c);
                let return_type = right_c.bin_op_ret_type(operator, &left_t, &right_t);
                (right_c, return_type)
            },
            Expr::UnaryExpr{ref mut operand, ..} => {
                operand.scopes_and_types(parent_id, context)
            }
            // TODO: Type checking
            Expr::FunctionCall{ref mut function, ref mut args, ref mut kwargs} => {
                let (mut new_c, wrapped_func) = function.scopes_and_types(parent_id, context);
                let (arg_types, ret) = match wrapped_func {
                    Type::Function(a, b) => (a, *b.clone()),
                    x => panic!("Expected function type. Got: {:?}", x)
                };
                for (i, arg) in args.into_iter().enumerate() {
                    let res = arg.scopes_and_types(parent_id, new_c);
                    new_c = res.0;
                    let arg_t = res.1;

                    let expected_type = arg_types[i].1.add_constraint(&arg_types[i].0, arg);
                    
                    assert!(new_c.check_subtype(&arg, &arg_t, &expected_type));
                    // assert_eq!(arg_types.get(i).unwrap().1, arg_t);
                }

                for (_, value) in kwargs {
                    let res = value.scopes_and_types(parent_id, new_c);
                    new_c = res.0;
                    let _kwarg_t = res.1;
                }
                (new_c, ret)
            },
            // TODO: Type checking
            Expr::StructLiteral{ref mut base, ref mut fields} => {
                let (mut new_c, base_t) = base.scopes_and_types(parent_id, context);
                for field in fields {
                    let res = field.scopes_and_types(parent_id, new_c);
                    new_c = res.0;
                    let _field_t = res.1;
                }
                (new_c, base_t.clone())
            },
            Expr::AttributeAccess{ref mut base, ref attribute} => {
                let (new_c, base_t) = base.scopes_and_types(parent_id, context);
                let attr_t = base_t.resolve_attribute(attribute);
                (new_c, attr_t)
            },
            Expr::ModuleAccess(ref id, ref mut names) => {
                let module_type = context.get_node_type(*id);
                let t = module_type.resolve_nested_record(&names[1..].to_vec());
                context.add_type(self.id, t.clone());
                (context, t)
            },
            Expr::Index{ref mut base, ..} => {
                let (_new_c, _base_t) = base.scopes_and_types(parent_id, context);
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
                (context, t)
            },
            Expr::Int(_) => {
                // let t = Numeric();
                context.add_type(self.id, Type::i32);
                (context, Type::i32)
            },
            Expr::Float(_) => {
                // let t = FloatingPoint();
                context.add_type(self.id, Type::f32);
                (context, Type::f32)
            },
            Expr::String(_) => {
                let t = Type::string;
                context.add_type(self.id, t.clone());
                (context, t)
            },
            Expr::Bool(_) => {
                let t = Type::boolean;
                context.add_type(self.id, t.clone());
                (context, t)
            },
            Expr::VecLiteral(ref mut exprs) => {
                let mut vec_t = Type::Undetermined;
                let mut new_c = context;
                for expr in exprs {
                    let res = expr.scopes_and_types(parent_id, new_c);
                    new_c = res.0;
                    vec_t = vec_t.merge(&res.1);
                }
                (new_c, Type::Vector(Box::new(vec_t)))
            },
            Expr::SetLiteral(ref mut exprs) => {
                let mut set_t = Type::Undetermined;
                let mut new_c = context;
                for expr in exprs {
                    let res = expr.scopes_and_types(parent_id, new_c);
                    new_c = res.0;
                    set_t = set_t.merge(&res.1);
                }
                (new_c, Type::Parameterized(Identifier::from("Set"), vec!(set_t)))
            },
            Expr::TupleLiteral(ref mut exprs) => {
                let mut types = vec!();
                let mut new_c = context;
                for expr in exprs {
                    let res = expr.scopes_and_types(parent_id, new_c);
                    new_c = res.0;
                    types.push(res.1);
                }
                let t = Type::Product(types);
                (new_c, t)
            },
            _ => panic!()
        };
        final_c.add_type(self.id, final_t.clone());
        return (final_c, final_t);
    }

    fn get_true_declarations(&self, _context: &Context) -> HashSet<(Identifier, Type)> {
        panic!()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use compiler_layers;
    use difference::{Difference, Changeset};
    use regex::Regex;

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
        let compilation = compiler_layers::Compilation::compile(
            &file_name);
        let compiled_module = compilation.modules.get(&"basic_grace".to_string()).unwrap();
        let first_func_id = compiled_module.ast.data.declarations.get(0).unwrap().id;
        let actual_type = compiled_module.context.type_map.get(&first_func_id).unwrap();
        let expected_type = Type::Function(vec!((Identifier::from("arg"), Type::i32)), Box::new(Type::i32));
        assert_eq!(&expected_type, actual_type);
    }

    #[test]
    fn test_function_locals() {
        let func_str = r#"fn a(b: i32, c: i32) -> i32:
        return b + c
        "#;
        let (func_stmt, _) = compiler_layers::to_context::<Node<Stmt>>(func_str.as_bytes());
        let usages = func_stmt.get_usages();
        assert!(usages.contains(&Identifier::from("b")));
        assert!(usages.contains(&Identifier::from("c")));
    }

    #[test]
    fn test_get_declarations() {
        let (func_dec, context) = compiler_layers::to_context::<Node<Stmt>>("fn a(b: i32) -> i32:\n let x = 5 + 6\n return x\n".as_bytes());
        let new_ident = Identifier::from("x");
        let actual = func_dec.get_true_declarations(&context);
        let scope_suffix_regex = Regex::new(r"^\.(\d)+$").unwrap();
        for ptr in actual {
            let changeset = Changeset::new("x", ptr.0.name.as_str(), "");
            for diff in changeset.diffs {
                match diff {
                    Difference::Same(_) => {},
                    Difference::Rem(_) => panic!(),
                    Difference::Add(added_string) => {
                        // Check if the thing being added is a scope ID on the end
                        // of a variable
                        assert!(scope_suffix_regex.is_match(added_string.as_str()));
                    }
                }
            }
        }
    }

    #[cfg(test)]
    mod scope_generation {
        use super::*;
        use parser::Parseable;
        use position_tracker::PosStr;

        fn check_ptr_stmt(ptr: &CanModifyScope, expected: &Stmt) -> bool {
            return match ptr {
                CanModifyScope::Statement(x, _) => {
                    unsafe {
                        let actual_stmt = &(**x).data;
                        assert_eq!(actual_stmt, expected);
                        true
                    }
                },
                x => panic!("Expected a statement modification, found: {:?}", x)
            }
        }

        impl Node<Stmt> {
            fn mod_scope(&mut self, new_scope: usize) {
                self.scope = new_scope;
                match self.data {
                    Stmt::LetStmt{ref mut expression, ..} => {
                        expression.mod_scope(new_scope);
                    }
                    _ => {}
                }
            }
        }

        /// Utility function to recursively modify the scope of an Expr AST.
        impl Node<Expr> {
            fn mod_scope(&mut self, new_scope: usize) {
                self.scope = new_scope;
                match self.data {
                    Expr::BinaryExpr{ref mut left, ref mut right, ..} => {
                        left.mod_scope(new_scope);
                        right.mod_scope(new_scope);
                    },
                    Expr::FunctionCall{ref mut function, ref mut args, ..} => {
                        function.mod_scope(new_scope);
                        for arg in args {
                            arg.mod_scope(new_scope);
                        }
                    },
                    _ => {}
                }
            }
        }

        #[test]
        fn test_block_scope() {
            let mut e1 = Node::<Expr>::parse(PosStr::from("5 + -1"));
            e1.mod_scope(1);
            let s1 = Stmt::LetStmt{name: Identifier::from("a"), type_annotation: None, expression: Node::from(e1)};

            let mut e2 = Node::<Expr>::parse(PosStr::from("true and false"));
            e2.mod_scope(1);
            let s2 = Stmt::LetStmt{name: Identifier::from("b"), type_annotation: None, expression: Node::from(e2)};

            let input = r#"
            let a = 5 + -1
            let b = true and false"#;

            let (block, context) = compiler_layers::to_context::<Node<Block>>(input.as_bytes());
            let scope = context.get_scope(block.scope);
            assert_eq!(scope.declarations.len(), 2);

            let stmt1_pointer = context.get_declaration(block.scope, &Identifier::from("a")).unwrap();
            check_ptr_stmt(stmt1_pointer, &s1);

            let stmt2_pointer = context.get_declaration(block.scope, &Identifier::from("b")).unwrap();
            check_ptr_stmt(stmt2_pointer, &s2);
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
                for literal in literals.iter_mut() {
                    let (id, mut context) = builtin_context();
                    context = literal.scopes_and_types(id, context).0;
                    assert_eq!(context.scopes.get(&context.root_id).unwrap(), &Scope::empty());
                }
            }
        }
        
        #[cfg(test)]
        mod stmts {
            use super::*;

            #[test]
            fn test_let_stmt() {
                let (block, context) = compiler_layers::to_context::<Node<Block>>("let a = 1".as_bytes());
                assert_eq!(context.scopes.len(), 2);
                let scope = context.get_scope(block.scope);
                assert_eq!(scope.declarations.len(), 1);
                assert!(scope.declarations.contains_key(&Identifier::from("a")));
            }

            #[test]
            fn test_function_decl() {
                let block_str = r#"
                fn a() -> i32:
                    return 0

                fn b() -> i32:
                    return 1
                "#;
                let (block, context) = compiler_layers::to_context::<Node<Block>>(block_str.as_bytes());
                let fn1 = context.get_declaration(block.scope, &Identifier::from("a")).unwrap();
                let fn2 = context.get_declaration(block.scope, &Identifier::from("b")).unwrap();
                match fn1.extract_stmt() {
                    Stmt::FunctionDecStmt{name, ..} => {
                        assert_eq!(name, Identifier::from("a"))
                    },
                    _ => panic!()
                };

                match fn2.extract_stmt() {
                    Stmt::FunctionDecStmt{name, .. } => {
                        assert_eq!(name, Identifier::from("b"))
                    },
                    _ => panic!()
                };
            }
        }

        
    }
}
