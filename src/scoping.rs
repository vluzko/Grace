use std::collections::{BTreeSet, BTreeMap, HashSet, HashMap};
use std::iter::FromIterator;

use expression::*;
use general_utils;
use refinements::check_constraints;

/// Types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub enum Type {
    i32,
    i64,
    f32,
    f64,
    ui32,
    ui64,
    string,
    boolean,
    empty,
    self_type(Box<Type>),
    // A sum type, e.g. a union type
    Sum(Vec<Type>),
    // A product type, e.g. a tuple
    Product(Vec<Type>),
    // A vector type.
    Vector(Box<Type>),
    // A vector of argument names and types, and the return type
    Function(Vec<(Identifier, Type)>, Box<Type>),
    // A referenced to a named type.
    Named(Identifier),
    // Struct{name: Identifier, attributes: BTreeMap<Identifier, Type>, methods: BTreeMap<Identifier, Type>}
    Parameterized(Identifier, Vec<Type>),
    // Attribute names, attribute types
    Record(Vec<Identifier>, BTreeMap<Identifier, Type>),
    Module(Vec<Identifier>, BTreeMap<Identifier, Type>),
    Gradual(usize),
    Refinement(Box<Type>, Vec<Refinement>),
    Undetermined
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Refinement {
    pub operator: ComparisonOperator,
    pub left: Box<Node<Expr>>,
    pub right: Box<Node<Expr>>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trait {
    pub name: Identifier,
    pub functions: HashMap<Identifier, Type>
}

impl Type {

    /// Get the name of this type in WAST.
    pub fn wast_name(&self) -> String {
        match self {
            &Type::i32 => "i32".to_string(),
            &Type::i64 => "i64".to_string(),
            &Type::f32 => "f32".to_string(),
            &Type::f64 => "f64".to_string(),
            &Type::ui32 => "i32".to_string(),
            &Type::ui64 => "i64".to_string(),
            &Type::boolean => "i32".to_string(),
            &Type::empty => "".to_string(),
            &Type::Function(ref _args, ref ret) => {
                format!("(result {})", ret.wast_name())
            }
            &Type::Record(..) => {
                "i32".to_string()
            },
            _ => panic!()
        }
    }

    /// Get the name of this type in WAST.
    pub fn trait_impl_name(&self) -> String {
        match self {
            &Type::i32 => "i32".to_string(),
            &Type::i64 => "i64".to_string(),
            &Type::f32 => "f32".to_string(),
            &Type::f64 => "f64".to_string(),
            &Type::ui32 => "ui32".to_string(),
            &Type::ui64 => "ui64".to_string(),
            &Type::boolean => "boolean".to_string(),
            &Type::empty => panic!(),
            &Type::Function(..) => panic!(),
            &Type::Record(..) => panic!("TODO: handle trait_impl_name for Record."),
            &Type::Sum(..) => panic!("TODO: handle trait_impl_name for Sum."),
            &Type::Product(..) => panic!("TODO: handle trait_impl_name for Product."),
            &Type::Vector(..) => panic!("TODO: handle trait_impl_name for Vector."),
            &Type::Named(ref name) => name.name.clone(),
            _ => panic!()
        }
    }

    /// Get _s or _u for signed values, otherwise an empty string.
    pub fn sign(&self) -> String {
        match &self {
            &Type::i32 | &Type::i64 => "_s".to_string(),
            &Type::ui32 | &Type::ui64 => "_u".to_string(),
            _ => "".to_string()
        }
    }

    /// Check if a type is a refinement type.
    pub fn is_simple(&self) -> bool {
        return match self {
            Type::Refinement(..) => false,
            _ => true
        };
    }

    /// Check if a type is a gradual type.
    /// Both pure gradual types and refinement types containing gradual types count.
    pub fn is_gradual(&self) -> bool {
        return match self {
            Type::Gradual(..) => true,
            Type::Refinement(ref inner_t, _) => inner_t.is_gradual(),
            _ => false
        };
    }

    /// Merge two types if they're compatible.
    pub fn merge(&self, other: &Type) -> Type {
        if self == other {
            return self.clone()
        } else {
            return match self {
                Type::Sum(ref types) => match other {
                    Type::Sum(ref other_types) => {
                        Type::Sum(general_utils::vec_c_int(types, other_types))
                    }, 
                    x => {
                        if types.contains(&x) {
                            x.clone()
                        } else {
                            panic!()
                        }
                    }
                },
                Type::Refinement(ref base, ..) => {
                    other.merge(&base)
                },
                Type::Undetermined => {
                    other.clone()
                },
                x => {
                    match other {
                        Type::Sum(ref other_types) => {
                            if other_types.contains(&x) {
                                x.clone()
                            } else {
                                panic!()
                            }
                        }, 
                        y => panic!("Type error. Tried to merge {:?} and {:?}", x, y)
                    }
                }
            }
        }
    }

    /// Check if it is possible to convert from one type to the other
    pub fn is_compatible(&self, other: &Type) -> bool {
        if self == other {
            return true
        } else {
            return match &other {
                Type::Refinement(ref base, ..) => self.is_compatible(base),
                Type::Gradual(_) => true,
                _ => match self {
                    Type::Refinement(ref base, ..) => base.is_compatible(other),
                    Type::i32 => {
                        match other {
                            Type::i64 | Type::f64 => true,
                            _ => false
                        }
                    },
                    Type::f32 => {
                        match other {
                            Type::f64 => true,
                            _ => false
                        }
                    },
                    Type::Sum(ref types) => {
                        match other {
                            Type::Sum(_) => true, 
                            x => types.contains(&x)
                        }
                    }, 
                    Type::Undetermined => true,
                    x => {
                        match other {
                            Type::Sum(ref other_types) => other_types.contains(&x),
                            _ => false
                        }
                    }
                }
            }
        }
    }

    pub fn has_simple_conversion(&self, other: &Type) -> bool {
        return match self {
            Type::i32 => {
                match other {
                    Type::i64 | Type::f64 => true,
                    _ => false
                }
            },
            Type::f32 => {
                match other {
                    Type::f64 => true,
                    _ => false
                }
            },
            _ => false
        };
    }

    /// The number of words required to store a type in WASM memory.
    pub fn size(&self) -> usize {
        return match self {
            Type::i32 => 1,
            Type::i64 => 2,
            Type::f32 => 1,
            Type::f64 => 2,
            Type::ui32 => 2,
            Type::boolean => 1,
            Type::string => 1,
            Type::Vector(ref t) => t.size(),
            Type::Product(ref types) => types.iter().map(|x| x.size()).sum(),
            Type::Record(_, ref fields)  | Type::Module(_, ref fields) => fields.iter().map(|(_, t)| t.size()).sum(),
            _ => panic!()
        }
    }

    /// Return true if other can be restricted to self.
    pub fn super_type(&self, other: &Type) -> bool {
        return match self {
            Type::Sum(ref types) => {
                match other {
                    Type::Sum(ref type_vec) => general_utils::vec_subset(type_vec, types),
                    x => types.contains(x)
                }
            },
            _ => false
        };
    }

    pub fn has_attribute(&self, attribute: &Identifier) -> bool {
        return match self {
            Type::Record (_, attributes) | Type::Module(_, attributes) => {
                for (attr_name, attr_type) in attributes {
                    if attribute == attr_name {
                        return true;
                    }
                }
                return false;
            },
            _ => false
        };
    }

    pub fn resolve_attribute(&self, attribute: &Identifier) -> Type {
        return match self {
            Type::Record (_, attributes) | Type::Module(_, attributes) => {
                let mut t = None;

                for (attr_name, attr_type) in attributes {
                    if attribute == attr_name {
                        t = Some(attr_type.clone());
                    }
                }
                match t {
                    Some(attr_type) => attr_type,
                    None => panic!("Self: {:?}, attribute: {:?}", self, attribute)
                }
            },
            _ => panic!("The provided type doesn't have attributes.")
        };
    }

    pub fn all_attributes(&self) -> HashSet<Identifier> {
        return match self {
            Type::Record (_, attributes) | Type::Module(_, attributes) => {
                attributes.keys().cloned().collect::<HashSet<Identifier>>()
            },
            _ => HashSet::new()
        };
    }

    pub fn flatten_to_record(idents: &Vec<Identifier>, base: BTreeMap<Identifier, Type>) -> Type {
        let mut rec = Type::Record(base.keys().map(|x| x.clone()).collect(), base);
        for ident in idents[1..].iter().rev() {
            let mut map = BTreeMap::new();
            let mut order = vec!();
            map.insert(ident.clone(), rec);
            order.push(ident.clone());
            rec = Type::Record(order, map);
        }
        return rec;
    }

    pub fn flatten_to_module(idents: &Vec<Identifier>, base: BTreeMap<Identifier, Type>) -> Type {
        let mut rec = Type::Module(base.keys().map(|x| x.clone()).collect(), base);
        for ident in idents[1..].iter().rev() {
            let mut map = BTreeMap::new();
            let mut order = vec!();
            map.insert(ident.clone(), rec);
            order.push(ident.clone());
            rec = Type::Module(order, map);
        }
        return rec;
    }

    pub fn resolve_nested_record(&self, idents: &Vec<Identifier>) -> Type {
        let mut t = self.clone();
        for ident in idents {
            t = t.resolve_attribute(ident);
        }
        return t.clone();
    }

    pub fn resolve_slice(&self, slices: &Vec<(Option<Type>, Option<Type>, Option<Type>)>) -> Type {
        return match self {
            Type::Vector(ref t) => match slices.get(0).unwrap() {
                (Some(_x), None, None) => (**t).clone(),
                _ => panic!()
            },
            _ => panic!()
        };
    }

    pub fn identifier_to_index(&self, ident: &Identifier) -> usize {
        return match self {
            Type::Record(ref order, ref fields) => {
                let mut words = 0;
                for i in order {
                    if i == ident {
                        break;
                    } else {
                        let size = fields.get(&i).unwrap().size();
                        words += size;
                    }
                }
                words
            },
            _ => panic!()
        };
    }

    pub fn get_constructor_type(&self) -> (Vec<(Identifier, Type)>, Type) {
        return match &self {
            Type::Record(_, ref fields) => {
                let args: Vec<(Identifier, Type)> = fields.clone().into_iter().collect();
                (args, Type::i32)
            },
            _ => panic!()
        }
    }

    /// Add a constraint if the type is a refinement. Do nothing otherwise.
    pub fn add_constraint(&self, name: &Identifier, expr: &Node<Expr>) -> Type {
        return match self {
            Type::Refinement(ref base, ref constraints) => {
                let mut new_constraints = constraints.clone();
                new_constraints.push(Refinement{
                    operator: ComparisonOperator::Equal,
                    left: Box::new(Node{
                        id: general_utils::get_next_id(),
                        scope: expr.scope,
                        data: Expr::from(name.clone())
                    }),
                    right: Box::new(expr.clone())
                });

                Type::Refinement(base.clone(), new_constraints)
            },
            x => x.clone()
        }
    }
}

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


/// A single layer of scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scope {
    /// The id of the parent scope.
    pub parent_id: Option<usize>,
    // TODO: Consider replacing with an insertion ordered map.
    /// The identifiers declared in this scope, and raw pointers to the statements that created them.
    pub declarations: BTreeMap<Identifier, CanModifyScope>,
    /// The order in which each identifier was declared. (Important for blocks.)
    pub declaration_order: BTreeMap<Identifier, usize>,
    // The trait we're in, if we're in one.
    pub maybe_trait: Option<Identifier>,
    // The struct we're in, if we're in one.
    pub maybe_struct: Option<Identifier>,
}

impl Scope {
    /// Create an empty scope.
    pub fn empty() -> Scope {
        return Scope{
            parent_id: None,
            declarations: BTreeMap::new(),
            declaration_order: BTreeMap::new(),
            maybe_trait: None,
            maybe_struct: None
        };
    }

    /// Create a child of the given parent
    pub fn child(parent_id: usize) -> Scope {
        return Scope{
            parent_id: Some(parent_id),
            declarations: BTreeMap::new(),
            declaration_order: BTreeMap::new(),
            maybe_trait: None,
            maybe_struct: None
        };
    }

    /// Create a child of the given parent with a struct
    pub fn child_struct_impl(parent_id: usize, structname: &Identifier) -> Scope {
        return Scope{
            parent_id: Some(parent_id),
            declarations: BTreeMap::new(),
            declaration_order: BTreeMap::new(),
            maybe_trait: None,
            maybe_struct: Some(structname.clone())
        };
    }

    /// Create a child of the given parent with a trait and a struct
    pub fn child_trait_impl(parent_id: usize, structname: &Identifier, traitname: &Identifier) -> Scope {
        return Scope{
            parent_id: Some(parent_id),
            declarations: BTreeMap::new(),
            declaration_order: BTreeMap::new(),
            maybe_trait: Some(traitname.clone()),
            maybe_struct: Some(structname.clone())
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

/// Generate a single binary trait.
fn binary_trait(trait_name: Identifier, method_name: Identifier) -> Trait {
    return Trait {
        name: trait_name,
        functions: hashmap!{
            method_name => Type::Function(
                vec!(
                    (Identifier::from("left"), Type::self_type(Box::new(Type::Undetermined))),
                    (Identifier::from("right"), Type::self_type(Box::new(Type::Undetermined)))
                ),
                Box::new(Type::self_type(Box::new(Type::Undetermined)))
            )
        }
    };
}

fn builtin_binary_names() -> Vec<(Identifier, Identifier)> {
    return vec!(("Add", "add"), ("Sub", "sub"), ("Mul", "mul"), ("Div", "div")).into_iter()
        .map(|(a, b)| (Identifier::from(a), Identifier::from(b))).collect();
}

/// Generate all the builtin binary traits.
fn builtin_binary_traits() -> HashMap<Identifier, Trait> {
    let mut traits = HashMap::new();
    for (tn, mn) in builtin_binary_names().into_iter() {
        let trt = binary_trait(tn.clone(), mn);
        traits.insert(tn, trt);
    }
    return traits;
}

/// Generate all the builtin trait implementations
fn builtin_trait_implementations() -> HashMap<(Identifier, Type), HashMap<Identifier, Type>> {
    let mut impls = HashMap::new();
    for (tn, mn) in builtin_binary_names().into_iter() {
        for t in vec!(Type::i32, Type::i64, Type::f32, Type::f64) {
            let func_t = Type::Function(
                vec!(
                    (Identifier::from("left"), t.clone()),
                    (Identifier::from("right"), t.clone())
                ),
                Box::new(t.clone())
            );
            let func_types = hashmap!{mn.clone() => func_t};
            impls.insert((tn.clone(), t), func_types);
        }
    }
    return impls;
}

/// Constructors
impl Context {

    /// Create a context containing all builtin types and functions.
    pub fn builtin() -> (usize, Context) {
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
            traits: builtin_binary_traits(),
            trait_implementations: builtin_trait_implementations()
        };
        return (id, context);
    }

    /// Create a context that contains only an empty scope.
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

    /// Create a new Context.
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
}

/// Basic methods
impl Context {
    /// Get a scope by its ID.
    pub fn get_scope(&self, scope_id: usize) -> &Scope {
        return self.scopes.get(&scope_id).unwrap();
    }

    /// Get a mutable reference to a Scope.
    pub fn get_mut_scope(&mut self, scope_id: usize) -> &mut Scope {
        return self.scopes.get_mut(&scope_id).unwrap();
    }

    /// Record the type of a node.
    pub fn add_type(&mut self, id: usize, t: Type) {
        self.type_map.insert(id, t);
    }

    /// Define a named type.
    pub fn define_type(&mut self, name: Identifier, t: Type) {
        self.defined_types.insert(name.clone(), t);
    }

    /// Get the type associated with a particular name.
    pub fn get_defined_type(&self, name: &Identifier) -> Type {
        return self.defined_types.get(name).unwrap().clone();
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
}

/// Scoping
impl Context {

    /// Create a new scope, returning the ID.
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

    pub fn get_struct_and_trait(&self, scope_id: usize) -> (Option<Identifier>, Option<Identifier>) {
        let initial_scope = self.scopes.get(&scope_id).unwrap();
        return if initial_scope.maybe_struct.is_some() || initial_scope.maybe_trait.is_some() {
            (initial_scope.maybe_struct.clone(), initial_scope.maybe_trait.clone())
        } else {
            match initial_scope.parent_id {
                Some(id) => self.get_struct_and_trait(id),
                None => (None, None)
            }
        };
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

    /// Resolve an attribute access within the current context.
    /// # Arguments:
    pub fn resolve_attribute(&self, base_type: &Type, name: &Identifier) -> Type {

        // Check if this is a direct attribute access
        let unwrapped_self = match base_type {
            Type::self_type(x) => *x.clone(),
            x => x.clone()
        };
        let attribute_type = match &unwrapped_self {
            Type::Record(_, ref attributes) => attributes.get(name),
            Type::Named(ref t_name) => {
                let record_t = self.defined_types.get(t_name).unwrap();
                match record_t {
                    Type::Record(_, ref attributes) => attributes.get(name),
                    _ => None
                }
            },
            _ => None
        };

        return match attribute_type {
            Some(x) => x.clone(),
            None => {
                // Check if this is a trait access
                let mut possible_traits = vec!();

                for (trait_name, trait_struct) in self.traits.iter() {
                    // Check if this trait has a function with the desired name.
                    if trait_struct.functions.contains_key(name) {
                        // Check if base_type implements this trait.
                        // OPT: We shouldn't be cloning these things. Everything's staying a reference.
                        if self.trait_implementations.contains_key(&(trait_name.clone(), unwrapped_self.clone())) {
                            possible_traits.push(trait_struct);
                        }
                    }
                }

                // Just resolve the trait.
                if possible_traits.len() == 1 {
                    // Get the type of the trait function and return it.
                    return possible_traits[0].functions.get(name).unwrap().clone();
                }// TODO: Handle ambiguous traits.
                else if possible_traits.len() > 1 {
                    panic!("ATTRIBUTE ERROR: Ambiguous trait method call. Base type {:?} call to {:?} could reference any of {:?}.", base_type, name, possible_traits);
                } else {
                    panic!("ATTRIBUTE ERROR: No matching attribute found for: {:?}, {:?}", base_type, name);
                }
            }
        }
    }

    /// Modify a Type::Self so it contains whatever Self actually is.
    pub fn resolve_self_type(&self, base_type: &Type, scope_id: usize) -> Type {
        return match base_type {
            Type::self_type(t) => {
                assert!(matches!(**t, Type::Undetermined), "TYPE ERROR: Matching against a self type that is not undetermined: {:?}", base_type);
                let (struct_name, trait_name) = self.get_struct_and_trait(scope_id);
                match struct_name {
                    Some(x) => Type::self_type(Box::new(Type::Named(x))),
                    None => panic!("TYPE ERROR: Self used outside of a method implementation.")
                }
            },
            t => t.clone()
        };
    }

    /// Check if this is a trait method call or an attribute access.
    pub fn trait_information(&self, base_type: &Type, name: &Identifier) -> Option<Identifier> {

        let mut possible_traits = vec!();

        for (trait_name, trait_struct) in self.traits.iter() {
            // Check if this trait has a function with the desired name.
            if trait_struct.functions.contains_key(name) {
                // Check if base_type implements this trait.
                // OPT: We shouldn't be cloning these things. Everything's staying a reference.
                if self.trait_implementations.contains_key(&(trait_name.clone(), base_type.clone())) {
                    possible_traits.push(trait_struct);
                }
            }
        }

        return match possible_traits.len() {
            0 => None,
            1 => Some(possible_traits[0].name.clone()),
            _ => panic!("Ambiguous trait")
        };
    }

    /// Check that a trait method call is valid, and get the return type.
    pub fn check_trait_method_call(&self, trait_name: &Identifier, method_name: &Identifier, implementing_type: &Type, arg_types: Vec<&Type>) -> Type {
        let func_types = self.trait_implementations.get(&(trait_name.clone(), implementing_type.clone()))
            .expect("TYPE ERROR: No trait implementation found.");
        let method_type = func_types.get(method_name).unwrap();
        return match method_type {
            Type::Function(ref args, ref return_type) => {

                for ((_, expected_t), actual_t) in args.iter().zip(arg_types.iter()) {
                    assert_eq!(&expected_t, actual_t);
                }

                *return_type.clone()
            },
            x => panic!("TYPE ERROR: Non-function type for a trait method. Trait and method are: {:?} and {:?}. Type is: {:?}", trait_name, method_name, x)
        }
    }

    /// Get the return type of a binary operator.
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

    /// Update a gradual type with a new constraint.
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
            let unwrapped_self = match expr_t {
                Type::self_type(x) => (**x).clone(),
                x => x.clone()
            };
            return match desired_type {
                Type::Undetermined => true,
                Type::empty => false,
                Type::i32 | Type::i64 | Type::f32  | Type::f64 | Type::boolean | Type::string => match unwrapped_self {
                    Type::Refinement(ref base, ..) => self.check_subtype(expr, base, desired_type),
                    Type::Gradual(ref id) => self.update_gradual(*id, desired_type),
                    x => x.has_simple_conversion(desired_type)
                },
                Type::Product(ref types) => match &unwrapped_self {
                    Type::Product(ref expr_types) => expr_types.iter().enumerate().all(|(i, x)| self.check_subtype(expr, x, &types[i])),
                    x => panic!("TYPE ERROR: Can't get {:?} from {:?}", &desired_type, x)
                },
                Type::Function(ref args, ref ret) => match &unwrapped_self {
                    Type::Function(ref e_args, ref e_ret) => {
                        let args_match = args.iter().enumerate().all(|(i, x)| self.check_subtype(expr, &e_args[i].1, &x.1));
                        args_match && self.check_subtype(expr, ret, e_ret)
                    },
                    x => panic!("TYPE ERROR: Can't get {:?} from {:?}", &desired_type, x)
                },
                Type::Vector(ref t) => match &unwrapped_self {
                    Type::Vector(ref e_t) => self.check_subtype(expr, e_t, t),
                    x => panic!("TYPE ERROR: Can't get {:?} from {:?}", &desired_type, x)
                },
                Type::Refinement(_, ref d_conds) => check_constraints(expr.scope, self, d_conds.clone()),
                Type::Gradual(ref id) => self.update_gradual(*id, &unwrapped_self),
                Type::Named(..) => desired_type == &unwrapped_self,
                Type::self_type(x) => self.check_subtype(expr, &unwrapped_self, x),
                x => panic!("TYPE ERROR: We don't handle subtyping for {:?}.", x)
            };
        }
    }

}


pub trait GetContext {

    /// Compute the scopes and types for an AST node.
    fn scopes_and_types(&mut self, parent_id: usize, context: Context) -> (Context, Type);

    /// Get all *non-Argument* declarations.
    fn get_true_declarations(&self, context: &Context) -> HashSet<(Identifier, Type)>;
}

impl GetContext for Node<Module> {

    fn scopes_and_types(&mut self, parent_id: usize, mut context: Context) -> (Context, Type) {
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
            new_context = stmt.scopes_and_types(scope_id, new_context).0;
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
                let res = dec.scopes_and_types(impl_scope_id, new_context);
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
                    x => panic!("TYPE ERROR: Somehow got a non function type.")
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
            new_context = stmt.scopes_and_types(scope_id, new_context).0;
        }

        return (new_context, Type::empty);
    }

    fn get_true_declarations(&self, context: &Context) -> HashSet<(Identifier, Type)> {
        let top_level: HashSet<Identifier> = context.get_scope(self.scope).declarations.keys().map(|x| x.clone()).collect();
        let mut with_types = top_level.into_iter().map(|x| {
            let t = context.get_type(self.scope, &x);
            (x, t)
        }).collect();
        for stmt in &self.data.functions {
            with_types = general_utils::m_union(with_types, stmt.get_true_declarations(context));
        }
        for stmt in &self.data.structs {
            with_types = general_utils::m_union(with_types, stmt.get_true_declarations(context));
        }
        return with_types;
    }
}

impl GetContext for Node<Block> {
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
    
    fn scopes_and_types(&mut self, parent_id: usize, mut context: Context) -> (Context, Type) {
        self.scope = parent_id;
        let (mut final_c, final_t) = match self.data {
            Stmt::LetStmt{ref mut expression, ref type_annotation, ref mut name} => {
                let (mut c, t) = expression.scopes_and_types(parent_id, context);
                match &type_annotation {
                    Some(ref x) => {
                        let actual_type = c.resolve_self_type(x, self.scope);
                        assert!(c.check_subtype(&expression, &t, &actual_type));
                    },
                    None => {}
                };

                (c, t)
            },
            Stmt::AssignmentStmt{ref mut expression, ref mut name} => {
                let (mut c, t) = expression.scopes_and_types(parent_id, context);
                let expected_type = c.get_type(self.scope, name);
                assert!(c.check_subtype(&expression, &t, &expected_type));
                (c, t)
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
                    let res = val.scopes_and_types(parent_id, context);
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

                let (block_context, block_type) = block.scopes_and_types(scope_id, context);

                assert!(return_type.is_compatible(&block_type), "{:?} not compatible with {:?}", return_type, block_type);

                (block_context, function_type)
            },
            Stmt::WhileStmt{ref mut condition, ref mut block} => {
                let (mut condition_context, condition_type) = condition.scopes_and_types(parent_id, context);
                assert_eq!(condition_type, Type::boolean);

                let block_scope = Scope::child(parent_id);

                let scope_id = condition_context.new_scope(block_scope);
                block.scopes_and_types(scope_id, condition_context)
            },
            Stmt::IfStmt{ref mut condition, ref mut block, ref mut else_block} => {
                let (mut new_context, condition_type) = condition.scopes_and_types(parent_id, context);
                assert_eq!(condition_type, Type::boolean);

                let block_scope = Scope::child(parent_id); 
                let scope_id = new_context.new_scope(block_scope);
                let res = block.scopes_and_types(scope_id, new_context);
                new_context = res.0;
                let mut if_type = res.1;

                match else_block {
                    Some(b) => {
                        let else_scope = Scope::child(parent_id);
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
                let record = Type::Record(order, records);
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

                let (trait_name, method_name) = operator.get_builtin_trait();
                if !left_t.is_gradual() || !right_t.is_gradual() {
                    let return_type = right_c.check_trait_method_call(&trait_name, &method_name, &left_t, vec!(&left_t, &right_t));
                }

                // let return_type = right_c.bin_op_ret_type(operator, &left_t, &right_t);
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
                println!("Start of StructLiteral: Base is {:?}", base);
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
                println!("Start of AttributeAccess: Base is {:?}", base);
                let attr_t = new_c.resolve_attribute(&base_t, attribute);
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

pub fn get_convert_expr(from: &Type, to: &Type, base: Node<Expr>, context: &mut Context) -> Node<Expr> {
    return match from == to {
        false => {
            let operator = UnaryOperator::cast(&from, &to);
            let raw_expr = Expr::UnaryExpr{operator: operator, operand: Box::new(base)};
            let new_node = Node::from(raw_expr);
            context.add_type(new_node.id, to.clone());
            new_node
        },
        true => base
    };
}

pub fn choose_return_type(possible: &Type) -> Type {
    return match possible {
        Type::Sum(ref types) => {
            let mut i = 0;
            let mut min_size = usize::MAX;
            for (j, t) in types.iter().enumerate() {
                if t.size() < min_size {
                    i = j;
                    min_size = t.size();
                }
            }
            types[i].clone()
        },
        _ => possible.clone()
    }
}

impl BinaryOperator {

    pub fn get_return_types(&self, left: &Type, right: &Type) -> Type {
        //let mut intersection = HashSet::new();
        return match self {
            BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mult | BinaryOperator::Mod => left.merge(right),
            BinaryOperator::Div => Type::f64,
            BinaryOperator::And | BinaryOperator::Or | BinaryOperator::Xor => Type::boolean,
            _ => panic!()
        }
    }

    pub fn choose_return_type(&self, merged_type: &Type) -> Type {

        return match self {
            BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mult | BinaryOperator::Mod => 
            choose_return_type(merged_type),
            BinaryOperator::Div => Type::f32,
            _ => panic!()   
        };
    }

    pub fn requires_sign(&self) -> bool {
        match self {
            BinaryOperator::Div => true,
            _ => false
        }
    }

    /// Return the index of the corresponding gradual function in the gradual function table.
    pub fn gradual_index(&self) -> i32 {
        return match self {
            BinaryOperator::Add => 0,
            BinaryOperator::Sub => 1,
            BinaryOperator::Mult => 2,
            _ => panic!()
        };
    }

    pub fn get_builtin_trait(&self) -> (Identifier, Identifier) {
        let (x, y) = match self {
            BinaryOperator::Add => ("Add", "add"),
            _ => panic!()
        };

        return (Identifier::from(x), Identifier::from(y));
    }
}

impl UnaryOperator {
    fn cast(from: &Type, to: &Type) -> UnaryOperator {
        let possible = from.is_compatible(to);

        if possible {
            return UnaryOperator::Convert(from.clone(), to.clone());
        } else {
            panic!("Cannot cast from {:?} to {:?}", from, to);
        }
    }
}

impl <'a> From<&'a Identifier> for Type {
    fn from(input: &'a Identifier) -> Self {
        return match input.name.as_ref() {
            "i32" => Type::i32,
            "i64" => Type::i64,
            "f32" => Type::f32,
            "f64" => Type::f64,
            "ui32" => Type::ui32,
            "ui64" => Type::ui64,
            "boolean" => Type::boolean,
            "string" => Type::string,
            "any" => Type::Gradual(general_utils::get_next_grad()),
            _ => Type::Named(input.clone())
        };
    }
}

impl From<Identifier> for Type {
    fn from(input: Identifier) -> Self {
        return Type::from(&input);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use compiler_layers;
    use difference::{Difference, Changeset};
    use regex::Regex;
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
        let compilation = compiler_layers::Compilation::compile(
            &file_name);
        let compiled_module = compilation.modules.get(&"basic_grace".to_string()).unwrap();
        let first_func_id = compiled_module.ast.data.functions.get(0).unwrap().id;
        let actual_type = compiled_module.context.type_map.get(&first_func_id).unwrap();
        let expected_type = Type::Function(vec!((Identifier::from("arg"), Type::i32)), Box::new(Type::i32));
        assert_eq!(&expected_type, actual_type);
    }

    #[test]
    fn test_get_declarations() {
        let (func_dec, context) = compiler_layers::to_context::<Node<Stmt>>("fn a(b: i32) -> i32:\n let x = 5 + 6\n return x\n".as_bytes());
        // let new_ident = Identifier::from("x");
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

    #[test]
    // One trait, one struct, one implmentation block  that uses self, and a function that uses it
    fn traits_and_self() {
        let mut f = File::open("test_data/trait_impl_self_test.gr").expect("File not found");
        let mut file_contents = String::new();
        f.read_to_string(&mut file_contents).unwrap();

        let compilation = compiler_layers::to_context::<Node<Module>>(file_contents.as_bytes());

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

        // impl Node<Stmt> {
        //     fn mod_scope(&mut self, new_scope: usize) {
        //         self.scope = new_scope;
        //         match self.data {
        //             Stmt::LetStmt{ref mut expression, ..} => {
        //                 expression.mod_scope(new_scope);
        //             }
        //             _ => {}
        //         }
        //     }
        // }

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
                    let (id, mut context) = Context::builtin();
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
