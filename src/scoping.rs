use std::collections::BTreeMap;
use std::collections::HashSet;
use std::collections::BTreeSet;
use std::collections::HashMap;
use expression::*;
use general_utils;
use typing::{
    Type,
    Numeric,
    FloatingPoint,
};


/// The full scoping context for a compilation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Context {
    // A map from Scope ids to Scopes.
    pub scopes: HashMap<usize, Scope>,
    // A map from Node ids to Scope ids. Each node that modifies scope
    // maps to the scope it's contained in.
    pub containing_scopes: HashMap<usize, usize>
}

/// The full scoping and typing context for a compilation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Context2 {
    // The ID of the root scope of the context.
    pub root_id: usize,
    // A map from Scope IDs to Scopes.
    pub scopes: HashMap<usize, Scope2>,
    // A map from Node IDs to Scope IDs. Each node that modifies scope
    // maps to the scope it's contained in.
    pub containing_scopes: HashMap<usize, usize>,
    // A map from Node IDs to types.
    pub type_map: HashMap<usize, Type>, 
    // The user-defined types
    pub defined_types: HashMap<String, Type>
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
    Expression(*const Node<Expr>, usize),
    Argument(Identifier, usize),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scope2 {
    /// The id of the parent scope.
    pub parent_id: Option<usize>,
    // TODO: Consider replacing with an insertion ordered map.
    /// The identifiers declared in this scope, and raw pointers to the statements that created them.
    pub declarations: BTreeMap<Identifier, usize>,
    /// The order in which each identifier was declared. (Important for blocks.)
    pub declaration_order: BTreeMap<Identifier, usize>
}

/// Any object that has a scope.
pub trait Scoped {
    /// Get the scope of the object.

    fn get_usages(&self) -> HashSet<Identifier>;

    /// Get all *non-Argument* declarations.
    fn get_true_declarations(&self, context: &Context) -> BTreeSet<Identifier>;

    /// Generate scopes recursively. Returns all scopes.
    fn gen_scopes(&mut self, parent_id: usize, context: &Context) -> Context;
}

pub trait GetContext {
    fn scopes_and_types(&mut self, parent_id: usize, mut context: Context2) -> (Context2, Type);
}

/// Create an empty scope.
pub fn base_scope() -> Scope {
    return Scope{
        parent_id: None,
        declarations: BTreeMap::new(),
        declaration_order: BTreeMap::new()
    };
}

pub fn base_scope2() -> Scope2 {
    return Scope2{
        parent_id: None,
        declarations: BTreeMap::new(),
        declaration_order: BTreeMap::new()
    };
}

/// Create a context containing only the empty scope.
pub fn initial_context() -> (usize, Context) {
    let empty = base_scope();
    let mut init_scopes = HashMap::new();
    let id = general_utils::get_next_scope_id();
    init_scopes.insert(id, empty);
    let context = Context{scopes: init_scopes, containing_scopes: HashMap::new()};
    return (id, context);
}

pub fn builtin_context() -> Context2 {
    let empty = base_scope2();
    let mut init_scopes = HashMap::new();
    let id = general_utils::get_next_scope_id();
    init_scopes.insert(id, empty);
    let context = Context2 {
        root_id: id, 
        scopes: init_scopes, 
        containing_scopes: HashMap::new(),
        type_map: HashMap::new(), 
        defined_types: HashMap::new()
    };
    return context;
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
}

impl Context2 {
    pub fn empty() -> Context2 {
        return Context2{
            root_id: general_utils::get_next_scope_id(),
            scopes: HashMap::new(),
            containing_scopes: HashMap::new(),
            type_map: HashMap::new(),
            defined_types: HashMap::new()
        };
    }

    /// Record the type of a node.
    pub fn add_type(&mut self, id: usize, t: Type) {
        self.type_map.insert(id, t);
    }
}

impl Context {
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

    pub fn get_declaration(&self, scope_id: usize, name: &Identifier) -> Option<&CanModifyScope> {
        let initial_scope = self.scopes.get(&scope_id).unwrap();
        if scope_id == 0 {
            panic!("Reached scope id 0 searching for {}", name);
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

    pub fn get_type(&self, scope_id: usize, name: &Identifier, type_map: &HashMap<usize, Type>) -> Type {
        let scope_mod = self.get_declaration(scope_id, name).unwrap();
        let t = unsafe {
            match scope_mod {
                CanModifyScope::Statement(ptr, _) => {
                    type_map.get(&(**ptr).id).unwrap().clone()
                },
                _ => panic!()
            }
        };
        return t;
    }

    /// Extend this context with another one.
    pub fn extend(&mut self, other_context: Context) {
        for (id, scope) in other_context.scopes.into_iter() {
            if self.scopes.contains_key(&id) {
                // TODO: Make this a real error.
                panic!("Duplicate scope IDs.\n ID: {}.\n Existing: {:?}.\n Replacement: {:?}", id, self.scopes.get(&id), scope);
            } else {
                self.scopes.insert(id, scope);
            }
        }
    }

    pub fn empty() -> Context {
        return Context{scopes: HashMap::new(), containing_scopes: HashMap::new()};
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
            CanModifyScope::Expression(ref _ptr, ref id) => *id,
            CanModifyScope::Argument(ref _name, ref id) => *id,
            CanModifyScope::ImportedModule(ref id) => *id
        };
    }
}

impl Scoped for Node<Module> {

    fn get_usages(&self) -> HashSet<Identifier> {
        panic!();
    }

    fn get_true_declarations(&self, context: &Context) -> BTreeSet<Identifier> {
        let mut top_level: BTreeSet<Identifier> = context.get_scope(self.scope).declarations.keys().map(|x| x.clone()).collect();
        for stmt in &self.data.declarations {
            top_level = general_utils::mb_union(top_level, stmt.get_true_declarations(context));
        }
        return top_level;
    }

    fn gen_scopes(&mut self, parent_id: usize, context: &Context) -> Context {
        let declarations = BTreeMap::new();
        let declaration_order = BTreeMap::new();

        let new_scope = Scope{parent_id: Some(parent_id), declarations, declaration_order};
        let scope_id = general_utils::get_next_scope_id();
        self.scope = scope_id;

        let mut new_context = Context::empty();
        new_context.add_scope(scope_id, new_scope);

        for stmt in self.data.declarations.iter_mut() {
            let child_context = stmt.gen_scopes(scope_id, context);
            new_context.extend(child_context);
        }

        self.update_scope(&mut new_context);

        return new_context;
    }
}

impl Scoped for Node<Block> {

    fn get_usages(&self) -> HashSet<Identifier> {
        let mut usages = HashSet::new();
        for stmt in &self.data.statements {
            usages = general_utils::m_union(usages, stmt.get_usages());
        }
        return usages;
    }

    fn get_true_declarations(&self, context: &Context) -> BTreeSet<Identifier> {
        let mut top_level: BTreeSet<Identifier> = context.get_scope(self.scope).declarations.keys().map(|x| x.clone()).collect();
        for stmt in &self.data.statements {
            top_level = general_utils::mb_union(top_level, stmt.get_true_declarations(context));
        }
        return top_level;
    }

    fn gen_scopes(&mut self, parent_id: usize, context: &Context) -> Context {
        let declarations = BTreeMap::new();
        let declaration_order = BTreeMap::new();

        let new_scope = Scope{parent_id: Some(parent_id), declarations, declaration_order};
        let scope_id = general_utils::get_next_scope_id();
        self.scope = scope_id;
        let mut new_context = Context::empty();
        new_context.add_scope(scope_id, new_scope);
        // Compute the child contexts.
        for stmt in self.data.statements.iter_mut() {
            let child_context = stmt.gen_scopes(scope_id, context);
            new_context.extend(child_context);
        }

        self.update_scope(&mut new_context);

        return new_context;
    }
}

impl Scoped for Node<Stmt> {

    fn get_usages(&self) -> HashSet<Identifier> {
        return match self.data {
            Stmt::IfStmt{ref condition, ref block, ref elifs, ref else_block} => {
                let mut usages = general_utils::m_union(condition.get_usages(), block.get_usages());
                for (cond, blck) in elifs {
                    usages = general_utils::m_union(usages, cond.get_usages());
                    usages = general_utils::m_union(usages, blck.get_usages());
                }
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

    fn get_true_declarations(&self, context: &Context) -> BTreeSet<Identifier> {
        return match self.data {
            Stmt::FunctionDecStmt{ref block, ..} => {
                block.get_true_declarations(context)
            },
            _ => BTreeSet::new()
        };
    }

    fn gen_scopes(&mut self, parent_id: usize, context: &Context) -> Context {
        let new_context = match &mut self.data {
            Stmt::LetStmt{expression, ..} => {
                self.scope = parent_id;
                expression.gen_scopes(parent_id, context)
            },
            Stmt::AssignmentStmt{expression, ..} => {
                self.scope = parent_id;
                expression.gen_scopes(parent_id, context)
            },
            Stmt::FunctionDecStmt{args, ref mut block, ..} => {
                // TODO: Handle keyword args expressions. They should receive just the parent scope.
                let mut declarations = BTreeMap::new();
                let mut declaration_order = BTreeMap::new();

                // Add arguments to declarations.
                for (i, arg) in args.iter().enumerate() {
                    declaration_order.insert(arg.0.clone(), i+1);
                    declarations.insert(arg.0.clone(), CanModifyScope::Argument(arg.0.clone(), self.id));
                }
                
                let mut new_context = Context::empty();
                let new_scope = Scope{parent_id: Some(parent_id), declarations, declaration_order};
                let scope_id = new_context.new_scope(new_scope);
                self.scope = scope_id;

                let block_context = block.gen_scopes(scope_id, context);
                new_context.extend(block_context);
                new_context
            },
            Stmt::ReturnStmt(expression) => {
                self.scope = parent_id;
                expression.gen_scopes(parent_id, context)
            },
            Stmt::WhileStmt{condition, block} => {
                let mut condition_context = condition.gen_scopes(parent_id, context);
                let block_context = block.gen_scopes(parent_id, context);
                condition_context.extend(block_context);
                self.scope = parent_id;
                condition_context
            },
            Stmt::IfStmt{condition, block, elifs, else_block} => {
                let mut condition_context = condition.gen_scopes(parent_id, context);
                let block_context = block.gen_scopes(parent_id, context);
                condition_context.extend(block_context);

                for (elif_cond, elif_block) in elifs {
                    condition_context.extend(elif_cond.gen_scopes(parent_id, context));
                    condition_context.extend(elif_block.gen_scopes(parent_id, context));
                }

                match else_block {
                    Some(b) => condition_context.extend(b.gen_scopes(parent_id, context)),
                    None => {}
                };

                self.scope = parent_id;
                condition_context
            },
            Stmt::StructDec{..} => {
                self.scope = parent_id;
                Context::empty()

            },
            _ => panic!()
        };

        return new_context;
    }
}

impl Scoped for Node<Expr> {

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

    fn get_true_declarations(&self, _context: &Context) -> BTreeSet<Identifier> {
        panic!()
    }

    fn gen_scopes(&mut self, parent_id: usize, context: &Context) -> Context {
        let new_context = match &mut self.data {
            Expr::ComparisonExpr{ref mut left, ref mut right, ..} => {
                let mut new_context = Context::empty();
                new_context.extend(left.gen_scopes(parent_id, context));
                new_context.extend(right.gen_scopes(parent_id, context));
                self.scope = parent_id;
                new_context
            },
            Expr::BinaryExpr{ref mut left, ref mut right, ..} => {
                let mut new_context = Context::empty();
                new_context.extend(left.gen_scopes(parent_id, context));
                new_context.extend(right.gen_scopes(parent_id, context));
                self.scope = parent_id;
                new_context
            },
            Expr::FunctionCall{ref mut function, ref mut args, ref mut kwargs} => {
                let mut new_context = Context::empty();
                new_context.extend(function.gen_scopes(parent_id, context));
                for arg in args {
                    new_context.extend(arg.gen_scopes(parent_id, context));
                }
                for (_, kwarg) in kwargs {
                    new_context.extend(kwarg.gen_scopes(parent_id, context));
                }
                self.scope = parent_id;
                new_context
            },
            Expr::Index{ref mut base, ref mut slices} => {
                let mut new_context = Context::empty();
                new_context.extend(base.gen_scopes(parent_id, context));
                self.scope = parent_id;
                let first_slice = slices.get(0).unwrap().clone();
                let index_context = first_slice.0.unwrap().gen_scopes(parent_id, context);
                new_context.extend(index_context);
                new_context
            },
            Expr::Int(_) | Expr::Float(_) | Expr::Bool(_) | Expr::String(_) | Expr::IdentifierExpr(_) => {
                self.scope = parent_id;
                Context::empty()
            },
            Expr::VecLiteral(exprs) | Expr::SetLiteral(exprs) | Expr::TupleLiteral(exprs) => {
                let mut new_context = Context::empty();
                for expr in exprs {
                    new_context.extend(expr.gen_scopes(parent_id, context));
                }
                self.scope = parent_id;
                new_context
            },
            Expr::StructLiteral{ref mut base, ref mut fields} => {
                let mut new_context = Context::empty();
                new_context.extend(base.gen_scopes(parent_id, context));
                for field in fields {
                    new_context.extend(field.gen_scopes(parent_id, context));
                }
                self.scope = parent_id;
                new_context
            },
            Expr::AttributeAccess{ref mut base, ..} => {
                let mut new_context = Context::empty();
                new_context.extend(base.gen_scopes(parent_id, context));
                self.scope = parent_id;
                new_context
            }
            
            _ => panic!()
        };

        return new_context;
    }
}

impl GetContext for Node<Expr> {
    fn scopes_and_types(&mut self, parent_id: usize, mut context: Context2) -> (Context2, Type) {
        self.scope = parent_id;
        let (mut final_c, final_t) = match self.data {
            Expr::ComparisonExpr{ref operator, ref mut left, ref mut right} => {
                let (left_c, left_t) = left.scopes_and_types(parent_id, context);
                let (mut right_c, right_t) = right.scopes_and_types(parent_id, left_c);
                assert_eq!(left_t, right_t);
                (right_c, Type::boolean)
            },
            // TODO: Type checking
            Expr::BinaryExpr{ref operator, ref mut left, ref mut right} => {
                let (left_c, left_t) = left.scopes_and_types(parent_id, context);
                let (mut right_c, right_t) = right.scopes_and_types(parent_id, left_c);
                let return_type = operator.get_return_types(&left_t, &right_t);
                (right_c, return_type)
            },
            Expr::UnaryExpr{ref mut operand, ..} => {
                operand.scopes_and_types(parent_id, context)
            }
            // TODO: Type checking
            Expr::FunctionCall{ref mut function, ref mut args, ref mut kwargs} => {
                let (mut new_c, t) = function.scopes_and_types(parent_id, context);
                for arg in args {
                    let res = arg.scopes_and_types(parent_id, new_c);
                    new_c = res.0;
                    let _arg_t = res.1;
                }

                for (_, value) in kwargs {
                    let res = value.scopes_and_types(parent_id, new_c);
                    new_c = res.0;
                    let _kwarg_t = res.1;
                }
                (new_c, t)
            },
            // TODO: Type checking
            Expr::StructLiteral{ref mut base, ref mut fields} => {
                let (mut new_c, base_t) = base.scopes_and_types(parent_id, context);
                for field in fields {
                    let res = field.scopes_and_types(parent_id, new_c);
                    new_c = res.0;
                    let _field_t = res.1;
                    // TODO: Check field types.
                }
                (new_c, base_t.clone())
            },
            Expr::AttributeAccess{ref mut base, ref attribute} => {
                let (new_c, base_t) = base.scopes_and_types(parent_id, context);
                let attr_t = base_t.resolve_attribute(attribute);
                (new_c, attr_t)
            },
            Expr::ModuleAccess(ref mut names) => {
                panic!()
            },
            Expr::Index{ref mut base, ref mut slices} => {
                panic!()
            },
            Expr::Int(_) => {
                let t = Numeric();
                context.add_type(self.id, t.clone());
                (context, t)
            },
            Expr::Float(_) => {
                let t = FloatingPoint();
                context.add_type(self.id, t.clone());
                (context, t)
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
}

impl Node<Block> {
    /// Update a context with the declarations in this block.
    /// All declarations will be either functions or let statements.
    pub fn update_scope(&self, context: &mut Context) {

        let scope = context.get_mut_scope(self.scope);
        for (i, stmt) in self.data.statements.iter().enumerate() {
            // Make updates to the current scope based on the child nodes. 
            // Only function declarations and lets can modify scope.
            match &stmt.data {
                Stmt::FunctionDecStmt{ref name, ..} => {
                    scope.declaration_order.insert(name.clone(), i);
                    let scope_mod = CanModifyScope::Statement(stmt.as_ref() as *const _, stmt.id);
                    scope.declarations.insert(name.clone(), scope_mod);
                },
                Stmt::LetStmt{ref name, ..} => {
                    scope.declaration_order.insert(name.clone(), i);
                    let scope_mod = CanModifyScope::Statement(stmt.as_ref() as *const _, stmt.id);
                    scope.declarations.insert(name.clone(), scope_mod);
                },
                Stmt::StructDec{ref name, ..} => {
                    scope.declaration_order.insert(name.clone(), i);
                    let scope_mod = CanModifyScope::Statement(stmt.as_ref() as *const _, stmt.id);
                    scope.declarations.insert(name.clone(), scope_mod);
                },
                _ => {}
            };
        }

    }
}

impl Node<Module> {
    /// Update a context with the declarations in this module.
    /// All declarations will be either functions or import statements.
    pub fn update_scope(&self, context: &mut Context) {
        let scope = context.get_mut_scope(self.scope);
        for (i, dec) in self.data.declarations.iter().enumerate() {
            match &dec.data {
                Stmt::FunctionDecStmt{ref name, ..} => {
                    let scope_mod = CanModifyScope::Statement(dec.as_ref() as *const _, dec.id);
                    scope.declarations.insert(name.clone(), scope_mod);
                    scope.declaration_order.insert(name.clone(), i);
                },
                Stmt::StructDec{ref name, ..} => {
                    let scope_mod = CanModifyScope::Statement(dec.as_ref() as *const _, dec.id);
                    scope.declarations.insert(name.clone(), scope_mod);
                    scope.declaration_order.insert(name.clone(), i);
                }, 
                _ => panic!()
            };
        }
        
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use compiler_layers;

    #[test]
    fn test_function_locals() {
        let func_str = r#"fn a(b: i32, c: i32) -> i32:
        return b + c
        "#;
        let (func_stmt, _) = compiler_layers::to_scopes::<Node<Stmt>>(func_str.as_bytes());
        let usages = func_stmt.get_usages();
        assert!(usages.contains(&Identifier::from("b")));
        assert!(usages.contains(&Identifier::from("c")));
    }

    #[cfg(test)]
    mod scope_generation {
        use super::*;

        #[cfg(test)]
        mod exprs {
            use super::*;

            /// Test scope modifications from literals.
            /// All should be empty.
            #[test]
            fn test_literal_scope() {
                let mut literals: Vec<Node<Expr>> = vec![Node::from(1), Node::from(0.5), Node::from(Expr::String("asdf".to_string())), Node::from(true)];
                let (id, init) = initial_context();
                for literal in literals.iter_mut() {
                    let context = literal.gen_scopes(id, &init);
                    assert_eq!(context, Context::empty());
                }
            }
        }
        
        #[cfg(test)]
        mod stmts {
            use super::*;

            #[test]
            fn test_let_stmt() {
                let (block, context) = compiler_layers::to_scopes::<Node<Block>>("let a = 1".as_bytes());
                assert_eq!(context.scopes.len(), 2);
                let scope = context.get_scope(block.scope);
                assert_eq!(scope.declarations.len(), 1);
                assert!(scope.declarations.contains_key(&Identifier::from("a")));
            }

            #[test]
            fn test_function_decl() {
                let block_str = r#"
                fn a():
                    return 0

                fn b():
                    return 1
                "#;
                let (block, context) = compiler_layers::to_scopes::<Node<Block>>(block_str.as_bytes());
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

        #[test]
        fn test_block_scope() {
            // Block is:
            // let a = 5 + -1
            // let b = true and false
            let l1 = Node::from(Expr::Int("5".to_string()));
            let r1 = Node::from(Expr::Int("-1".to_string()));
            let l2 = Node::from(true);
            let r2 = Node::from(false);

            let e1 = Expr::BinaryExpr{operator: BinaryOperator::Add, left: Box::new(l1), right: Box::new(r1)};
            let e2 = Expr::BinaryExpr{operator: BinaryOperator::And, left: Box::new(l2), right: Box::new(r2)};

            let s1 = Stmt::LetStmt{name: Identifier::from("a"), type_annotation: None, expression: Node::from(e1)};
            let s2 = Stmt::LetStmt{name: Identifier::from("b"), type_annotation: None, expression: Node::from(e2)};

            let mut block = Node::from(Block{
                statements: vec!(Box::new(Node::from(s1)), Box::new(Node::from(s2)))
            });
            let (id, init) = initial_context();
            let context = block.gen_scopes(id, &init);
            let scope = context.get_scope(block.scope);
            assert_eq!(scope.declarations.len(), 2);
            unsafe {
                let stmt1_pointer = context.get_declaration(block.scope, &Identifier::from("a")).unwrap();
                match stmt1_pointer {
                    CanModifyScope::Statement(x, _) => {
                        match (**x).data {
                            Stmt::LetStmt{ref name, ..} => {
                                assert_eq!(name, &Identifier::from("a"))
                            },
                            _ => panic!()
                        }
                    },
                    _ => panic!()
                }

                let stmt1_pointer = context.get_declaration(block.scope, &Identifier::from("b")).unwrap();
                match stmt1_pointer {
                    CanModifyScope::Statement(x, _) => {
                        match (**x).data {
                            Stmt::LetStmt{ref name, ..} => {
                                assert_eq!(name, &Identifier::from("b"))
                            },
                            _ => panic!()
                        }
                    },
                    _ => panic!()
                }
            }
        }
    }

    #[test]
    fn test_get_declarations() {
        let (func_dec, context) = compiler_layers::to_scopes::<Node<Stmt>>("fn a(b: i32) -> i32:\n let x = 5 + 6\n return x\n".as_bytes());
        let new_ident = Identifier::from("x");
        let actual = func_dec.get_true_declarations(&context);
        for ptr in actual {
            assert_eq!(ptr, new_ident);
        }
    }
}
