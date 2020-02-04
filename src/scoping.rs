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
    Argument(Type),
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

pub trait GetContext {
    fn get_usages(&self) -> HashSet<Identifier>;

    fn scopes_and_types(&mut self, parent_id: usize, context: Context) -> (Context, Type);

    /// Get all *non-Argument* declarations.
    fn get_true_declarations(&self, context: &Context) -> BTreeSet<Identifier>;
}

/// Create an empty scope.
pub fn base_scope() -> Scope {
    return Scope{
        parent_id: None,
        declarations: BTreeMap::new(),
        declaration_order: BTreeMap::new()
    };
}

pub fn builtin_context() -> (usize, Context) {
    let empty = base_scope();
    let mut init_scopes = HashMap::new();
    let id = general_utils::get_next_scope_id();
    init_scopes.insert(id, empty);
    let context = Context {
        root_id: id, 
        scopes: init_scopes, 
        containing_scopes: HashMap::new(),
        type_map: HashMap::new(), 
        defined_types: HashMap::new()
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
        return return Scope{
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

impl Context {
    pub fn empty() -> Context {
        return Context{
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

    pub fn get_type(&self, scope_id: usize, name: &Identifier) -> Type {
        let scope_mod = self.get_declaration(scope_id, name).unwrap();
        let t = match scope_mod {
            CanModifyScope::Statement(_, ref id) | CanModifyScope::Expression(_, ref id) => {
                self.type_map.get(id).unwrap().clone()
            },
            CanModifyScope::Argument(ref t) => t.clone(),
            CanModifyScope::ImportedModule(ref _id) => panic!()
        };
        return t;
    }

    pub fn g_type(&self, node_id: usize) -> Type {
        return self.type_map.get(&node_id).unwrap().clone();
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

    /// Add the
    pub fn append_declaration(&mut self, scope_id: usize, name: &Identifier, stmt: &Box<Node<Stmt>>) {
        let mut scope = self.scopes.get_mut(&scope_id).unwrap();
        scope.append_declaration(name, stmt);
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
            CanModifyScope::ImportedModule(ref id) => *id,
            CanModifyScope::Argument(..) => panic!()
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

        return (new_context, Type::empty);
    }

    fn get_true_declarations(&self, context: &Context) -> BTreeSet<Identifier> {
        let mut top_level: BTreeSet<Identifier> = context.get_scope(self.scope).declarations.keys().map(|x| x.clone()).collect();
        for stmt in &self.data.declarations {
            top_level = general_utils::mb_union(top_level, stmt.get_true_declarations(context));
        }
        return top_level;
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
                    block_type.merge(&res.1);
                    break;
                },
                Stmt::BreakStmt | Stmt::ContinueStmt => {
                    block_type.merge(&Type::empty);
                    break;
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

    fn get_true_declarations(&self, context: &Context) -> BTreeSet<Identifier> {
        let mut top_level: BTreeSet<Identifier> = context.get_scope(self.scope).declarations.keys().map(|x| x.clone()).collect();
        for stmt in &self.data.statements {
            top_level = general_utils::mb_union(top_level, stmt.get_true_declarations(context));
        }
        return top_level;
    }
}

impl GetContext for Node<Stmt> {
    
    fn get_usages(&self) -> HashSet<Identifier> {
        return match self.data {
            Stmt::IfStmt{ref condition, ref block, ref else_block} => {
                let mut usages = general_utils::m_union(condition.get_usages(), block.get_usages());
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
            Stmt::LetStmt{ref mut expression, ..} => {
                // TODO: Type checking
                expression.scopes_and_types(parent_id, context)
            },
            Stmt::AssignmentStmt{ref mut expression, ..} => {
                // TODO: Type checking
                expression.scopes_and_types(parent_id, context)
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
                
                let scope_id = context.new_scope(new_scope);
                self.scope = scope_id;

                let (block_context, block_type) = block.scopes_and_types(scope_id, context);
                // TODO: Type checking
                return_type.merge(&block_type);

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
            Stmt::StructDec{ref fields, ..} => {
                let mut order = vec!();
                let mut records = BTreeMap::new();
                for (n, t) in fields {
                    order.push(n.clone());
                    records.insert(n.clone(), t.clone());
                }
                let record = Type::Record(order, records);
                (context, record)

            },
            Stmt::ReturnStmt(ref mut expression) => {
                expression.scopes_and_types(parent_id, context)
            },
            _ => panic!()
        };
        final_c.add_type(self.id, final_t.clone());
        return (final_c, final_t);
    }

    fn get_true_declarations(&self, context: &Context) -> BTreeSet<Identifier> {
        return match self.data {
            Stmt::FunctionDecStmt{ref block, ..} => {
                block.get_true_declarations(context)
            },
            _ => BTreeSet::new()
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
            Expr::IdentifierExpr(ref name) => {
                let t = context.get_type(self.scope, name);
                context.add_type(self.id, t.clone());
                (context, t)
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

    fn get_true_declarations(&self, _context: &Context) -> BTreeSet<Identifier> {
        panic!()
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
        let (func_stmt, context) = compiler_layers::to_context::<Node<Stmt>>(func_str.as_bytes());
        let usages = func_stmt.get_usages();
        assert!(usages.contains(&Identifier::from("b")));
        assert!(usages.contains(&Identifier::from("c")));
    }

    #[test]
    fn test_get_declarations() {
        let (func_dec, context) = compiler_layers::to_context::<Node<Stmt>>("fn a(b: i32) -> i32:\n let x = 5 + 6\n return x\n".as_bytes());
        let new_ident = Identifier::from("x");
        let actual = func_dec.get_true_declarations(&context);
        for ptr in actual {
            assert_eq!(ptr, new_ident);
        }
    }

    #[cfg(test)]
    mod scope_generation {
        use super::*;
        
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
                statements: vec!(Box::new(Node::from(s1.clone())), Box::new(Node::from(s2.clone())))
            });
            let (id, init) = builtin_context();
            let context = block.scopes_and_types(id, init).0;
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
                        };
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
                        };
                    },
                    _ => panic!()
                }
            }
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
