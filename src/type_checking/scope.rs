use std::collections::BTreeMap;

use expression::*;
use general_utils;
use type_checking::context::Context;
use type_checking::types::Type;

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
    ImportedModule(usize),
}

impl CanModifyScope {
    pub fn extract_stmt(&self) -> Stmt {
        unsafe {
            match self {
                CanModifyScope::Statement(stmt_ptr, _) => (**stmt_ptr).data.clone(),
                _ => panic!(),
            }
        }
    }

    pub fn get_id(&self) -> usize {
        match self {
            CanModifyScope::Statement(ref _ptr, ref id) => *id,
            CanModifyScope::ImportedModule(ref id) => *id,
            CanModifyScope::Argument(..) | CanModifyScope::Return(..) => panic!(),
        }
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
        Scope {
            parent_id: None,
            declarations: BTreeMap::new(),
            declaration_order: BTreeMap::new(),
            maybe_trait: None,
            maybe_struct: None,
        }
    }

    /// Create a child of the given parent
    pub fn child(parent_id: usize) -> Scope {
        Scope {
            parent_id: Some(parent_id),
            declarations: BTreeMap::new(),
            declaration_order: BTreeMap::new(),
            maybe_trait: None,
            maybe_struct: None,
        }
    }

    /// Create a child of the given parent with a struct
    pub fn child_struct_impl(parent_id: usize, structname: &Identifier) -> Scope {
        Scope {
            parent_id: Some(parent_id),
            declarations: BTreeMap::new(),
            declaration_order: BTreeMap::new(),
            maybe_trait: None,
            maybe_struct: Some(structname.clone()),
        }
    }

    /// Create a child of the given parent with a trait and a struct
    pub fn child_trait_impl(
        parent_id: usize,
        structname: &Identifier,
        traitname: &Identifier,
    ) -> Scope {
        Scope {
            parent_id: Some(parent_id),
            declarations: BTreeMap::new(),
            declaration_order: BTreeMap::new(),
            maybe_trait: Some(traitname.clone()),
            maybe_struct: Some(structname.clone()),
        }
    }

    /// Add a declaration to scope.
    pub fn append_declaration(&mut self, name: &Identifier, stmt: &Node<Stmt>) {
        self.declaration_order
            .insert(name.clone(), self.declaration_order.len() + 1);
        let scope_mod = CanModifyScope::Statement(stmt as *const _, stmt.id);
        self.declarations.insert(name.clone(), scope_mod);
    }

    /// Add a modication to scope
    pub fn append_modification(&mut self, name: &Identifier, modification: CanModifyScope) {
        self.declaration_order
            .insert(name.clone(), self.declaration_order.len() + 1);
        self.declarations.insert(name.clone(), modification);
    }

    /// All the names in a scope.
    pub fn names(&self) -> Vec<Identifier> {
        self.declarations.keys().cloned().collect()
    }
}

pub fn choose_return_type(possible: &Type) -> Type {
    match possible {
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
        }
        _ => possible.clone(),
    }
}

impl BinaryOperator {
    pub fn choose_return_type(&self, merged_type: &Type) -> Type {
        match self {
            BinaryOperator::Add
            | BinaryOperator::Sub
            | BinaryOperator::Mult
            | BinaryOperator::Mod => choose_return_type(merged_type),
            BinaryOperator::Div => Type::f32,
            _ => panic!(),
        }
    }

    /// Return the index of the corresponding gradual function in the gradual function table.
    pub fn gradual_index(&self) -> i32 {
        match self {
            BinaryOperator::Add => 0,
            BinaryOperator::Sub => 1,
            BinaryOperator::Mult => 2,
            _ => panic!(),
        }
    }

    pub fn get_builtin_trait(&self) -> (Identifier, Identifier) {
        let (x, y) = match self {
            BinaryOperator::Add => ("Add", "add"),
            BinaryOperator::Sub => ("Sub", "sub"),
            BinaryOperator::Mult => ("Mult", "mult"),
            BinaryOperator::Div => ("Div", "div"),
            BinaryOperator::And => ("And", "and"),
            BinaryOperator::Or => ("Or", "or"),
            BinaryOperator::Equal => ("Eq", "eq"),
            BinaryOperator::Unequal => ("Neq", "neq"),
            BinaryOperator::Xor => ("Xor", "xor"),
            BinaryOperator::Mod => ("Mod", "mod"),
            BinaryOperator::BitAnd => ("BitAnd", "band"),
            BinaryOperator::BitOr => ("BitOr", "bor"),
            BinaryOperator::BitXor => ("BitXor", "bxor"),
            BinaryOperator::BitShiftL => ("BitShiftL", "shl"),
            BinaryOperator::BitShiftR => ("BitShiftR", "shr"),
            BinaryOperator::Exponent => ("Exponent", "exp"),
            BinaryOperator::Greater => ("Greater", "gt"),
            BinaryOperator::Less => ("Less", "lt"),
            BinaryOperator::GreaterEqual => ("GreaterEqual", "ge"),
            BinaryOperator::LessEqual => ("LessEqual", "le"),
        };

        (Identifier::from(x), Identifier::from(y))
    }
}

impl UnaryOperator {
    pub fn get_builtin_trait(&self) -> (Identifier, Identifier) {
        let (x, y) = match self {
            UnaryOperator::BitNot => ("BitNot", "bitnot"),
            UnaryOperator::Negative => ("Negative", "negative"),
            UnaryOperator::Not => ("Not", "not"),
            UnaryOperator::Positive => ("Positive", "positive"),
        };

        (Identifier::from(x), Identifier::from(y))
    }
}

impl<'a> From<&'a Identifier> for Type {
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
            _ => Type::Named(input.clone()),
        };
    }
}

impl From<Identifier> for Type {
    fn from(input: Identifier) -> Self {
        Type::from(&input)
    }
}

/// Create and populate scopes.
pub trait SetScope {
    fn set_scope(&mut self, parent_scope: usize, context: Context) -> Context;
}

/// Use the root scope, recurse over all children.
impl SetScope for Node<Module> {
    fn set_scope(&mut self, parent_scope_id: usize, mut context: Context) -> Context {
        // TODO: Add traits
        self.scope = parent_scope_id;
        let scope = context
            .get_mut_scope(parent_scope_id)
            .expect("Compiler bug: parent scope doesn't exist.");

        // Add function names to scope
        for stmt in self.data.functions.iter_mut() {
            scope.append_declaration(&stmt.data.get_name(), stmt);
        }

        // Add struct names to scope
        for stmt in &self.data.structs {
            scope.append_declaration(&stmt.data.get_name(), stmt);
        }

        // Add all traits to the context.
        for (k, v) in &self.data.traits {
            context.traits.insert(k.clone(), v.clone());
        }

        // Add all trait implementations to the context.
        for (trait_name, struct_name, func_impls) in self.data.trait_implementations.iter_mut() {
            assert!(self.data.traits.contains_key(trait_name));

            // Create the scope for this trait implementation
            let implementation_scope =
                Scope::child_trait_impl(parent_scope_id, struct_name, trait_name);
            let impl_scope_id = context.new_scope(implementation_scope);

            for dec in func_impls.iter_mut() {
                context = dec.set_scope(impl_scope_id, context);
            }
        }

        // Set scopes for functions
        for stmt in self.data.functions.iter_mut() {
            context = stmt.set_scope(parent_scope_id, context);
        }

        // Set scopes for structs
        for stmt in self.data.structs.iter_mut() {
            context = stmt.set_scope(parent_scope_id, context);
        }

        context
    }
}

/// New scope for the block, then recurse.
impl SetScope for Node<Block> {
    fn set_scope(&mut self, parent_scope: usize, mut context: Context) -> Context {
        let new_scope = Scope::child(parent_scope);
        let scope_id = context.new_scope(new_scope);
        self.scope = scope_id;

        // let mut new_context = context;
        for stmt in self.data.statements.iter_mut() {
            context = stmt.set_scope(scope_id, context);

            // Add declarations to scope.
            if let Stmt::StructDec { ref name, .. } = &stmt.data {
                context.append_declaration(self.scope, name, stmt);
            }
        }

        context
    }
}

/// Recurse, create new scopes for any blocks.
impl SetScope for Node<Stmt> {
    fn set_scope(&mut self, parent_scope: usize, mut context: Context) -> Context {
        self.scope = parent_scope;
        match self.data {
            Stmt::LetStmt {
                ref name,
                ref mut expression,
                type_annotation: _,
            } => {
                context = expression.set_scope(parent_scope, context);
                context.append_declaration(self.scope, name, self);
                context
            }

            Stmt::FunctionDecStmt {
                ref name,
                ref mut kwargs,
                ref mut block,
                ..
            } => {
                for (_, _, ref mut val) in kwargs.iter_mut() {
                    context = val.set_scope(self.scope, context);
                }
                context = block.set_scope(self.scope, context);
                context.append_declaration(self.scope, name, self);
                context
            }
            Stmt::WhileStmt {
                ref mut condition,
                ref mut block,
            } => {
                context = condition.set_scope(self.scope, context);
                block.set_scope(self.scope, context)
            }
            Stmt::IfStmt {
                ref mut condition,
                ref mut block,
                ref mut else_block,
            } => {
                // Scope for condition and first block.
                context = condition.set_scope(self.scope, context);
                context = block.set_scope(self.scope, context);

                if let Some(b) = else_block {
                    context = b.set_scope(self.scope, context);
                }
                context
            }
            Stmt::StructDec { .. } => context,
            Stmt::AssignmentStmt {
                ref mut expression, ..
            }
            | Stmt::ReturnStmt(ref mut expression) => expression.set_scope(parent_scope, context),
            Stmt::ContinueStmt | Stmt::BreakStmt | Stmt::PassStmt => context,
            _ => panic!("add_to_context not implemented for {:?}", self.data),
        }
    }
}

/// Recurse, don't create new scopes.
impl SetScope for Node<Expr> {
    fn set_scope(&mut self, parent_scope: usize, context: Context) -> Context {
        self.scope = parent_scope;
        context
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use compiler_layers;

    #[cfg(test)]
    mod scope_generation {
        use super::*;
        use parser::base::Parseable;
        use parser::position_tracker::PosStr;

        fn check_ptr_stmt(ptr: &CanModifyScope, expected: &Stmt) -> bool {
            match ptr {
                CanModifyScope::Statement(x, _) => unsafe {
                    let actual_stmt = &(**x).data;
                    assert_eq!(actual_stmt, expected);
                    true
                },
                x => panic!("Expected a statement modification, found: {:?}", x),
            }
        }

        #[test]
        fn test_block_scope() {
            let mut e1 = Node::<Expr>::parse(PosStr::from("5 + -1"));
            e1.scope = 1;
            let s1 = Stmt::LetStmt {
                name: Identifier::from("a"),
                type_annotation: None,
                expression: e1,
            };

            let mut e2 = Node::<Expr>::parse(PosStr::from("true and false"));
            e2.scope = 1;
            let s2 = Stmt::LetStmt {
                name: Identifier::from("b"),
                type_annotation: None,
                expression: e2,
            };

            let input = r#"
            let a = 5 + -1
            let b = true and false"#;

            let (block, context) = compiler_layers::to_context::<Node<Block>>(input.as_bytes());
            let scope = context.get_scope(block.scope).unwrap();
            assert_eq!(scope.declarations.len(), 2);

            let stmt1_pointer = context
                .get_declaration(block.scope, &Identifier::from("a"))
                .unwrap();
            check_ptr_stmt(stmt1_pointer, &s1);

            let stmt2_pointer = context
                .get_declaration(block.scope, &Identifier::from("b"))
                .unwrap();
            check_ptr_stmt(stmt2_pointer, &s2);
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
                    let scoped_context = input.set_scope(context.root_id, context);
                    assert_eq!(scoped_context.scopes, original.scopes);
                }
            }
        }

        #[cfg(test)]
        mod stmts {
            use super::*;

            #[test]
            fn test_let_stmt() {
                let (block, context) =
                    compiler_layers::to_context::<Node<Block>>("let a = 1".as_bytes());
                assert_eq!(context.scopes.len(), 2);
                let scope = context.get_scope(block.scope).unwrap();
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
                let (block, context) =
                    compiler_layers::to_context::<Node<Block>>(block_str.as_bytes());
                let fn1 = context
                    .get_declaration(block.scope, &Identifier::from("a"))
                    .unwrap();
                let fn2 = context
                    .get_declaration(block.scope, &Identifier::from("b"))
                    .unwrap();
                match fn1.extract_stmt() {
                    Stmt::FunctionDecStmt { name, .. } => {
                        assert_eq!(name, Identifier::from("a"))
                    }
                    _ => panic!(),
                };

                match fn2.extract_stmt() {
                    Stmt::FunctionDecStmt { name, .. } => {
                        assert_eq!(name, Identifier::from("b"))
                    }
                    _ => panic!(),
                };
            }
        }
    }
}
