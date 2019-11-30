use std::collections::HashSet;

use expression::*;
use typing::Type;
use general_utils::get_next_var;

#[derive(Debug, Clone, PartialEq, Eq)]
/// The available context at every step of the parser.
pub struct ParserContext {
    pub imported_names: HashSet<Identifier>,
    pub indent: usize
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Update {
    pub new_statements: Vec<Box<Node<Stmt>>>,
}

impl Update {
    pub fn empty() -> Update {
        return Update {
            new_statements: vec!(),
        };
    }

    pub fn join(mut self, mut b: Update) -> Update {
        self.new_statements.append(&mut b.new_statements);
        return self;
    }
}

pub trait ASTRewrite<T> {
    fn rewrite(&self, context: &ParserContext) -> (T, Update);
}

impl ASTRewrite <Node<Block>> for Node<Block> {
    fn rewrite(&self, context: &ParserContext)  -> (Node<Block>, Update) {
        let mut stmts = vec!();
        let mut update = Update::empty();
        for (s, mut u) in self.data.statements.iter().map(|x| x.rewrite(context)) {
            stmts.append(&mut u.new_statements);
            stmts.push(Box::new(s));
            update = update.join(u);
        }
        panic!()
    }
}

impl ASTRewrite <Node<Stmt>> for Node<Stmt> {
    fn rewrite(&self, context: &ParserContext)  -> (Node<Stmt>, Update) {
        let (new_data, final_update) = match &self.data {
            Stmt::LetStmt{ref name, ref type_annotation, ref expression} => {
                let (new_expr, update) = expression.rewrite(context);
                let new_stmt = Stmt::LetStmt{
                    name: name.clone(),
                    type_annotation: type_annotation.clone(),
                    expression: new_expr
                };
                (new_stmt, update)
            },
            Stmt::AssignmentStmt{ref name, ref expression} => {
                let (new_expr, update) = expression.rewrite(context);
                let new_stmt = Stmt::AssignmentStmt{
                    name: name.clone(),
                    expression: new_expr
                };
                (new_stmt, update)
            },
            Stmt::FunctionDecStmt{ref name, ref args, ref kwargs, ref block, ref return_type} => {
                let (new_block, update) = block.rewrite(context);
                let new_dec = Stmt::FunctionDecStmt{
                    name: name.clone(), args: args.clone(), kwargs: kwargs.clone(),
                    block: new_block,
                    return_type: return_type.clone()
                };
                (new_dec, update)
            },
            Stmt::StructDec{..} => {
                (self.data.clone(), Update::empty())
            },
            Stmt::IfStmt{ref condition, ref block, ref elifs, ref else_block} => {
                let (new_cond, c_update) = condition.rewrite(context);
                let (new_block, b_update) = block.rewrite(context);
                let mut update = c_update.join(b_update);

                let mut new_elifs = vec!();
                for (c, b) in elifs {
                    let (nc, cu) = c.rewrite(context);
                    let (nb, bu) = b.rewrite(context);
                    update = update.join(cu.join(bu));
                    new_elifs.push((nc, nb));
                }

                let new_else = match else_block {
                    Some(x) => {
                        let (ne, eu) = x.rewrite(context);
                        update = update.join(eu);
                        Some(ne)
                    },
                    None => None
                };

                let new_if = Stmt::IfStmt{
                    condition: new_cond,
                    block: new_block,
                    elifs: new_elifs,
                    else_block: new_else
                };

                (new_if, update)
            },
            Stmt::ForInStmt{ref iter_vars, ref iterator, ref block} => {
                let (r_iterator, mut update) = iterator.rewrite(context);
                let (rewritten_block, t) = block.rewrite(context);
                update = update.join(t);
                let (mut new_stmts, while_loop) = rewrite_for(iter_vars.clone(), &r_iterator, rewritten_block.data.statements.clone());
                update.new_statements.append(&mut new_stmts);

                (while_loop, update)
            },
            Stmt::WhileStmt{ref condition, ref block} => {
                let (rewritten_cond, update) = condition.rewrite(context);
                let (rewritten_block, block_update) = block.rewrite(context);
                let new_while = Stmt::WhileStmt {
                    condition: rewritten_cond,
                    block: rewritten_block
                };
                (new_while, update.join(block_update))
            },
            Stmt::ReturnStmt(ref expression) => {
                let (new_expr, update) = expression.rewrite(context);
                (Stmt::ReturnStmt(new_expr), update)
            },
            Stmt::YieldStmt(ref expression) => {
                let (new_expr, update) = expression.rewrite(context);
                (Stmt::YieldStmt(new_expr), update)
            },
            x => (x.clone(), Update::empty())
        };
        return (self.replace(new_data), final_update);
    }
}

impl ASTRewrite <Node<Expr>> for Node<Expr> {
    fn rewrite(&self, context: &ParserContext)  -> (Node<Expr>, Update) {
        let (new_data, final_update) = match &self.data {
            Expr::VecComprehension{ref value, ref iterators} => {
                
                let vec_ident = Identifier::from(format!(".{}", get_next_var()));
                // The vector being created.
                let new_let = Stmt::LetStmt{
                    name: vec_ident.clone(),
                    type_annotation: Some(Type::Undetermined),
                    expression: Node::from(Expr::FunctionCall{
                        function: wrap(Expr::from("vec")), 
                        args: vec!(), 
                        kwargs: vec!()
                    })
                };

                // Contents of the inner most loop.
                let push_stmt = Stmt::AssignmentStmt{
                    name: vec_ident.clone(),
                    expression: Node::from(Expr::FunctionCall{
                        function: wrap(Expr::AttributeAccess{
                            base: wrap(Expr::from(vec_ident.clone())),
                            attribute: Identifier::from("push")
                        }),
                        args: vec!(*value.clone()),
                        kwargs: vec!()
                    })
                };

                let mut outer_stmts = vec!(wrap(push_stmt));
                for comprehension_iter in iterators.iter().rev() {

                    // The contents of the loop.
                    let (inner_stmts, new_outer) = match &comprehension_iter.if_clause {
                        None => (outer_stmts, vec!()),
                        Some(cond) => {
                            let (new_cond, update) = cond.rewrite(context);

                            let if_stmt = Stmt::IfStmt{
                                condition: new_cond,
                                block: Node::from(Block{statements: outer_stmts}),
                                elifs: vec!(),
                                else_block: None
                            };
                            let new_stmts = vec!(wrap(if_stmt));
                            (new_stmts, update.new_statements)
                        }
                    };

                    outer_stmts = new_outer;

                    let (r_iter, mut update) = comprehension_iter.iterator.rewrite(context);

                    outer_stmts.append(&mut update.new_statements);

                    let mut rewritten = rewrite_for(
                        comprehension_iter.iter_vars.get(0).unwrap().clone(), 
                        &r_iter, 
                        inner_stmts
                    );
                    outer_stmts.append(&mut rewritten.0);
                    outer_stmts.push(wrap(rewritten.1));

                }
                outer_stmts.insert(0, wrap(new_let));
                let new_expr = Expr::IdentifierExpr(vec_ident.clone());

                (new_expr, Update{new_statements: outer_stmts})
            },
            Expr::IdentifierExpr(ref name) => {
                if context.imported_names.contains(&name) {
                    (Expr::ModuleAccess(vec!(name.clone())), Update::empty())
                } else {
                    (self.data.clone(), Update::empty())
                }
            },
            Expr::VecLiteral(ref expressions) => {
                let mut new_exprs = vec!();
                let mut update = Update::empty();
                for (expr, u) in expressions.iter().map(|x| x.rewrite(context)) {
                    new_exprs.push(expr);
                    update = update.join(u);
                }
                (Expr::VecLiteral(new_exprs), update)
            },
            Expr::SetLiteral(ref expressions) => {
                let mut new_exprs = vec!();
                let mut update = Update::empty();
                for (expr, u) in expressions.iter().map(|x| x.rewrite(context)) {
                    new_exprs.push(expr);
                    update = update.join(u);
                }
                (Expr::SetLiteral(new_exprs), update)
            },
            Expr::TupleLiteral(ref expressions) => {
                let mut new_exprs = vec!();
                let mut update = Update::empty();
                for (expr, u) in expressions.iter().map(|x| x.rewrite(context)) {
                    new_exprs.push(expr);
                    update = update.join(u);
                }
                (Expr::TupleLiteral(new_exprs), update)
            },
            x => (x.clone(), Update::empty())
        };

        return (self.replace(new_data), final_update);
    }
}

fn rewrite_for(loop_var: Identifier, iterator: &Node<Expr>, mut inner_loop: Vec<Box<Node<Stmt>>>) -> (Vec<Box<Node<Stmt>>>, Stmt) {
    // The contents of the loop.

    let mut outer_stmts = vec!();

    // Expression to create the iterator.
    let iter_call = Node::from(Expr::FunctionCall{
        function: wrap(Expr::AttributeAccess{
            base: Box::new(iterator.clone()),
            attribute: Identifier::from("iter")
        }),
        args: vec!(),
        kwargs: vec!()
    });

    // The name of the iterator
    let loop_iter_name = Identifier::from(format!(".{}", get_next_var()));
    // The name of the iterator values.
    let loop_var_name =  Identifier::from(format!(".{}", get_next_var()));

    // Statement to store the iterator
    let loop_iter = Stmt::LetStmt{
        name: loop_iter_name.clone(),
        type_annotation: Some(Type::Undetermined),
        expression: iter_call
    };
    outer_stmts.push(wrap(loop_iter));

    // The next value of the iterator
    let iter_next = Stmt::LetStmt{
        name: loop_var_name.clone(),
        type_annotation: Some(Type::Undetermined),
        expression: Node::from(Expr::FunctionCall{
            function: wrap(Expr::AttributeAccess{
                base: wrap(Expr::from(loop_iter_name)),
                attribute: Identifier::from("next")
            }),
            args: vec!(),
            kwargs: vec!()
        })
    };
    // Add the first iteration outside the loop.
    outer_stmts.push(wrap(iter_next.clone()));
    // Call next at the end of every loop.
    inner_loop.push(wrap(iter_next));

    // The contents of the first value.
    let iter_var = Stmt::LetStmt{
        name: loop_var,
        type_annotation: Some(Type::Undetermined),
        expression: Node::from(Expr::Index{
            base: wrap(Expr::from(loop_var_name.clone())),
            slices: vec!((Some(Node::from(Expr::from(0))), None, None))
        })
    };
    outer_stmts.push(wrap(iter_var));

    let iter_state = Node::from(Expr::Index{
        base: wrap(Expr::from(loop_var_name)),
        slices: vec!((Some(Node::from(Expr::from(1))), None, None))
    });

    let while_loop = Stmt::WhileStmt{
        condition: iter_state, 
        block: Node::from(Block{statements: inner_loop})
    };


    return (outer_stmts, while_loop);
}