# Rewrites
A rewrite is *type-invariant* if the return value of the rewrite has the same type as the rewritten node.
By 'same type' I mean that the output *representation* has the same type as the input representation.

Other wise it is type variant.

## Type invariant, in parser
* Complex assignments -> assignment with a modified rhs
*

## Type variant, in parser
* For loops -> while loops
    * Stmt -> (Stmt, Stmt)
* Comprehensions -> while loops
    * Expr -> (Vec<Stmt>, IdentifierExpr)
* Match expressions -> nested if/else
    * Expr -> (Vec<Stmt>, IdentifierExpr)

## Type invariant, post context
* Attribute access -> module access
    * Possibly unnecessary, if we just type them as module accesses.
* Struct declaration -> function declaration
* Method call -> function call

## Type variant, low level
* First class function call -> indirect call
* Method declaration -> function declaration
