# Rewrites
A rewrite is *type-invariant* if the return value of the rewrite has the same type as the rewritten node.

Other wise it is type variant.

## Type invariant, in parser
* Complex assignments (Implemented)

## Type variant, post parser
* For loops
    * Stmt -> (Stmt, Stmt)
* Comprehensions -> loops
    * Expr -> (Vec<Stmt>, Identifier)
    * Each rewritten loop must appear *after* its descendants.

## Type invariant, post context
* Attribute access -> module access
    * Possibly unnecessary, if we just type them as module accesses.
* Struct declaration -> function declaration
* Method call -> function call

## Type variant, low level
* First class function call -> indirect call
* Method declaration -> function declaration






