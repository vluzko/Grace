# Rewrites

## No information required
* For loops
* Complex assignments

## Type information required
* Comprehensions -> loops
* Attribute access -> module access
* First class function call -> indirect call
* Struct declaration -> function declaration
* Method declaration -> function declaration
* Method call -> function call

Rewrite post-typing, but on the AST.
