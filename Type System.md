# Type System

## Problems
* ASTNodes can be equal without having the same type (Ident('a') equals Ident('a'), even though they can have different types in different scopes).
* Not sure if scope can be unambiguously identified just from the parse tree
