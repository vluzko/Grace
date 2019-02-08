# Compiler Overview

`T: Node`: separate implementations for each T that implements the Node trait. So `Stmt`, `Expr`, etc. each have their own implementation.

`Node`: A trait object or something for the `Node` trait.

## Overview

    File -> Node
    Node.name_resolution -> Result<Map<Node, Scope>, Error>

## Parser
Information required:

* Line number range
* Column number range
* Node
* Scope?


## Scoping and Name Resolution

    get_scope: Node -> Scope
    scope_map: Map<Node, Scope>

## Typechecking

    get_type: Identifier -> Scope -> Node -> Type
    get_expected_type: Node -> Type

## Rewrites

    type_rewrite: T:Node -> T

## Bytecode Generation

    generate: T:Node -> String

## Debugging
