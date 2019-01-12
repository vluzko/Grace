# Type System
The most likely scenario is that we implement a type system very similar to Scala's, then extend it with dependent types


## Subtyping
Our most significant short term design decision is whether and how to support subtyping / inheritance. Since our goal is to *compete* with Javascript, we must also compete for Javascript *coders*.

Some solutions under consideration:

### Full Scala style subtypes and inheritance

#### Pros
* Easiest switch for OOP coders
* There's an existing implementation

#### Cons
* Only local type inference


### Inheritance without subtyping
Provide syntactic sugar for "inheritance", but without establishing any subtype relationship.

#### Pros
* Can still do global type inference (ignoring dependent types, obviously)
* Preserves many of the time saving properties of inheritance

#### Cons
* Novel paradigm (to our knowledge)
* Might be confusing to OOP coders, since we meet one of their expectations but violate another

### No Subtypes
Just copy Haskell/Coq/Idris

#### Pros
* Global type inference
* There's an existing implementation

#### Cons
* No subtypes.


## Problems
* We're pretty sure we can handle gradual dependent typing by inserting run time checks. Can this be done without an enormous performance hit?
* How do we handle
