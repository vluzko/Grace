# Type System
The most likely scenario is that we implement a type system very similar to Scala's, then extend it with dependent types

## Implementation Details
* Rewriting the scope during the type inference step may break the raw pointers. Switch to IDs.


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
Just copy Hindley-Milner

#### Pros
* Global type inference
* There's an existing implementation

#### Cons
* No subtypes.


## Problems
* We're pretty sure we can handle gradual dependent typing by inserting run time checks. Can this be done without an enormous performance hit?
* How do we handle


## Run-Time Type System

We need a "type-wrapper" of some sort.

This tells us how to map a piece of data to it's type. In particular how to call functions with it.

## Dynamic Attribute Access
We have the code `foo.bar`. `foo` can either be an `A` or a `B`.

* Case: `type(A.bar) == type(B.bar)`
    * Perfect. Accessing `foo.bar` gives us a statically known type.
* Case: both are gradual types
    * This is fine. At runtime a gradual type is an object. This is essentially the same as them having the same type.
* Case: `type(A.bar) != type(B.bar)`
    * Now `foo.bar` has a gradual type.

## Dynamic Function Calls
We have the code `foo(a, b)`. How do we call this?

* Case: All the types are known statically
    * Just do it normally
* Case: 

## Dynamically Finding The Right Function

### One function per gradual operator

1. Take the function ID and use it as an index to an array. The value we get is a pointer to the function chooser for that function ID
2. Call the function chooser, passing the type IDs of all the gradual types. This gives us a pointer to the real function to call.
3. Call that function with the *gradual* arguments
4. The function unwraps the gradual arguments
5. It runs with the actual data
6. It wraps the result and returns

### Example
We have the type `Gradual(f32 | i32)`. How do we resolve this at run-time?

We store a "type-wrapper" object containing a pointer to a "type-descriptor" object and the actual data.

How do we do function calls?

fn foo(x, y):
    let x: Gradual(f32 | i32) = 1
    let y: Gradual(f32 | i32) = 2
    let z = x + y


First: how do we turn the let statements into WASM?

We have a `make_gradual` call which takes the underlying data (an i32, in each case), and wraps it in a gradual type.

The `+` operator gets converted to a *function call* (something like `add_gradual`).

`add_gradual` needs to map types of inputs to an actual function.

So for instance if both are `i32`, it should map to the function i32.add, which we can then call with the underlying data.

If both are `f32`, we do essentially the same thing.

If the types are mismatched, we throw an error.

Finally we call `make_gradual` on the result of that function call. That gets stored as `z`.



