# Grace
[WebAssembly](https://webassembly.org/) represents the first real opportunity to replace Javascript. Grace aims to be the first *high-level* language compiling to WebAssembly.

Grace mostly draws inspiration from Python and Haskell.

## Core Features
* Garbage collection
* Gradual typing
* A relatively rich type system (possibly Scala based, the exact details are being decided)
    * We're of the opinion that OOP is a *requirement* for any language aiming to reach widespread usage, so we will have to make concessions in the type system to that.


## Notes on memory management
*Write a bunch of functions in wast that do memory-management-y things (like resizing arrays and moving them around in
memory and so forth) at runtime; have the compiler add in a call to them whenever something requiring memory managment
is going to happen
*Garbage collection: reference counting. Have a reference-counter object that knows all the names of things that have
had memory allocated to them, and where that memory is and how much, and how many references there are to that name.
Also, a wrapper function within which all the other functions (e.g. ones users write, the memory-handling ones) are
called, and which calls the garbage collector after every function (or whenever).