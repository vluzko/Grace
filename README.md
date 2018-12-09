# Grace
[![Build Status](https://travis-ci.com/vluzko/Grace.svg?branch=master)](https://travis-ci.com/vluzko/Grace)

Grace is an attempt to provide a viable alternative to Javascript that *doesn't* compile to Javascript. Instead we target WebAssembly.

The main design principle of Grace is "gradualness": providing tools that allow the programmer to work at the level of detail required by the task. Of course this involves making tradeoffs, and Grace will never be as fast as C or as correct as Idris.

Grace mostly draws inspiration from Python and Haskell.

## Core Planned Features
* Garbage collection (this isn't a given for WebAssembly, but obviously any viable Javascript replacement needs it)
    * For now this means implementing our own garbage collector. It's possible that WebAssembly will support some form of gc in the future.
* A rich typesystem (see the corresponding document)
    * Subtyping
    * Gradual typing
    * Parametric polymorphism
    * Typeclasses
    * Kinds
    * Algebraic data types
    * Gradual dependent typing
* The types of high-level niceties that Python has and Javascript decidedly lacks
    * Operator overloading
    * Keyword arguments
    * Comprehensions
    * Arbitrary size integers by default
    * Decorators

## Roadmap to v 0.1
Grace is *very much* in development. It's not remotely usable as a language yet. We're planning on declaring version 0.1 once Grace reaches roughly the same level of functionality as C, meaning:

* ~~Parsing~~
* ~~WebAssembly memory management~~
    * ~~Alloc and free~~
    * ~~Arrays~~
* Basic programming constructs
    * ~~Functions~~
    * ~~Variables~~
    * ~~Arithmetic and logic~~
    * Control flow
        * ~~while~~
        * ~~if~~
        * break
    * Imports
    * Structs
* Basic type checking and inference
