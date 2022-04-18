# Grace
[![Build Status](https://travis-ci.com/vluzko/Grace.svg?branch=master)](https://travis-ci.com/vluzko/Grace)

Grace is an attempt to provide a viable WebAssembly alternative to Javascript.

The main design principle of Grace is "gradualness": providing tools that allow the programmer to work at the level of detail required by the task. Of course this involves making tradeoffs, and Grace will never be as fast as C or as correct as Coq.

Grace mostly draws inspiration from Python and Haskell.

## Building

### Testing
* Rust unit tests are run with `cargo test`
* Rust integration tests are run with ???
* Tests for compiled WebAssembly are run with `jest` from the `js_test` folder.

## Core Planned Features
* Garbage collection (this isn't a given for WebAssembly, but obviously any viable Javascript replacement needs it)
    * For now this means implementing our own garbage collector. It's possible that WebAssembly will support some form of gc in the future.
* A rich typesystem based mostly on Haskell, with some modifications:
    * Subtyping (possibly) and/or inheritance (probably)
    * Gradual typing
    * Dependent types
    * Substructural types (possibly)
* The types of high-level niceties that Python has and Javascript decidedly lacks:
    * Operator overloading
    * Keyword arguments
    * Comprehensions
    * Arbitrary size integers by default
    * Decorators
    * An equality operator that makes any sense at all.
