# Grace
[![Build Status](https://travis-ci.com/vluzko/Grace.svg?branch=master)](https://travis-ci.com/vluzko/Grace)

Grace is an attempt to provide a viable WebAssembly alternative to Javascript.

The main design principle of Grace is "gradualness": providing tools that allow the programmer to work at the level of detail required by the task. Of course this involves making tradeoffs, and Grace will never be as fast as C or as correct as Coq.

Grace mostly draws inspiration from Python and Haskell.

## Building
`cargo build`

### Testing
* Rust unit tests are run with `cargo test`
* Rust integration tests are run with ???
* Tests for compiled WebAssembly are run with `jest` from the `js_test` folder.

