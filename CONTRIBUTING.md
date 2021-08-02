# Contributing

## Building


## Tests
Tests of the Rust code live in `src/` and can be run as usual with `cargo test`. 

Tests of the compiler on Grace code that do not actually run the code are in `project_test/`. As the name suggests these are mostly used for benchmarking the compiler.

Full output tests must run in a JS environment, and live in `js_test/`. Tests are run using [`jest`](https://github.com/facebook/jest).

## Profiling
[`flamegraph`](https://github.com/flamegraph-rs/flamegraph) is currently used for profiling.
