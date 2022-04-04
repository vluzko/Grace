# Contributing

## Building


## Tests
* Tests of the Rust code live in `src/` and can be run as usual with `cargo test`.
* Tests of the compiler on Grace code that do not actually run the code are in `tests/`. Individual files can be run with `cargo test --test $NAME_OF_FILE`.
* Full output tests must run in a JS environment, and live in `js_test/`. Tests are run using [`jest`](https://github.com/facebook/jest).
Tests of the Rust code live in `src/` and can be run as usual with `cargo test`.

Tests of the compiler on Grace code that do not actually run the code are in `tests/`. As the name suggests these are mostly used for benchmarking the compiler.

Full output tests must run in a JS environment, and live in `js_test/`. Tests are run using [`jest`](https://github.com/facebook/jest).
>>>>>>> 53468d5b1442309da41c306adde67622347c2650

## Profiling
[`flamegraph`](https://github.com/flamegraph-rs/flamegraph) is currently used for profiling.
