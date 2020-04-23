# Gradual Refinement Types Project
[![Build Status](https://travis-ci.com/vluzko/Grace.svg?branch=master)](https://travis-ci.com/vluzko/Grace)

This is a fork of a programming language project ([Grace](https://www.github.com/vluzko/Grace)) that adds gradual and refinement types.

## Installation
Fully installing Grace is unfortunately a complicated process. The core components are:

* [Rust](https://www.rust-lang.org/tools/install) - The Grace compiler is written in Rust.
* [Z3](https://github.com/Z3Prover/z3) - Refinement type checking uses Z3 to check that all the constraints are satisfiable
* [Python](https://www.python.org/downloads/release/python-382/) - There are no official Z3 bindings for Rust, so we call out to Python instead. This is very much a temporary hack. The required packages are in requirements.txt.
* [wabt](https://github.com/WebAssembly/wabt) - Grace targets WebAssembly, specifically the WebAssembly Text Format (WAST). Compiling WAST to actual WebAssembly requires wabt.
* [Node](https://nodejs.org/en/) or a web browser. WebAssembly requires a virtual machine to run. NodeJS and all modern web browsers can run WebAssembly. (NodeJS is used for testing)

Once the requirements are installed, this repo can be built with:

    git clone https://github.com/vluzko/gradual-refinement-types.git
    cd gradual-refinement-types
    cargo build

The compiler can then be run with `cargo run /path/to/main/file /path/to/output/folder`
