#!/bin/bash
cargo fmt
cargo doc
cp -r target/doc/grace_lib/* docs/
cargo build
