extern crate bytecount;
extern crate difference;
extern crate indexmap;
extern crate itertools;
extern crate log;
#[macro_use]
extern crate maplit;
extern crate memchr;
extern crate rand;
extern crate regex;
#[macro_use]
extern crate pretty_assertions;
extern crate petgraph;
extern crate proptest;

// pub mod position_tracker;
// #[macro_use]
// pub mod parser_utils;
pub mod bytecode;
pub mod cfg;
pub mod compiler_layers;
pub mod expression;
pub mod general_utils;
pub mod grace_error;
pub mod llr;
#[macro_use]
pub mod parser;
pub mod pre_cfg_rewrites;
pub(crate) mod proptest_utils;
pub(crate) mod test_utils;
pub mod type_checking;
