use std::env;
use std::fs::File;
use std::io::prelude::*;
extern crate grace_lib;
use grace_lib::parser;
use grace_lib::utils::*;
use grace_lib::ast_node::*;
use grace_lib::bytecode::*;
use grace_lib::type_rewrites::*;


fn main() {
    let args: Vec<String> = env::args().collect();
    let mut f = File::open(&args[1]).expect("File not found");
    let mut file_contents = String::new();
    f.read_to_string(&mut file_contents);
    let parse_result = parser::module(file_contents.as_bytes());
    let type_rewrites = output(parse_result).type_based_rewrite();
    let wast = type_rewrites.generate_bytecode();
    let mut outfile = File::create(&args[2]);
    outfile.unwrap().write_all(wast.as_bytes());
}
