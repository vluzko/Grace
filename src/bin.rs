use std::env;
use std::fs::File;
use std::io::prelude::*;
extern crate grace_lib;
use grace_lib::compiler_layers;
use grace_lib::expression::Node;
use grace_lib::expression::Module;


fn main() {
    let args: Vec<String> = env::args().collect();
    let mut f = File::open(&args[1]).expect("File not found");
    let mut file_contents = String::new();
    f.read_to_string(&mut file_contents).unwrap();
    // let (_, _, wast) = compiler_layers::to_bytecode::<Node<Module>>(file_contents.as_bytes());
    // let outfile = File::create(&args[2]);
    // outfile.unwrap().write_all(wast.as_bytes()).unwrap();
    panic!()
}
