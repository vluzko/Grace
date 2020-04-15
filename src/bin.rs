use std::env;
use std::path::Path;
extern crate grace_lib;
use grace_lib::compiler_layers;


fn main() {
    let args: Vec<String> = env::args().collect();
    let compilation = compiler_layers::Compilation::compile(&args[1]);
    compilation.generate_wast_files(&Box::from(Path::new(&args[2])));
}
