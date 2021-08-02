use std::env;
use std::path::Path;
extern crate grace_lib;
use grace_lib::compiler_layers;

fn main() {
    let args: Vec<String> = env::args().collect();
    let compilation = compiler_layers::Compilation::compile(&args[1]);
    let input_path: Box<Path> = Box::from(Path::new(&args[1]));
    let output_path: Box<Path> = Box::from(Path::new(&args[2]));
    // Check if input_path and output_path are files or directories
    // If input_path is a file, assume output_path is also a file and compile to the parent
    if input_path.is_file() {
        match output_path.parent() {
            Some(parent) => {
                compilation.generate_wast_files(&Box::from(parent));
            }
            None => println!("{:?} is not a valid output location.", output_path),
        };
    } else {
        compilation.generate_wast_files(&output_path);
    }
}
