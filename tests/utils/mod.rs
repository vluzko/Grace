extern crate difference;
extern crate grace_lib;
extern crate itertools;
extern crate regex;
use self::grace_lib::compiler_layers;
use std::path::Path;

use self::difference::{Changeset, Difference};
use self::regex::Regex;
use std::fs::{read_dir, read_to_string};

pub fn compile_folder(subfolder: &str) {
    let folder_path = format!("./tests/test_data/{}", subfolder);
    let output_path = format!("./tests/test_data/{}/outputs", subfolder);
    let file_path = format!("{}/file_1.gr", folder_path);
    let compiled = compiler_layers::Compilation::compile(&file_path);
    let _ = compiled.generate_wast_files(&Box::from(Path::new(&output_path)));
    let paths = read_dir(folder_path).unwrap();
    for path in paths {
        let p = path.unwrap().path();
        let is_gr = match p.extension() {
            Some(s) => s == "gr",
            None => false,
        };
        if is_gr {
            let name = p.file_stem();
            let output_file = format!("{}/{}.wat", output_path, name.unwrap().to_str().unwrap());
            let expected_file = format!(
                "{}/{}_expected.wat",
                output_path,
                name.unwrap().to_str().unwrap()
            );
            let actual = read_to_string(output_file).unwrap();
            let expected = read_to_string(expected_file).unwrap();
            let changeset = Changeset::new(expected.as_str(), actual.as_str(), "");
            let scope_suffix_regex = Regex::new(r"^\.(\d)+$").unwrap();
            for diff in changeset.diffs {
                match diff {
                    Difference::Same(_) => {}
                    Difference::Rem(x) => panic!("Removed {:?} in {:?}", x, name),
                    Difference::Add(added_string) => {
                        // Check if the thing being added is a scope ID on the end
                        // of a variable
                        // Scope IDs aren't the same every time, so instead of comparing
                        // the string, check that the diff is plausibly a scope_id
                        assert!(
                            scope_suffix_regex.is_match(added_string.as_str()),
                            "Added {:?} in {:?}",
                            added_string,
                            name
                        );
                    }
                }
            }
        }
    }
}
