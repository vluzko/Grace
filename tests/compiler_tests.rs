mod utils;


#[cfg(test)]
mod compiler_tests {
use super::*;

    #[test]
    fn simple_imports_test() {
        utils::compile_folder("simple_imports_test");
    }

    #[test]
    fn import_calls_test() {
        utils::compile_folder("import_calls_test");
    }

    #[test]
    fn refinement_types_test() {
        utils::compile_folder("refinement_types_test");
    }

    #[test]
    fn gradual_add_test() {
        utils::compile_folder("gradual_add_test");
    }

    #[test]
    #[should_panic]
    fn refinement_failures_test() {
        utils::compile_folder("refinement_failures_test");
    }
}
