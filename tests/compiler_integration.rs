mod utils;

#[test]
fn simple_imports_test() {
    utils::check_against_expected("simple_imports_test");
}

#[test]
fn import_calls_test() {
    utils::check_against_expected("import_calls_test");
}

#[test]
fn refinement_types_test() {
    utils::check_against_expected("refinement_types_test");
}

#[test]
fn gradual_add_test() {
    utils::check_against_expected("gradual_add_test");
}

#[test]
#[should_panic]
fn refinement_failures_test() {
    utils::check_against_expected("refinement_failures_test");
}

#[test]
fn binary_operations_test() {
    utils::check_against_expected("builtin_tests");
}