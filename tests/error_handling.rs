mod utils;

#[test]
#[should_panic(expected = "line: 6, column: 13")]
fn line_and_column_test() {
    utils::compile_folder("line_and_column_test");
}