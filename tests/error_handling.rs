mod utils;

#[test]
#[should_panic(expected = "line: 7, column: 4")]
fn line_and_column_test() {
    utils::compile_folder("line_and_column_test");
}