mod utils;

#[test]
#[should_panic(expected = "line: 6, column: 13, end_line: 6, end_column: 19")]
fn line_and_column_test() {
    utils::compile_folder("line_and_column_test");
}