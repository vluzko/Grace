extern crate grace_lib;
use self::grace_lib::compiler_layers;
use self::grace_lib::expression::Identifier;
use self::grace_lib::type_checking::types::Type;

// #[test]
// fn test_function_declaration_type() {
//     let file_name = "tests/test_data/basic_grace.gr".to_string();
//     let compilation = compiler_layers::Compilation::compile(&file_name);
//     let compiled_module = compilation.modules.get(&"basic_grace".to_string()).unwrap();
//     let first_func_id = compiled_module.ast.data.functions.get(0).unwrap().id;
//     let actual_type = compiled_module
//         .context
//         .type_map
//         .get(&first_func_id)
//         .unwrap();
//     let expected_type = Type::Function(
//         vec![(Identifier::from("arg"), Type::i32)],
//         Box::new(Type::i32),
//     );
//     assert_eq!(&expected_type, actual_type);
// }
