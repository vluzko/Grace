(import "file_2" "func_2" (func $.file_2.func_2  (result i32)))
(import "file_3" "func_3" (func $.file_3.func_3  (result i32)))

(func $func_1  (result i32) 
i32.const 1
)(export "func_1" (func $func_1))