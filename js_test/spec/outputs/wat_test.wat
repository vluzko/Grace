(module (memory 1)
(func $array (result i32)
i32.const 0
memory.grow
memory.grow)
(export "array" (func $array))
(func $loadstuff (result i32)
i32.const 0
i32.const 7
i32.store
i32.const 0
i32.load)
(export "loadstuff" (func $loadstuff))
)
