from itertools import product
from pathlib import Path

types = ("i32", "i64", "f32", "f64")
binary_operators = ("add", "sub", "mul")
comparison_operators = ("eq", "ne")
# Div, lt, le, gt, and ge have been removed due to issues distinguishing signed and unsigned versions

binary_template = """
    (func ${op}_{t} (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_{t}

        ;; Unwrap b
        get_local $b
        call $unwrap_{t}

        ;; Do the operation
        {t}.{op}

        ;; Rewrap and return.
        call $wrap_{t}
    ) (export "{op}_{t}" (func ${op}_{t}))
"""

comparison_template = """
    (func ${op}_{t} (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_{t}

        ;; Unwrap b
        get_local $b
        call $unwrap_{t}

        ;; Do the comparison and return
        {t}.{op}
    ) (export "{op}_{t}" (func ${op}_{t}))
"""

choose_template = """
    (func $choose_{op} (param $type_id i32) (result i32)
        ;; Check if it's an i32
        get_local $type_id
        i32.eqz
        if (result i32)
            i32.const {i32_ptr}
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                i32.const {i64_ptr}
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    i32.const {f32_ptr}
                else
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        i32.const {f64_ptr}
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive addition operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
    ) (export "choose_{op}" (func $choose_{op}))
"""

gradual_op_template = """   (func ${op}_gradual (param $a i32) (param $b i32) (result i32) (local $type_id i32)
        ;; Get the type ID of a
        get_local $a
        i32.load
        ;; Check if it's an i32
        tee_local $type_id
        i32.eqz
        if (result i32)
            get_local $a
            get_local $b
            call ${op}_i32
        else
            ;;  Check if it's an i64
            get_local $type_id
            i32.const 1
            i32.eq
            if (result i32)
                get_local $a
                get_local $b
                call ${op}_i64
            else
                ;; Check if it's an f32
                get_local $type_id
                i32.const 2
                i32.eq
                if (result i32)
                    get_local $a
                    get_local $b
                    call ${op}_f32
                else
                    ;; Check if it's an f64
                    get_local $type_id
                    i32.const 3
                    i32.eq
                    if (result i32)
                        get_local $a
                        get_local $b
                        call ${op}_f64
                    else
                        ;; TODO: Handle non-primitive values.
                        ;; This is essentially a method call.
                        ;; e.g. "a" + "b" has no primitive operator, so we have to call some string method.
                        unreachable
                    end
                end
            end
        end
    ) (export "{op}_gradual" (func ${op}_gradual))"""


table_functions = ['${}_gradual'.format(op) for op in (*binary_operators, *comparison_operators)]
all_functions = []
ops_start = len(table_functions)
for i, op in enumerate(binary_operators):
    for t in types:
        binary_val = binary_template.format(op=op, t=t)
        all_functions.append(binary_val)

    gradual_val = gradual_op_template.format(op=op)
    all_functions.append(gradual_val)

for i, op in enumerate(comparison_operators):
    for t in types:
        comparison_val = comparison_template.format(op=op, t=t)
        all_functions.append(comparison_val)

    gradual_val = gradual_op_template.format(op=op)
    all_functions.append(gradual_val)

table_func_names = " ".join(table_functions)
table_output = "(elem (i32.const 0) {})".format(table_func_names)
function_output = "\n".join(all_functions)

#read in gradual_binary_ops_template.txt
path = Path(__file__).parent/"gradual_binary_ops_template.txt"
template = path.open().read()
full_file = template.format(len(table_functions), function_output, table_output)
output_path = Path(__file__).parent/"gradual_binary_ops.wat"
output_path.open("w+").write(full_file)