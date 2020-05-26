from itertools import product

types = ("i32", "i64", "f32", "f64")
operators = ("add", "sub", "mul", "eq", "ne")
# Div, lt, le, gt, and ge have been removed due to issues distinguishing signed and unsigned versions

binary_template = """
    (func ${op}_{t} (param $a i32) (param $b i32) (result i32)
        ;; Unwrap a
        get_local $a
        call $unwrap_{t}

        ;; Unwrap b
        get_local $b
        call $unwrap_{t}

        ;; Add them
        {t}.{op}

        ;; Rewrap and return.
        call $wrap_{t}
    )
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
    )
"""

gradual_op_template = """   (func ${op}_gradual (param $a i32) (param $b i32) (result i32)
        ;; Get the type ID of a
        get_local $a
        i32.load

        ;; Get a pointer to the correct operator function.
        call $choose_{op}

        ;; Call the returned function pointer.
        get_local $a
        get_local $b

        call_indirect (type $generic_binary)
    )"""

table_functions = []

for i, op in enumerate(operators):
    for t in types:
        binary_val = binary_template.format(op=op, t=t)
        print(binary_val)
    start = i * 4
    choose_val = choose_template.format(op=op, i32_ptr=start, i64_ptr=start+1, f32_ptr=start+2, f64_ptr=start+3)
    print(choose_val)

    gradual_val = gradual_op_template.format(op=op)
    print(gradual_val)
    table_functions.append("${}_gradual".format(op))

all_funcs = " ".join(table_functions)
table_output = "(elem (i32.const 0) {})".format(all_funcs)
print(table_output)