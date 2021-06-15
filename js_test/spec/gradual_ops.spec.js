import {compile_wat} from './compilation';

describe("gradual tests.", () => {
    let mem_module;
    let grad_module;
    let grad_funcs;

    beforeAll(async () => {
        mem_module = (await compile_wat("../src/builtins/memory_management.wat")).instance.exports;
        const imports = {'memory_management': mem_module};
        grad_module = await compile_wat("../src/builtins/gradual_binary_ops.wat", imports);
        grad_funcs = grad_module.instance.exports;
    })

    test('wrap_i32.', async () => {
        const wrapped = grad_funcs.wrap_i32(5);
        const type_val = mem_module.inspect(wrapped);
        const data_val = mem_module.inspect(wrapped + 4);
        expect(type_val).toBe(0);
        expect(data_val).toBe(5);
    })
    
    test('add_i32.', async () => {
        const a_ptr = mem_module.alloc_words(2);
        const b_ptr = mem_module.alloc_words(2);
        mem_module.set(a_ptr, 0);
        mem_module.set(b_ptr, 0);
        mem_module.set(a_ptr+4, 2);
        mem_module.set(b_ptr+4, 7);
        const res_ptr = grad_funcs.add_i32(a_ptr, b_ptr);
        const type_res = mem_module.inspect(res_ptr);
        const data_res = mem_module.inspect(res_ptr + 4);
        expect(type_res).toBe(0);
        expect(data_res).toBe(9);
    })
    
    test('add_gradual.', async () => {
        const a_ptr = mem_module.alloc_words(2);
        const b_ptr = mem_module.alloc_words(2);
        mem_module.set(a_ptr, 0);
        mem_module.set(b_ptr, 0);
        mem_module.set(a_ptr+4, 2);
        mem_module.set(b_ptr+4, 7);
        const res_ptr = grad_funcs.add_i32(a_ptr, b_ptr);
        const type_res = mem_module.inspect(res_ptr);
        const data_res = mem_module.inspect(res_ptr + 4);
        expect(type_res).toBe(0);
        expect(data_res).toBe(9);
    })

    test('gradual_binary test add f32.', async () => {
        const a_ptr = mem_module.alloc_words(2);
        const b_ptr = mem_module.alloc_words(2);
        mem_module.set(a_ptr, 2);
        mem_module.set(b_ptr, 2);
        mem_module.set_f32(a_ptr+4, 2);
        mem_module.set_f32(b_ptr+4, 7);
        const res_ptr = grad_funcs.call_gradual(0, a_ptr, b_ptr);
        const type_res = mem_module.inspect(res_ptr);
        const data_res = mem_module.inspect_f32(res_ptr + 4);
        expect(type_res).toBe(2);
        expect(data_res).toBe(9);
    })

    test('gradual_binary test add f64.', async () => {
        const a_ptr = mem_module.alloc_words(3);
        const b_ptr = mem_module.alloc_words(3);
        mem_module.set(a_ptr, 3);
        mem_module.set(b_ptr, 3);
        mem_module.set_f64(a_ptr+4, 2);
        mem_module.set_f64(b_ptr+4, 7);
        const res_ptr = grad_funcs.call_gradual(0, a_ptr, b_ptr);
        const type_res = mem_module.inspect(res_ptr);
        const data_res = mem_module.inspect_f64(res_ptr + 4);
        expect(type_res).toBe(3);
        expect(data_res).toBe(9);
    })

    test('gradual_binary_test.', async () => {
        const a_ptr = mem_module.alloc_words(2);
        const b_ptr = mem_module.alloc_words(2);
        mem_module.set(a_ptr, 1);
        mem_module.set(b_ptr, 1);
        mem_module.set(a_ptr+4, 2);
        mem_module.set(b_ptr+4, 7);
        const res = grad_funcs.call_gradual(0, a_ptr, b_ptr);
        console.log(res)
        const type_res = mem_module.inspect(res);
        const data_res = mem_module.inspect(res + 4);
        expect(type_res).toBe(1);
        expect(data_res).toBe(9);
        // expect(first_call).toBe(7);
    });
});