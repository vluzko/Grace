import {compile_wat} from './compilation';


describe("Basic array tests.", function () {
    let mem_manage;
    let arrays;

    beforeAll(async () => {
        mem_manage = (await compile_wat("../src/builtins/memory_management.wat")).instance.exports;
        const imports = {'memory_management': mem_manage};
        arrays = (await compile_wat('../src/builtins/arrays.wat', imports)).instance.exports;
    });

    afterEach(function () {
        mem_manage.obliviate();
    });

    test('create arrays.', () => {
        expect(arrays.create_array(10, 4)).toBe(12);
    });

    test('set and get.', () => {
        let array = arrays.create_array(10, 4);
        expect(array).toBe(12);
        // Set the third element of the array to 2.
        arrays.set_value(-2, array, 3, 4);
        expect(mem_manage.inspect(24)).toBe(-2);
        expect(arrays.get_value(array, 3, 4)).toBe(-2);
    });

    test('delete array.', () => {
        arrays.create_array(5, 1);
        let second = arrays.create_array(5, 1);
        arrays.create_array(5, 1);
        let resized = arrays.delete(second);
        expect(mem_manage.inspect(0)).toBe(4);
        expect(mem_manage.inspect(4)).toBe(60);
    });

    describe('Array resizing.', () => {
        test('Single array.', () => {
            let first = arrays.create_array(10, 4);
            let second = arrays.resize(first, 10, 4);
            expect(second).toBe(12);
        });

        test('Interior array.', () => {
            arrays.create_array(5, 1);
            let second = arrays.create_array(5, 1);
            arrays.create_array(5, 1);
            let resized = arrays.resize(second, 10, 1);
            expect(mem_manage.inspect(0)).toBe(4);
            expect(mem_manage.inspect(4)).toBe(60);
            expect(mem_manage.inspect(60)).toBe(88);
            expect(resized).toBe(96); // pointer to data segment of chunk that starts at 88
        })
    });

});
