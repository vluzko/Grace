import {compile_wat} from './compilation';

describe("Memory management tests.", function() {
    let mem_manage;

    beforeAll(async () => {
        mem_manage = (await compile_wat("../src/builtins/memory_management.wat")).instance.exports;
    });

    afterEach(function () {
        mem_manage.obliviate();
    });

    test('Allocation tests.', () => {
        expect(mem_manage.alloc(4)).toBe(12);
        expect(mem_manage.alloc(40)).toBe(24);
        expect(mem_manage.alloc(40)).toBe(72);
        expect(mem_manage.inspect(4)).toBe(16);
        expect(mem_manage.inspect(16)).toBe(64);
        expect(mem_manage.inspect(64)).toBe(0);
        expect(mem_manage.free_chunk(24)).toBe(1);
        expect(mem_manage.inspect(4)).toBe(64);
        expect(mem_manage.alloc(40)).toBe(24);
    })

    test('Copy many.', () => {
        expect(mem_manage.alloc(12)).toBe(12);
        mem_manage.set(12, 20);
        mem_manage.set(16, 30);
        mem_manage.set(20, 40);
        mem_manage.copy_many(12, 30, 12);
        expect(mem_manage.inspect(30)).toBe(20);
        expect(mem_manage.inspect(34)).toBe(30);
        expect(mem_manage.inspect(38)).toBe(40);
    });
});