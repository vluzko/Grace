// const async_utils = require("./async_utils").utils;

import {compile_grace} from './compilation';


describe("Small grace tests.", () => {
    test('Arithmetic operators', async () => {
        const module = await compile_grace("spec/inputs/small_grace.gr", "spec/outputs/small_grace.wat");
        expect(module.instance.exports.add(2, 3)).toBe(5);
        expect(module.instance.exports.sub(2, 3)).toBe(-1);
        expect(module.instance.exports.mult(2, 3)).toBe(6);
        expect(module.instance.exports.div(2, 3)).toBe(2/3);
    })
});
