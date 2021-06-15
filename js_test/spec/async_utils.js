"use strict";
let child = require("child_process");
let process = require("process");
let util = require("util");
let fs = require("fs");
let exec = util.promisify(child.exec);

/**
 * Wraps a promise to call done().
 * @param promise
 * @param done
 */
function wrap_promise(promise, done) {
    return promise
        .then(function (data) {
        done();
        return data;
        }, function (err) {
        console.log("Promise error " + err);
        done();
        return err;
    });
}

/**
 * Returns a function which acts as an asynchronous it.
 * Jasmine adds far too much overhead to asynchronous testing, this resolves that.
 * @param describe
 * @return
 */
function get_async_it(describe) {
    return function async_describe(test_name, promiser, expectation) {
        describe(test_name, function () {
        let data;
        beforeEach(function (done) {
            let promise = promiser();
            data = wrap_promise(promise, done);
        });
        it(".it", function (done) {
            data.then(expectation)
            .then(function () {
                done();
            }, function (err) {
                console.log(err);
                done();
            });
        });
        });
    };
}

function get_async_desc(describe) {
  return function async_describe(test_name, promiser, expectations) {
    describe(test_name, function () {
      let data;
      beforeEach(function (done) {
        let promise = promiser();
        data = wrap_promise(promise, done);
      });

      for (let [name, expectation] of expectations) {
        it(name, function (done) {
          data.then(expectation)
            .then(function () {
              done();
            }, function (err) {
              console.log(err);
              done();
            });
        });
      }

    });
  };
}

/**
 * Compile a Grace file to WebAssembly.
 * @param {String} input  - Path to the input file.
 * @param {String} output - Path to the output file.
 */
async function compile_grace(input, output) {
    console.log(`Compiling ${input}`);
    const wasm_file = output.replace(".wat", ".wasm");
    // First clean previous versions.
    try {
        fs.unlinkSync(output);
    } catch (err) {}

    try {
        fs.unlinkSync(wasm_file);
    } catch (err) {}

    // Asynchronously compile the Grace code to WAST.
    const wast_compilation = await exec(`cargo run ${input} ${output} -- --nocapture`);
    console.log(wast_compilation.stdout);
    // Once that's finished, asynchronously compile the WAST to WASM.
    const wasm_compilation = await exec(`wat2wasm ${output} -o ${wasm_file}`);

    // Once *that's* finished, asynchronously load the generated WASM into a WASM module.
    const module_as_bytes = new Uint8Array(fs.readFileSync(wasm_file));
    const mem = await compile_wat("../src/builtins/memory_management.wat");
    const ops = await compile_wat("../src/builtins/gradual_binary_ops.wat", {
        'memory_management': mem.instance.exports
    });
    return WebAssembly.instantiate(module_as_bytes, {
        'memory_management': mem.instance.exports,
        'gradual_binary_ops': ops.instance.exports
    });
}

/**
 * Compile a WAST file to WASM.
 * Requires wat2wasm in your path.
 * @param {String} input - The path to the WAST file.
 * @param {object} imports - The imports object required by the WAST module.
 */
async function compile_wat(input, imports) {
    let wasm_file = input.replace(".wat", ".wasm");
    let compile_to_wasm = await exec(`wat2wasm ${input} -o ${wasm_file}`);

    let module_as_bytes = new Uint8Array(fs.readFileSync(wasm_file));
    // TODO: Cleanup: Log failures better (built in error message is completely opaque)
    return WebAssembly.instantiate(module_as_bytes, imports);
}

exports.utils = {
    get_async_it,
    wrap_promise,
    compile_grace,
    compile_wat,
    get_async_desc
};
