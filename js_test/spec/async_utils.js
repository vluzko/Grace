"use strict";
let child = require("child_process");
let process = require("process");
let util = require("util");

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

/**
 *
 * @param {String} input
 * @param {String} output
 */
function compile_grace(input, output) {
  process.chdir("..");
  // Asynchronously compile the Grace code to WAST.
  let compile_to_wast = exec(`cargo run ${input} ${output}`);

  // Once that's finished, asynchronously compile the WAST to WASM.
  let compile_to_wasm = compile_to_wast.then(({stdout, stderr})=> {
    const wasm_file = output.replace(".wat", ".wasm");
    return exec(`wat2wasm ${output} -o ${wasm_file}`);
  });

  // Once *that's* finished, asynchronously load the generated WASM into a WASM module.
  return compile_to_wasm;

}

exports.utils = {
  get_async_it,
  wrap_promise,
  compile_grace
};
