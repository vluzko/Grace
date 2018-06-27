"use strict";
exports.__esModule = true;
var utils;
(function (utils) {
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
    utils.wrap_promise = wrap_promise;
    /**
     * Returns a function which acts as an asynchronous it.
     * Jasmine adds far too much overhead to asynchronous testing, this resolves that.
     * @param describe
     * @return {(test_name:string, promiser:any, expectation:any)=>undefined}
     */
    function get_async_it(describe) {
        return function async_describe(test_name, promiser, expectation) {
            describe(test_name, function () {
                var data;
                beforeEach(function (done) {
                    var promise = promiser();
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
    utils.get_async_it = get_async_it;
})(utils || (utils = {}));
exports.utils = utils;
