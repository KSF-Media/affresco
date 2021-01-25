"use strict";
const util = require('util');

exports.callApi_ = function (api, methodName, params, opts) {
  var debugInfo = { api: api, methodName: methodName, params: params, opts: opts };
  return function (onError, onSuccess) {
    /*
    We need to do this parameter juggle (with `apply`) because the generated client function arity depends if it takes any `param` or not.
    For example, when getting an article from Lettera, the function `articleUuidGet` has the definition `function(uuid, opts, callback) { .. }`
    However Bottega's `paymentMethodCreditCardGet` take no params and has the definition of `function(opts, callback) { ... }`
    So `articleUuidGet` has arity of 3 but `paymentMethodCreditCardGet` has arity of 2. We need to take this into consideration.
    In short: `opts` and `callback` are always given, but `params` is not.
    */
    var args = params.concat(opts);
    var req = api[methodName].apply(api, args.concat(function (err, data, res) {
      if (err) {
        debugInfo.error = err;
        // If we have an error message we decode it and attach it as the `data`
        // so we can eventually read the error from there
        if (res && res.text) {
          try {
            err.data = JSON.parse(res.text);
          } catch (decodeErr) {
            debugInfo.decodeError = decodeErr;
            console.error("Failed to parse error response body", decodeErr)
          }
        }
        const debugString = util.inspect(debugInfo, { depth: null });
        console.error("Superagent error", err, debugInfo, debugString.replaceAll(/OAuth [0-9a-zA-Z]+/g, '<omitted>'));
        onError(err);
      } else {
        onSuccess(data);
      }
    }));
    return function (cancelError, onCancelerError, onCancelerSuccess) {
      try {
        req.abort();
        onCancelerSuccess();
      } catch (e) {
        onCancelerError(e);
      }
    };
  }
}
