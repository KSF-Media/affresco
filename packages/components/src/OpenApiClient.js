"use strict";

exports.callApi_ = function(api, methodName, params, opts) {
  var debugInfo = { api: api, methodName: methodName, params: params, opts: opts };
  return function(onError, onSuccess) {
    var args = isEmptyObject(opts) ? params : params.concat(opts);
    var req = api[methodName].apply(api, args.concat(function(err, data, res) {
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
        console.error("Superagent error", err, debugInfo);
        onError(err);
      } else {
        onSuccess(data);
      }
    }));
    return function(cancelError, onCancelerError, onCancelerSuccess) {
      try {
        req.abort();
        onCancelerSuccess();
      } catch(e) {
        onCancelerError(e);
      }
    };
  }
  function isEmptyObject(obj) {
    return Object.keys(obj).length === 0 && obj.constructor === Object;
  }
}
