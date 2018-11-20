"use strict";
var Persona = require('persona');

Persona.ApiClient.instance.basePath = process.env.PERSONA_URL;

exports.loginApi = new Persona.LoginApi(Persona.ApiClient.instance);
exports.usersApi = new Persona.UsersApi(Persona.ApiClient.instance);

exports.callApi_ = function(api, methodName, params, opts) {
  return function(onError, onSuccess) {
    var args = isEmptyObject(opts) ? params : params.concat(opts);
    var req = api[methodName].apply(api, args.concat(function(err, data, res) {
      if (err) {
        // If we have an error message we decode it and attach it as the `data`
        // so we can eventually read the error from there
        if (res && res.text) {
          err.data = JSON.parse(res.text);
        }
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
