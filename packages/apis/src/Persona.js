"use strict";
var Persona = require("persona");

Persona.ApiClient.instance.basePath = process.env.PERSONA_URL;

// https://visionmedia.github.io/superagent/#timeouts
Persona.ApiClient.instance.timeout = {
  response: 60000, // the server has a whopping 60 seconds to respond
  deadline: 20000, // but up to 20 seconds of overall data transfer
};

exports.accountApi = new Persona.AccountApi(Persona.ApiClient.instance);
exports.adminApi = new Persona.AdminApi(Persona.ApiClient.instance);
exports.loginApi = new Persona.LoginApi(Persona.ApiClient.instance);
exports.usersApi = new Persona.UsersApi(Persona.ApiClient.instance);

exports.rawJSONStringify = function(x) {
    return JSON.stringify(x);
};

exports.rawJSONParse = function(x) {
    const reISO = /^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2}(?:\.\d*))(?:Z|(\+|-)([\d|:]*))?$/;
    return JSON.parse(x, function (key, value) {
        if (typeof value === 'string') {
            var a = reISO.exec(value);
            if (a)
                return new Date(value);
        }
        return value;
    });
};
