"use strict";
var Persona = require('persona');

Persona.ApiClient.instance.basePath = process.env.PERSONA_URL;

// https://visionmedia.github.io/superagent/#timeouts
Persona.ApiClient.instance.timeout = {
  response: 60000, // the server has a whopping 60 seconds to respond
  deadline: 20000 // but up to 20 seconds of overall data transfer
};

exports.loginApi = new Persona.LoginApi(Persona.ApiClient.instance);
exports.usersApi = new Persona.UsersApi(Persona.ApiClient.instance);
