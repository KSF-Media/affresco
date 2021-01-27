"use strict";
var Lettera = require('lettera');

Lettera.ApiClient.instance.basePath = process.env.LETTERA_URL;

// https://visionmedia.github.io/superagent/#timeouts
Lettera.ApiClient.instance.timeout = {
  response: 60000, // the server has a whopping 60 seconds to respond
  deadline: 20000 // but up to 20 seconds of overall data transfer
};

exports.articlesApi = new Lettera.ArticlesApi(Lettera.ApiClient.instance);
