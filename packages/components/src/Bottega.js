"use strict";
var Bottega = require('bottega');

Bottega.ApiClient.instance.basePath = process.env.BOTTEGA_URL;

// https://visionmedia.github.io/superagent/#timeouts
Bottega.ApiClient.instance.timeout = {
  response: 60000, // the server has a whopping 60 seconds to respond
  deadline: 20000 // but up to 20 seconds of overall data transfer
};

exports.ordersApi = new Bottega.OrdersApi(Bottega.ApiClient.instance);
exports.packagesApi = new Bottega.PackagesApi(Bottega.ApiClient.instance);
