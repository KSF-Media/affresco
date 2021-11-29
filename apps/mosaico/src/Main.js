require("dotenv").config();
var cheerio = require("cheerio");

// Writes mosaico html inside #app
exports.appendMosaicoImpl = function (a, HTML_TEMPLATE) {
  const $template = cheerio.load(HTML_TEMPLATE);
  $template("#app").append(a);
  return $template.html();
};

// Writes given element under the `head` element
exports.appendHeadImpl = function (element, HTML_TEMPLATE) {
  const $template = cheerio.load(HTML_TEMPLATE);
  $template("head").append(element);
  return $template.html();
};

// Server Port
exports.serverPort = process.env.PORT;

