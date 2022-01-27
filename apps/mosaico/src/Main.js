var cheerio = require("cheerio");

exports.parseTemplate = function (HTML_TEMPLATE) {
  return cheerio.load(HTML_TEMPLATE);
}

exports.renderTemplateHtml = function ($template) {
  return $template.html();
}

exports.cloneTemplate = function ($template) {
  return $template("html").clone();
}

// Writes mosaico html inside #app
exports.appendMosaicoImpl = function (a, $template) {
  $template.find("#app").append(a);
  return $template;
};

// Writes given element under the `head` element
exports.appendHeadImpl = function (element, $template) {
  $template.find("head").append(element);
  return $template;
};

// Appends content to body
exports.appendVarsImpl = function (script, $template) {
  $template.find("#app-vars").append(script);
  return $template;
}

// Server Port
exports.serverPort = process.env.PORT;
