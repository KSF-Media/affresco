exports.requireDotenv = require("dotenv").config();
var cheerio = require("cheerio");

exports.appendMosaico = function (HTML_TEMPLATE, a) {
  const $template = cheerio.load(HTML_TEMPLATE);
  $template("#app").append(a);
  return $template.html();
};

exports.addArticleToHead = function (HTML_TEMPLATE, article) {
  const $template = cheerio.load(HTML_TEMPLATE);
  $template("head").append("<script>window.articleId='" + article + "';</script>");
  return $template.html();
};

exports.addArticle = function (HTML_TEMPLATE, article) {
  const $template = cheerio.load(HTML_TEMPLATE);
  $template("head").append("<script>window.article=" + JSON.stringify(article) + "</script>");
  return $template.html();
};
