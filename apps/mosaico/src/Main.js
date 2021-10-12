require("dotenv").config();
var cheerio = require("cheerio");

// Writes mosaico html inside #app
exports.appendMosaicoImpl = function (HTML_TEMPLATE, a) {
  const $template = cheerio.load(HTML_TEMPLATE);
  $template("#app").append(a);
  return $template.html();
};

// Writes article json to a window variable
exports.writeArticleImpl = function (article, isPreviewArticle, HTML_TEMPLATE) {
  const $template = cheerio.load(HTML_TEMPLATE);
  const appendArticle = "<script>window.article=" + JSON.stringify(article) + "</script>";
  const appendIsPreview = "<script>window.isPreview=" + isPreviewArticle + "</script>";
  $template("head").append(appendArticle + appendIsPreview);
  return $template.html();
};
