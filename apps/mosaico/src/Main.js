require("dotenv").config();
var cheerio = require("cheerio");

// Writes mosaico html inside #app
exports.appendMosaicoImpl = function (HTML_TEMPLATE, a) {
  const $template = cheerio.load(HTML_TEMPLATE);
  $template("#app").append(a);
  return $template.html();
};

// Writes article json to a window variable
exports.writeArticleImpl = function (article, isPreviewArticle, mostReadArticles, isDraftArticle, HTML_TEMPLATE) {
  const $template = cheerio.load(HTML_TEMPLATE);
  const appendArticle = "<script>window.article=" + JSON.stringify(article) + "</script>";
  const appendIsPreview = "<script>window.isPreview=" + isPreviewArticle + "</script>";
  const appendMostReadArticles = "<script>window.mostReadArticles=" + JSON.stringify(mostReadArticles) + "</script>";
  const appendIsDraft = "<script>window.isDraft=" + isDraftArticle + "</script>";
  $template("head").append(appendArticle + appendIsPreview + appendMostReadArticles + appendIsDraft);
  return $template.html();
};

exports.writeFrontpageImpl = function (frontpageArticles, mostReadArticles, HTML_TEMPLATE) {
  const $template = cheerio.load(HTML_TEMPLATE);
  const appendFrontpage = "<script>window.frontpageArticles=" + JSON.stringify(frontpageArticles) + "</script>";
  const appendMostReadArticles = "<script>window.mostReadArticles=" + JSON.stringify(mostReadArticles) + "</script>";
  $template("head").append(appendFrontpage + appendMostReadArticles);
  return $template.html();
};

exports.writeStaticPageImpl = function (pageName, pageContent, HTML_TEMPLATE) {
  const $template = cheerio.load(HTML_TEMPLATE);
  const staticPageContent = { pageName, pageContent };
  const appendStaticPage = "<script>window.staticPageContent=" + JSON.stringify(staticPageContent) + "</script>";
  $template("head").append(appendStaticPage);
  return $template.html();
}
