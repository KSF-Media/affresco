import fs from "fs";
import path from "path";
import cheerio from "cheerio";
const _ = require("lodash");

const templatePath = path.join(__dirname, "..", "client", "index.html");
const HTML_TEMPLATE = fs.readFileSync(templatePath).toString();

export default function generateHtml(markup, article, user) {
  const $template = cheerio.load(HTML_TEMPLATE);
  if (article) {
    // Clone the article because js likes to mutate everything
    const anotherArticle = Object.assign({}, article);
    // Let's remove external scripts from the article, as it can mess up
    // the final HTML for some reason. These are not needed in the article object anyways
    _.unset(anotherArticle, "externalScripts");
    $template("head").append(
      `<script>window.article = ${JSON.stringify(anotherArticle)}</script>
     <script>window.user = ${user ? JSON.stringify(user) : null}</script>
     <script>document.addEventListener('DOMContentLoaded', function(event) {
	pushLoadingArticleToGoogleTagManager(window.article, window.user);
     })</script>
    `
    );

    $template("title").text(_.get(article, "title"));

    const externalScripts = article.externalScripts || [];
    externalScripts.forEach((script) => {
      $template("head").append(script);
    });
  }
  $template("#root").html(markup);
  return $template.html();
}
