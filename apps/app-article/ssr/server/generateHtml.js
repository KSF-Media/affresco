import fs from "fs";
import path from "path";
import cheerio from "cheerio";
const _ = require("lodash");

const templatePath = path.join(__dirname, "..", "client", "index.html");
const HTML_TEMPLATE = fs.readFileSync(templatePath).toString();

export default function generateHtml(markup, article) {
  const $template = cheerio.load(HTML_TEMPLATE);
  // Clone the article because js likes to mutate everything
  const anotherArticle = Object.assign({}, article);
  // Let's remove external scripts from the article, as it can mess up
  // the final HTML for some reason. These are not needed in the article object anyways
  _.unset(anotherArticle, "externalScripts");
  $template("head").append(
    `<script>window.article = ${JSON.stringify(anotherArticle)}</script>`
  );

  $template("#root").html(markup);
  $template("title").text(article.title);
  article.externalScripts.forEach((script) => {
    $template("head").append(script);
  });
  return $template.html();
}
