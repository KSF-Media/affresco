var express = require("express");
var React = require("react");
var ReactDOM = require("react-dom/server");
var app = express();
const port = 3000;
var middleware = require("./middleware");
// var App = require("../src/App.jsx");
import generateHtml from "./generateHtml";
import Article from "../src/components/article";
const axios = require("axios");
const _ = require("lodash");
const https = require("https");

app.use("/dist", express.static(`${__dirname}/../client`));
// 47b04f5b-b5b2-43c7-acb5-b95b24cf6784"
app.get("/*", async (req, res) => {
  const httpGet = (url) => {
    return new Promise((resolve, reject) => {
      https.get(url, (res) => {
	res.setEncoding("utf8");
	let body = "";
	res.on("data", (chunk) => (body += chunk));
	res.on("end", () => resolve(JSON.parse(body)));
      });
    });
  };

  const articleResponse = await httpGet(
    "https://lettera.api.ksfmedia.fi/v3/article/d8f9668d-9f61-4486-9aca-cd845a5e1f28"
  );
  const mostReadResponse = await httpGet(
    "https://lettera.api.ksfmedia.fi/v3/mostread?paper=hbl"
  );
  console.log(articleResponse.http_code);

  const isPreviewArticle =
    articleResponse.http_code === 403 &&
    _.has(articleResponse, "not_entitled.articlePreview");

  const article = isPreviewArticle
    ? articleResponse.not_entitled.articlePreview
    : articleResponse;

  const mostReadArticles =
    typeof mostReadResponse === "array" ? mostReadResponse : [];

  const articleJSX = (
    <Article
      title={article.title}
      mainImage={article.mainImage}
      body={article.body}
      tags={article.tags || []}
      relatedArticles={article.relatedArticles || []}
      preamble={article.preamble}
      articleType={article.articleType}
      articleTypeDetails={article.articleTypeDetails}
      publishingTime={article.publishingTime}
      updateTime={article.updateTime}
      authors={article.authors}
      premium={article.premium}
      isPreview={isPreviewArticle}
      mostReadArticles={mostReadArticles}
    />
  );
  sendArticleResponse(
    res,
    _.merge(article, {
      mostReadArticles: mostReadArticles,
      isPreview: isPreviewArticle,
    }),
    articleJSX
  );
});

function sendArticleResponse(res, article, articleJSX) {
  const markup = ReactDOM.renderToString(articleJSX);
  const html = generateHtml(markup, article);
  res.send(html);
}

// respond with "hello world" when a GET request is made to the homepage
// app.get("/article/:articleId", function (req, res) {
//   res.send(ReactDOMServer.renderToString(App.App()));
// });

app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`);
});
