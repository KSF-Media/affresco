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

app.use("/dist", express.static(`${__dirname}/../client`));
// 47b04f5b-b5b2-43c7-acb5-b95b24cf6784"
app.get("/*", (req, res) => {
  axios
    .get(
      "https://lettera.api.ksfmedia.fi/v3/article/d8f9668d-9f61-4486-9aca-cd845a5e1f28"
    )
    .then((response) => {
      const articleJSX = (
	<Article
	  title={response.data.title}
	  mainImage={response.data.mainImage}
	  body={response.data.body}
	  tags={response.data.tags || []}
	  relatedArticles={response.data.relatedArticles || []}
	  preamble={response.data.preamble}
	  articleType={response.data.articleType}
	  articleTypeDetails={response.data.articleTypeDetails}
	  publishingTime={response.data.publishingTime}
	  updateTime={response.data.updateTime}
	  authors={response.data.authors}
	  premium={response.data.premium}
	/>
      );
      sendArticleResponse(res, response.data, articleJSX);
    })
    .catch((err) => {
      if (err.response.status === 403) {
	const article = err.response.data.not_entitled.articlePreview;
	const previewArticleJSX = (
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
	    isPreview={true}
	  />
	);
	sendArticleResponse(
	  res,
	  _.set(article, "isPreview", true),
	  previewArticleJSX
	);
      }
    });
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
