var express = require("express");
var React = require("react");
var ReactDOM = require("react-dom/server");
var app = express();
const port = 3000;
import App from "../src/App";
import generateHtml from "./generateHtml";
import Article from "../src/components/article";
const _ = require("lodash");
const https = require("https");
const UUID = require("uuid");

app.use("/dist", express.static(`${__dirname}/../client`));

app.get("/article/:id", async (req, res) => {
  const articleId = req.params.id;
  if (articleId && UUID.validate(articleId)) {
    const authHeaders = () => {
      if (
	req.headers.authuser &&
	UUID.validate(req.headers.authuser) &&
	req.headers.authorization
      ) {
	return {
	  authuser: req.headers.authuser,
	  authorization: req.headers.authorization,
	};
      }
    };
    const queryString = req._parsedUrl.search;
    renderArticle(articleId, res, authHeaders(), req.query, queryString);
  } else {
    res.send("");
  }
});

async function renderArticle(
  articleId,
  res,
  authHeaders,
  queryParams,
  queryString
) {
  const httpGet = (url) => {
    const requestOptions = {
      method: "get",
      headers: authHeaders,
    };
    return new Promise((resolve, reject) => {
      https.get(url, requestOptions, (res) => {
	res.setEncoding("utf8");
	let body = "";
	res.on("data", (chunk) => (body += chunk));
	res.on("end", () => resolve(JSON.parse(body)));
      });
    });
  };

  const articleResponse = await httpGet(
    "https://lettera.api.ksfmedia.fi/v3/article/" + articleId
  );

  const paper = queryParams.paper || "hbl";
  const mostReadResponse = await httpGet(
    "https://lettera.api.ksfmedia.fi/v3/mostread?paper=" + paper
  );

  const user = _.get(authHeaders, "authuser")
    ? await httpGet(
	"https://persona.api.ksfmedia.fi/v1/users/" + authHeaders.authuser
      )
    : null;

  const isPreviewArticle =
    articleResponse.http_code === 403 &&
    _.has(articleResponse, "not_entitled.articlePreview");

  const article = isPreviewArticle
    ? articleResponse.not_entitled.articlePreview
    : articleResponse;

  const mostReadArticles = _.isArray(mostReadResponse) ? mostReadResponse : [];

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
      fontSize={queryParams.fontSize}
      darkModeEnabled={queryParams.mode === "dark"}
      queryString={queryString}
      paper={paper}
    />
  );

  const updatedArticle = _.merge(article, {
    mostReadArticles: mostReadArticles,
    isPreview: isPreviewArticle,
    fontSize: queryParams.fontSize,
    darkModeEnabled: queryParams.mode === "dark",
    queryString: queryString,
    paper: paper,
  });

  const markup = ReactDOM.renderToString(articleJSX);
  const html = generateHtml(markup, updatedArticle, user);
  res.send(html);
}

app.listen(port, () => {
  console.log(`Up and running!`);
});
