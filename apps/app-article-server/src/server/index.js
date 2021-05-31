var express = require("express");
var React = require("react");
var ReactDOM = require("react-dom/server");
var app = express();
const port = 8080;
require("dotenv").config();
import generateHtml from "./generateHtml";
import Article from "../browser/components/article";
import ErrorPage from "../browser/components/error";
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
    const queryString = req._parsedUrl.search || "";
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
	res.on("end", () => {
	  try {
	    const parsedJson = JSON.parse(body);
	    resolve(parsedJson);
	  } catch {
	    resolve(null);
	  }
	});
      });
    });
  };

  const articleResponse = await httpGet(
    "https://lettera.api.ksfmedia.fi/v3/article/" + articleId
  );

  // If we get some weird response or a 500, abort!
  if (articleResponse === null || _.get(articleResponse, "http_code") === 500) {
    const markup = ReactDOM.renderToString(
      <ErrorPage message={"Artikeln kunde inte hämtas!"} />
    );
    const html = generateHtml(markup);
    res.send(html);
    return;
  }

  const paper = queryParams.paper || "hbl";
  const mostReadResponse = await httpGet(
    process.env.LETTERA_URL + "/mostread?paper=" + paper
  );

  const user = _.get(authHeaders, "authuser")
    ? await httpGet(process.env.PERSONA_URL + "/users/" + authHeaders.authuser)
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
