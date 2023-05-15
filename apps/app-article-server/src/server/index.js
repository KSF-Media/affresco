/* global __dirname */
const express = require("express");
const React = require("react");
const ReactDOM = require("react-dom/server");
const app = express();
const port = process.env.PORT || 8080;
const axios = require("axios");
const _ = require("lodash");
const UUID = require("uuid");

import generateHtml from "./generateHtml";
import insertAds from "./insertAds";
import Article from "../browser/components/article";
import ErrorPage from "../browser/components/error";
import { v4 as uuidv4 } from "uuid";

app.use("/dist", express.static(`${__dirname}/../client`));

app.get("/", async (req, res) => {
  res.send("/");
});

app.get("/healthz", async (req, res) => {
  res.send("OK");
});

app.get("/article/:id", async (req, res) => {
  const articleId = req.params.id;
  if (articleId && UUID.validate(articleId)) {
    const authHeaders = () => {
      if (req.headers.authuser && UUID.validate(req.headers.authuser) && req.headers.authorization) {
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

async function renderArticle(articleId, res, authHeaders, queryParams, queryString) {
  let articleReq = axios.get(process.env.LETTERA_URL + "/article/" + articleId, {
    headers: authHeaders,
    // Throw errors on any typical HTTP status code except 403, which is the response status of a preview article
    validateStatus: function (httpStatus) {
      return httpStatus < 400 || httpStatus === 403;
    },
  });
  const paper = queryParams.paper || "hbl";
  const darkModeEnabled = queryParams.mode === "dark";

  const requests = [articleReq];
  // If we have a user id in the headers, let's fetch the user too
  if (_.has(authHeaders, "authuser") && _.has(authHeaders, "authorization")) {
    requests.push(
      axios.get(process.env.PERSONA_URL + "/users/" + authHeaders.authuser, {
        headers: { authorization: authHeaders.authorization },
        validateStatus: function (httpStatus) {
          return httpStatus < 400 || httpStatus === 403;
        },
      })
    );
  }

  axios
    .all(requests)
    .then(
      axios.spread((...responses) => {
        const articleResponse = responses[0].data;
        let article;
        let isPreviewArticle;
        // If the response.data is an article already, meaning we can find the article id (uuid) there, great! We have an article
        if (_.has(articleResponse, "uuid")) {
          article = articleResponse;
          isPreviewArticle = false;
          // Otherwise, it might be a preview article, so we need to dig a bit deeper into the JSON object
        } else if (_.has(articleResponse, "not_entitled")) {
          article = _.get(articleResponse, "not_entitled.articlePreview");
          isPreviewArticle = true;
        }
        const user = _.get(responses[1], "data");

        const articleBodyWithAds = insertAds(article.body);

        const articleBody = articleBodyWithAds.map((block) => {
          return {
            ...block,
            key: uuidv4(),
          };
        });

        article = { ...article, body: articleBody };

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
            removeAds={article.removeAds}
            isPreview={isPreviewArticle}
            fontSize={queryParams.fontSize}
            darkModeEnabled={darkModeEnabled}
            queryString={queryString}
            paper={paper}
          />
        );

        const updatedArticle = _.merge(article, {
          isPreview: isPreviewArticle,
          fontSize: queryParams.fontSize,
          darkModeEnabled,
          queryString: queryString,
          paper: paper,
        });
        const markup = ReactDOM.renderToString(articleJSX);
        const finalHtml = generateHtml(markup, updatedArticle, user);
        res.send(finalHtml);
      })
    )
    .catch((errors) => {
      const markup = ReactDOM.renderToString(<ErrorPage message={"Artikeln kunde inte hämtas!"} />);
      const html = generateHtml(markup);
      // We might get some other error as well here, so let's log that also
      const errorMessage = _.get(errors, "response.data") || errors;
      console.warn("Failed to fetch article!", {
        url: _.get(errors, "response.config.url"),
        status: _.get(errors, "response.status"),
        message: errorMessage,
      });
      res.send(html);
    });
}

app.listen(port, () => {
  console.log(`Up and running!`);
});
