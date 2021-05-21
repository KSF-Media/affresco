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
app.use("/dist", express.static(`${__dirname}/../client`));

app.get("/*", (req, res) => {
  axios
    .get(
      "https://lettera.api.ksfmedia.fi/v3/article/47b04f5b-b5b2-43c7-acb5-b95b24cf6784"
    )
    .then((response) => {
      const article = (
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
      const markup = ReactDOM.renderToString(article);

      console.log(response);
      const html = generateHtml(markup);
      res.send(html);
    });
});

// respond with "hello world" when a GET request is made to the homepage
// app.get("/article/:articleId", function (req, res) {
//   res.send(ReactDOMServer.renderToString(App.App()));
// });

app.listen(port, () => {
  console.log(`Example app listening at http://localhost:${port}`);
});
