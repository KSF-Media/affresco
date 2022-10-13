import React from "react";
import { rehydrateMarks } from "react-imported-component";

import ReactDOM from "react-dom";
import "./index.css";
import Article from "./components/article";
import * as serviceWorker from "./serviceWorker";
if (window.article) {
  var body = [...window.article.body];
  body.splice(2, 0, { ad: "MOBMITT" });
  body.splice(8, 0, { ad: "DIGIHELMOB" });

  rehydrateMarks().then(() => {
    ReactDOM.hydrate(
      <Article
        articleType={window.article.articleType}
        articleTypeDetails={window.article.articleTypeDetails}
        authors={window.article.authors}
        body={body}
        darkModeEnabled={window.article.darkModeEnabled}
        fontSize={window.article.fontSize}
        isPreview={window.article.isPreview}
        mainImage={window.article.mainImage}
        mostReadArticles={window.article.mostReadArticles}
        paper={window.article.paper}
        preamble={window.article.preamble}
        premium={window.article.premium}
        publishingTime={window.article.publishingTime}
        queryString={window.article.queryString}
        relatedArticles={window.article.relatedArticles || []}
        tags={window.article.tags || []}
        title={window.article.title}
        updateTime={window.article.updateTime}
      />,
      document.getElementById("root")
    );
  });
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: http://bit.ly/CRA-PWA
serviceWorker.unregister();
