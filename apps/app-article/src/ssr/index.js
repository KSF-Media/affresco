import React from "react";
import { rehydrateMarks } from "react-imported-component";

import ReactDOM from "react-dom";
import "../index.css";
import Article from "../components/article";
import * as serviceWorker from "../serviceWorker";
rehydrateMarks().then(() => {
  ReactDOM.hydrate(
    <Article
      title={window.article.title}
      mainImage={window.article.mainImage}
      body={window.article.body}
      tags={window.article.tags || []}
      relatedArticles={window.article.relatedArticles || []}
      preamble={window.article.preamble}
      articleType={window.article.articleType}
      articleTypeDetails={window.article.articleTypeDetails}
      publishingTime={window.article.publishingTime}
      updateTime={window.article.updateTime}
      authors={window.article.authors}
      premium={window.article.premium}
      isPreview={window.article.isPreview}
      mostReadArticles={window.article.mostReadArticles}
      fontSize={window.article.fontSize}
      darkModeEnabled={window.article.darkModeEnabled}
      queryString={window.article.queryString}
    />,
    document.getElementById("root")
  );
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: http://bit.ly/CRA-PWA
serviceWorker.unregister();
