import React from "react";
import { rehydrateMarks } from "react-imported-component";

import ReactDOM from "react-dom";
import "./index.css";
import Article from "./components/article";
import * as serviceWorker from "./serviceWorker";
if (window.article) {
  var body = [...window.article.body];
  var bodyElems = body.map((elem) => Object.keys(elem));
  var elemsCount = bodyElems.length;

  var ad1RoughPosition;
  var ad2RoughPosition;

  if (elemsCount > 15) {
    ad1RoughPosition = Math.floor(elemsCount / 3);
    ad2RoughPosition = Math.floor(elemsCount / 1.5);
  } else if (elemsCount > 6) {
    ad1RoughPosition = Math.floor(elemsCount / 2);
  }

  function findExactPositionForAd(roughPosition) {
    var position;
    for (let i = roughPosition; i < elemsCount; i++) {
      if (canAdGoHere(i)) {
        position = i;
        break;
      }
    }
    return position;
  }

  function canAdGoHere(i) {
    if (bodyElems[i - 1] == "html" && bodyElems[i] == "html") {
      return true;
    } else {
      return false;
    }
  }

  var ad1Position = findExactPositionForAd(ad1RoughPosition);
  var ad2Position = findExactPositionForAd(ad2RoughPosition);

  if (ad1Position && ad2Position) {
    body.splice(ad1Position, 0, { ad: "MOBMITT" });
    // +1 needed because inserted ad1 changes positioning after it
    body.splice(ad2Position+1, 0, { ad: "DIGIHELMOB" });
  } else if (ad1Position) {
    body.splice(ad1Position, 0, { ad: "MOBMITT" });
  }

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
        removeAds={window.article.removeAds}
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
