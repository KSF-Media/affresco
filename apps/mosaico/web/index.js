import React from "react";
import ReactDOM from "react-dom";
import { BrowserRouter as Router } from "react-router-dom";
import { rehydrateMarks } from "react-imported-component";

// yup, welcome to react 16
import createReactClass from "create-react-class";
React.createClass = createReactClass;
var Mosaico = require("../output/Mosaico/index.js").jsApp();

function main() {
  rehydrateMarks().then(() => {
    const mosaico = (
      <Mosaico
	article={window.article || null}
	isPreview={window.isPreview || null}
	mostReadArticles={window.mostReadArticles || null}
	frontpageArticles={window.frontpageArticles || null}
  staticPageContent={window.staticPageContent || null}
	tagListArticles={window.tagListArticles || null}
	tagListArticlesName={window.tagListArticlesName || null}
	scoredListCategory={window.scoredListCategory || null}
	scoredListArticles={window.scoredListArticles || null}
      />
    );
    ReactDOM.hydrate(mosaico, document.getElementById("app"));
  });
}

if (module.hot) {
  module.hot.accept(function () {
    console.log("running main again");
    main();
  });
}

console.log("starting");
main();
