import React from "react";
import { rehydrateMarks } from "react-imported-component";

import ReactDOM from "react-dom";
import "../index.css";
import Article from "../components/article";
// import * as serviceWorker from "./serviceWorker";
rehydrateMarks().then(() => {
  ReactDOM.hydrate(<Article />, document.getElementById("root"));
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: http://bit.ly/CRA-PWA
serviceWorker.unregister();
