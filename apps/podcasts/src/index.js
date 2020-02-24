// src/index.js
import React from "react";
import ReactDOM from "react-dom";
import "@babel/polyfill";
import {HashRouter} from "react-router-dom";

import App from "./App.js";
import "./less/main.less"

ReactDOM.render(
  <HashRouter>
    <App userIds={[
      "705599305", // Hissapodden
      "630339678", // Samtal med vänner
      "542583531", // Queerents Podcast
      "694513583"  // Vetskap
    ]} />
  </HashRouter>,
  document.getElementById("ksf-podcasts")
);
