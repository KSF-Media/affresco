// src/index.js
import React from "react";
import ReactDOM from "react-dom";
import "@babel/polyfill";
import {HashRouter} from "react-router-dom";

import App from "./App.js";
import "./less/main.less"

ReactDOM.render(
  <HashRouter>
    <App />
  </HashRouter>,
  document.getElementById("ksf-podcasts")
);
