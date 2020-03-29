// src/index.js
import React from "react";
import ReactDOM from "react-dom";
import "@babel/polyfill";

import App from "./App.js";
import "./less/main.less"

ReactDOM.render(
  <App />,
  document.getElementById("ksf-podcasts")
);
