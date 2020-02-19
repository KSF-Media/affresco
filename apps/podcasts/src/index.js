// src/index.js
import React from "react";
import ReactDOM from "react-dom";
import PodList from "./PodList.js";
import "@babel/polyfill";
import "./less/main.less"

ReactDOM.render(
  <PodList userIds={["705599305","630339678","542583531","694513583"]} />,
  document.getElementById("ksf-podcasts")
);
