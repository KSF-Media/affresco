import React from "react";
import ReactDOM from "react-dom";
import { BrowserRouter as Router } from "react-router-dom";

// yup, welcome to react 16
import createReactClass from "create-react-class";
React.createClass = createReactClass;
var Mosaico = require("../output/Mosaico/index.js");

function main() {
  const mosaico = <Mosaico.jsApp />;

  ReactDOM.render(mosaico, document.getElementById("app"));
}

if (module.hot) {
  module.hot.accept(function () {
    console.log("running main again");
    main();
  });
}

console.log("starting");
main();
