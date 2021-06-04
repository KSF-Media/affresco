import React from "react";
import ReactDOM from "react-dom";
import { BrowserRouter as Router } from "react-router-dom";
import "basscss/css/basscss-cp.css";

// yup, welcome to react 16
import createReactClass from "create-react-class";
React.createClass = createReactClass;
var Main = require("./output/MittKonto.Main/index.js");

function main() {
  const myComponent = (
    <Router>
      <Main.jsApp />
    </Router>
  );

  ReactDOM.render(myComponent, document.getElementById("app"));
}

if (module.hot) {
  module.hot.accept(function () {
    console.log("running main again");
    main();
  });
}

console.log("starting");
main();
