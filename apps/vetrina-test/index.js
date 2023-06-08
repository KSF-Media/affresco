import React from "react";
import ReactDOM from "react-dom/client";
import "basscss/css/basscss-cp.css";

// yup, welcome to react 16
import createReactClass from "create-react-class";
React.createClass = createReactClass;
var Main = require("./output/VetrinaTest.Main/index.js");

function main() {
  const myComponent = (
    <Main.jsApp />
  );

  const root = ReactDOM.createRoot(document.getElementById("app"));
  root.render(myComponent);
}

if (module.hot) {
  module.hot.accept(function () {
    console.log("running main again");
    main();
  });
}

console.log("starting");
main();
