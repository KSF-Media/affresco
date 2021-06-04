import React from "react";
import ReactDOM from "react-dom";

console.log("index.js");

var Box = require("./output/Mosaico.Article.Box/index.js");

ReactDOM.hydrate(
  <Box.box
    headline="Hydrated Headline"
    title="Hydrated Title"
    content={["Hydrated Content 1", "Hydrated Content 2"]}
    brand="hbl"
  />,
  document.querySelector("div.factbox")
);

// ReactDOM.render(
//   <p>yolo</p>,
//   document.querySelector("div.factbox")
// );
