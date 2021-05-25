import React, { Component } from "react";
import Card from "./card";

const isArray = (value) => {
  return value && typeof value === "object" && value.constructor === Array;
};

const TabletList = (props) => {
  let relatedArticles = [];
  if (isArray(props.articles)) {
    relatedArticles = props.articles.map((item, index) => {
      return (
	<div className={"col-4 mt-n1 "} style={{ padding: "5px" }} key={index}>
	  <Card
	    article={item}
	    darkModeEnabled={props.darkModeEnabled}
	    queryString={props.queryString}
	  />
	</div>
      );
    });
    return relatedArticles;
  }
};

export default TabletList;
