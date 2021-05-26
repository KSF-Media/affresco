import React, { Component, Fragment } from "react";
import hblDefaultImage from "../assets/images/hbl-fallback-img.png";
import MobileList from "./moble-article-list";
import TabletList from "./tablet-article-list";

class MostReadArticles extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <div className={"relatedArticles"}>
	<div className={"row"}>
	  <div className={"col-12"}>
	    <h3
	      className={`latest ${
		this.props.darkModeEnabled ? "darkMode" : ""
	      }`}
	    >
	      Andra läser
	    </h3>
	  </div>
	</div>
	<div className={"mobileView"}>
	  <MobileList
	    articles={this.props.mostReadArticles}
	    darkModeEnabled={this.props.darkModeEnabled}
	    queryString={this.props.queryString}
	    paper={this.props.paper}
	  />
	</div>

	<div className={"tabletView"}>
	  <div className={"row articleItem"}>
	    <TabletList
	      articles={this.props.mostReadArticles}
	      darkModeEnabled={this.props.darkModeEnabled}
	      queryString={this.props.queryString}
	      paper={this.props.paper}
	    />
	  </div>
	</div>
      </div>
    );
  }
}

export default MostReadArticles;
