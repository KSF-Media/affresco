import React, { Component } from "react";
import MobileList from "./mobile-article-list";

class MostReadArticles extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    return (
      <div className={"relatedArticles"}>
        <div className={"row"}>
          <div className={"col-12"}>
            <h3 className={`latest ${this.props.darkModeEnabled ? "darkMode" : ""}`}>Andra läser</h3>
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
      </div>
    );
  }
}

export default MostReadArticles;
