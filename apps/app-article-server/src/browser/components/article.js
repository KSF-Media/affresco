import React, { Component } from "react";
import hblDefaultImage from "../assets/images/hbl-fallback-img.png";
import Header from "./header";
import Additional from "./article-additional";
import PremiumBox from "./premium";
import ArticleDetails from "./article-details";
import Content from "./article-content";
import MostReadArticles from "./most-read-articles";
import Footer from "./footer";
import RelatedArticles from "./related-articles";
import Lightbox from "react-image-lightbox";
import "react-image-lightbox/style.css";

var _ = require("lodash");

class Article extends Component {
  constructor(props) {
    super(props);
    this.state = {
      modalCaption: "",
      isImageModalOpen: false,
    };
  }

  showHighResolutionImage = (imgSrc, caption) => {
    this.setState({
      isImageModalOpen: true,
      modalImage: imgSrc,
      modalCaption: caption,
    });
  };

  render() {
    return (
      <div className="article">
        {this.state.isImageModalOpen && (
          <Lightbox
            mainSrc={this.state.modalImage + "&width=1200"}
            onCloseRequest={() => this.setState({ isImageModalOpen: false })}
            imageTitle={this.state.title}
            imageCaption={this.state.modalCaption}
            enableZoom={true}
          />
        )}

        <div className={`container-fluid article ${this.props.darkModeEnabled ? "darkMode" : ""}`}>
          <div>
            <Tag tags={this.props.tags} paper={this.props.paper} />
            {this.props.articleType === "Advertorial" ? (
              <div>
                <div className={"row"}>
                  <div className="advertorial-top-box">
                    <div className="advertorial-top-box-left">ANNONS</div>
                  </div>
                </div>
              </div>
            ) : (
              ""
            )}
            <Title
              title={this.props.title}
              fontSize={this.props.fontSize}
              darkModeEnabled={this.props.darkModeEnabled}
            />
            <Header
              showHighResolutionImg={this.showHighResolutionImage}
              mainImage={this.props.mainImage}
              caption={_.get(this.props.mainImage, "caption") || ""}
              appendBylineLabel={
                _.has(this.props.mainImage, "byline") &&
                _.get(this.props.mainImage, "byline") !== null &&
                _.get(this.props.mainImage, "byline") !== ""
                  ? "BILD:"
                  : ""
              }
              byline={_.get(this.props.mainImage, "byline") || ""}
            />
            <Additional
              preamble={this.props.preamble}
              increaseFontSize={this.increaseFontSize}
              fontSize={this.props.fontSize}
              darkModeEnabled={this.props.darkModeEnabled}
            />
            <ArticleDetails
              category={this.props.articleType}
              premium={this.props.premium}
              authors={this.props.authors}
              publishingTime={this.props.publishingTime}
              updateTime={this.props.updateTime}
              articleTypeDetails={this.props.articleTypeDetails}
              paper={this.props.paper}
            />
            <Content
              body={this.props.body}
              paper={this.props.paper}
              showHighResolutionImage={this.showHighResolutionImage}
              fontSize={this.props.fontSize}
              darkModeEnabled={this.props.darkModeEnabled}
            />
            <div className={"row"}>
              <div className={"col-sm-12"}>{this.props.isPreview ? <PremiumBox paper={this.props.paper} /> : ""}</div>
            </div>{" "}
            {this.props.relatedArticles.length > 0 ? (
              <RelatedArticles
                relatedArticles={this.props.relatedArticles}
                queryString={this.props.queryString}
                darkModeEnabled={this.props.darkModeEnabled}
                paper={this.props.paper}
              />
            ) : (
              ""
            )}
            {this.props.mostReadArticles.length > 0 ? (
              <MostReadArticles
                mostReadArticles={this.props.mostReadArticles}
                queryString={this.props.queryString}
                darkModeEnabled={this.props.darkModeEnabled}
                paper={this.props.paper}
              />
            ) : (
              ""
            )}
          </div>
        </div>
        <Footer brandValueName={this.props.paper} />
      </div>
    );
  }
}

const Title = (props, state) => {
  const fontSizeStyle = () => (props.fontSize ? { fontSize: props.fontSize + 1 + "rem", lineHeight: "100%" } : {});
  return (
    <div className={"row"}>
      <div className={"col-12 mt-2 mb-3"} style={{ wordWrap: "break-word" }}>
        <h2 className={`title ${props.darkModeEnabled ? "darkMode" : ""}`} style={fontSizeStyle()}>
          {props.title}
        </h2>
      </div>
    </div>
  );
};

const Tag = (props) => {
  let tags = props.tags;
  let tag = "";
  if (tags.length > 0) {
    tag = tags[0];
  }

  return (
    <div className={"row"}>
      <div className={`col-12 mt-2 mb-1 articleTag brandColor-${props.paper}`}>{tag}</div>
    </div>
  );
};

export default Article;
