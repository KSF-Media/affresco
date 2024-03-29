import React, { Component, Suspense } from "react";
import hblDefaultImage from "../assets/images/hbl-fallback-img.png";
import Header from "./header";
import Additional from "./article-additional";
import ArticleDetails from "./article-details";
import Content from "./article-content";
import Footer from "./footer";
import Lightbox from "react-image-lightbox";
import "react-image-lightbox/style.css";
import { AdvertorialLiftup } from "./advertorial-liftup.js"

const MostReadArticles = React.lazy(() => import('./most-read-articles'))

const axios = require("axios");
var _ = require("lodash");

class Article extends Component {
  constructor(props) {
    super(props);
    const adsAreShown = this.props.articleType !== "Advertorial" && !this.props.removeAds;
    this.state = {
      modalCaption: "",
      isImageModalOpen: false,
      mostReadArticles: [],
      adsAreShown: adsAreShown,
      paper: props.paper || "hbl",
    };
  }

  componentDidMount() {
    if (this.props.darkModeEnabled) {
      document.getElementsByTagName("HTML")[0].setAttribute("data-theme", "dark");
    }
    this.getMostReadArticles();
    window.addEventListener("message", this.handleMessage);
  }

  componentWillUnmount() {
    window.removeEventListener("message", this.handleMessage);
  }

  // This listens for messages from DN interactive graphic embeds to get their height
  handleMessage = (event) => {
    if (event.data.location && event.data.ratio) {
      const iframeToTarget = document.querySelector(`iframe[src="${event.data.location}"]`);
      if (iframeToTarget) {
        iframeToTarget.style.height = `${iframeToTarget.clientWidth * event.data.ratio}px`;
      }
    }
  }

  showHighResolutionImage = (imgSrc, caption) => {
    this.setState({
      isImageModalOpen: true,
      modalImage: imgSrc,
      modalCaption: caption,
    });
  };

  getMostReadArticles() {
    const headers = {
      "content-type": "application/json",
      "Accept": "application/json",
    };
    const mostreadReq =
      process.env.LETTERA_URL +
      `/list/mostread?start=0&limit=11&paper=${this.props.paper}&onlySubscribers=${this.props.paper === "hbl"}`;
      axios.get(mostreadReq, {headers: headers}).then((res) => {
        this.setState(
          { mostReadArticles:
            res.data.filter(a => a.uuid != this.props.uuid).slice(0,10)
          }
        );
    });
  }

  renderFooterAd(adName) {
    if (this.state.adsAreShown) {
      return (
        <div className="ksf-app-ad" id={this.state.paper + "/" + this.state.paper + "_" + adName.toLowerCase()}></div>
      );
    }
  }

  render() {
    return (
      <div className={`article ${this.props.darkModeEnabled ? "darkMode" : ""}`} id={`${this.props.paper.toUpperCase()}`}>
        {this.state.isImageModalOpen && (
          <Lightbox
            mainSrc={this.state.modalImage + "&width=1200"}
            onCloseRequest={() => this.setState({ isImageModalOpen: false })}
            imageTitle={this.state.title}
            imageCaption={this.state.modalCaption}
            enableZoom={true}
          />
        )}

        <div className="container-fluid article">
          <div>
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
            <Additional
              preamble={this.props.preamble}
              increaseFontSize={this.increaseFontSize}
              fontSize={this.props.fontSize}
              darkModeEnabled={this.props.darkModeEnabled}
            />
            <Tag tags={this.props.tags} paper={this.props.paper} />
            <Header
              showHighResolutionImg={this.showHighResolutionImage}
              mainImage={this.props.mainImage}
              caption={_.get(this.props.mainImage, "caption") || ""}
              fontSize={this.props.fontSize}
              appendBylineLabel={
                _.has(this.props.mainImage, "byline") &&
                _.get(this.props.mainImage, "byline") !== null &&
                _.get(this.props.mainImage, "byline") !== ""
                  ? "BILD:"
                  : ""
              }
              byline={_.get(this.props.mainImage, "byline") || ""}
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
              isPreview={this.props.isPreview}
              showHighResolutionImage={this.showHighResolutionImage}
              fontSize={this.props.fontSize}
              darkModeEnabled={this.props.darkModeEnabled}
              articleType={this.props.articleType}
              queryString={this.props.queryString}
              adsAreShown={this.state.adsAreShown}
            />
            {this.state.adsAreShown && (
                <AdvertorialLiftup
                  darkModeEnabled={this.props.darkModeEnabled}
                  paper={this.props.paper}
                />
            )}
            {this.renderFooterAd("mobbox1")}
            {this.state.mostReadArticles.length > 0 ? (
              <Suspense fallback={<div>Laddar ...</div>}>
                <MostReadArticles
                  mostReadArticles={this.state.mostReadArticles}
                  queryString={this.props.queryString}
                  darkModeEnabled={this.props.darkModeEnabled}
                  paper={this.props.paper}
                />
              </Suspense>
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
  const classNames = new Map();
  classNames.set("1.06", "title-xs");
  classNames.set("1.5", "title-sm");
  classNames.set("2.0", "title-md");
  classNames.set("2.5", "title-lg");
  classNames.set("3.0", "title-xl");

  return (
    <div className={"row"}>
      <div className={"col-12 mt-3 mb-3"} style={{ wordWrap: "break-word" }}>
        <h2 className={`title ${props.darkModeEnabled ? "darkMode" : ""} ${classNames.get(props.fontSize)}`}>
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
      <div className={`col-12 mt-1 mb-3 articleTag brandColor-${props.paper}`}>{tag}</div>
    </div>
  );
};

export default Article;
