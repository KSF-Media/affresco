import React, { Component } from "react";
import hblDefaultImage from "../assets/images/hbl-fallback-img.png";
import Header from "./header";
import Loading from "./loading";
import Additional from "./article-additional";
import PremiumBox from "./premium";
import ArticleDetails from "./article-details";
import Content from "./article-content";
import RelatedArticles from "./related-articles";
import Footer from "./footer";
import ManuallyRelatedArticles from "./manually-related-articles";
var _ = require("lodash");

class Article extends Component {
  constructor(props) {
    super(props);
    this.state = {
      title: props.title,
      relatedArticles: props.relatedArticles,
      tags: props.tags,
      isPreview: props.isPreview,
      mainImage: props.mainImage,
      body: props.body,
      preamble: props.preamble,
      articleType: props.articleType,
      articleTypeDetails: props.articleTypeDetails,
      publishingTime: props.publishingTime,
      updateTime: props.updateTime,
      auhtors: props.authors,
      premium: props.premium,
    };
  }
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

	<div
	  className={`container-fluid article ${
	    this.state.mode === "dark" ? "darkMode" : ""
	  }`}
	>
	  <div>
	    <Tag tags={this.state.tags} />
	    {this.state.articleType === "Advertorial" ? (
	      <div>
		<div className={"row"}>
		  <div class="advertorial-top-box">
		    <div class="advertorial-top-box-left">ANNONS</div>
		  </div>
		</div>
	      </div>
	    ) : (
	      ""
	    )}
	    <Title title={this.state.title} />
	    <Header
	      showHighResolutionImg={this.showHighResolutionImage}
	      mainImage={this.state.mainImage}
	      caption={_.get(this.state.mainImage, "caption")}
	      appendBylineLabel={this.state.appendBylineLabel}
	      byline={_.get(this.state.mainImage, "byline")}
	    />
	    <Additional
	      preamble={this.state.preamble}
	      increaseFontSize={this.increaseFontSize}
	    />
	    <ArticleDetails
	      category={this.state.articleType}
	      premium={this.state.premium}
	      authors={this.state.authors}
	      publishingTime={this.state.publishingTime}
	      updateTime={this.state.updateTime}
	      articleTypeDetails={this.state.articleTypeDetails}
	    />
	    <Content
	      body={this.state.body}
	      showHighResolutionImage={this.showHighResolutionImage}
	    />
	    <div className={"row"}>
	      <div className={"col-sm-12"}>
		{this.state.showBuyOption ? (
		  <PremiumBox showLogin={this.showLogin} />
		) : (
		  ""
		)}
	      </div>
	    </div>{" "}
	    {this.state.relatedArticles.length > 0 ? (
	      <ManuallyRelatedArticles
		manuallyRelatedArticles={this.state.relatedArticles}
	      />
	    ) : (
	      ""
	    )}
	    {/*
	      <RelatedArticles relatedArticles={this.state.mostReadArticles} />
	     */}
	  </div>
	</div>
	{!this.state.isLoading && <Footer brandValueName={this.state.brand} />}
      </div>
    );
  }
}

const ErrorPage = (props) => {
  return (
    <div className={"row"}>
      <div
	className={"col-12 mt-2 mt-5 text-center"}
	style={{ wordWrap: "break-word" }}
      >
	<h2 className={"title"}>{props.message}</h2>
      </div>
    </div>
  );
};

const Title = (props, state) => {
  return (
    <div className={"row"}>
      <div className={"col-12 mt-2 mb-3"} style={{ wordWrap: "break-word" }}>
	<h2 className={`title ${state.mode === "dark" ? "darkMode" : ""}`}>
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
      <div className={`col-12 mt-2 mb-1 articleTag brandColor-${props.brand}`}>
	{tag}
      </div>
    </div>
  );
};

export default Article;
