import React, { Component } from "react";
import quoteIcon from "../assets/images/quotes-png-11.png";

class Content extends Component {
  constructor(props) {
    super(props);
    this.state = {
      paper: props.paper || "hbl",
    };
  }

  conditionalRendering(block, key) {
    let blockType = Object.keys(block)[0].toLowerCase() || null;
    if (blockType != null) {
      if (blockType === "paragraph") {
	return this.renderParagraph(block, key);
      } else if (blockType === "headline") {
	return this.renderHeadline(block, key);
      } else if (blockType === "image") {
	return this.renderImage(block, key);
      } else if (blockType === "html") {
	return this.renderHtml(block, key);
      } else if (blockType === "factbox") {
	const factBox = {
	  box: {
	    headline: "FAKTA",
	    title: block.factBox.title,
	    content: block.factBox.content,
	    type: "fact",
	  },
	};
	return this.renderGenericBox(factBox, key);
      } else if (blockType === "box") {
	return this.renderGenericBox(block, key);
      } else if (blockType === "quote") {
	return this.renderQuotes(block, key);
      } else if (blockType === "question") {
	return this.renderQuestion(block, key);
      } else if (blockType === "footnote") {
	return this.renderFootnote(block, key);
      }
    } else {
      console.log("couldn't render " + { blockType });
    }
  }

  renderParagraph(block, key) {
    return (
      <p className={"paragraph"} key={key}>
	{htmlToReactParser.parse(block.paragraph)}
      </p>
    );
  }

  renderQuotes(block, key) {
    return (
      <div className={"quotePlaceHolder"} key={key}>
	<div className={"row"}>
	  <div className={"col-2 thojzat"}>
	    <img width="30px" src={quoteIcon} alt="" />
	  </div>
	  <div className={"col-10 quote"} style={{ paddingLeft: "0px" }}>
	    {block.quote}
	  </div>
	</div>
      </div>
    );
  }

  renderHeadline(block, key) {
    return (
      <h4
	className={`headline ${isDarkModeOn(this.props) ? "darkMode" : ""}`}
	key={key}
      >
	{block.headline}
      </h4>
    );
  }

  renderImage(block, key) {
    let appendBylineLabel = block.image.byline !== "" ? "BILD:" : "";
    let caption = block.image.caption === null ? "" : block.image.caption;
    let byline = block.image.byline === null ? "" : block.image.byline;
    return (
      <div className={"image"} key={key}>
	<img
	  className={"articleImage"}
	  width="100%"
	  src={block.image.url}
	  alt=""
	  onClick={() =>
	    this.props.showHighResolutionImage(
	      block.image.url,
	      caption + " " + appendBylineLabel + " " + byline
	    )
	  }
	/>
	<p
	  dangerouslySetInnerHTML={{
	    __html: caption + " " + appendBylineLabel + " " + byline,
	  }}
	/>
      </div>
    );
  }

  countBlockChatacters(block) {
    if (block) {
      const totalCharacters = block.reduce((acc, item) => {
	return acc + item.length;
      }, 0);
      return totalCharacters;
    }
    return 0;
  }

  renderGenericBox(block, key) {
    return (
      <div
	className={`genericBox ${isDarkModeOn(this.props) ? "darkMode" : ""} ${
	  this.countBlockChatacters(block.box.content) > 400
	    ? ""
	    : "genericBoxAutoHeight"
	} genericBox-border-${this.state.paper} `}
	key={key}
	id={"genericBox-" + key}
      >
	{block.box && block.box.headline ? (
	  <div
	    className={`genericBox-headline ${
	      isDarkModeOn(this.props) ? "darkMode" : ""
	    }`}
	  >
	    {block.box.headline}
	  </div>
	) : (
	  <div
	    className={`genericBox-headline ${
	      isDarkModeOn(this.props) ? "darkMode" : ""
	    }`}
	  >
	    {block.box.type === "fact" ? "FAKTA" : ""}
	  </div>
	)}
	<h3 class={isDarkModeOn(this.props) ? "faktBoxdarkMode" : ""}>
	  {block && block.box && block.box.title}
	</h3>
	<ul
	  className={`factboxList ${
	    isDarkModeOn(this.props) ? "darkMode" : ""
	  }`}
	>
	  {block.box &&
	    block.box.content.map((fact, key) => {
	      return (
		<li key={key} dangerouslySetInnerHTML={{ __html: fact }} />
	      );
	    })}
	</ul>

	{block &&
	  block.box &&
	  block.box.content &&
	  this.countBlockChatacters(block.box.content) > 400 && (
	    <div
	      className={`expand ${isDarkModeOn(this.props) ? "darkMode" : ""}`}
	      id={"expandFactBox-" + key}
	      onClick={() => {
		console.log("WÄT");
		this.expandFactBox(key);
	      }}
	    >
	      <div
		className={`expandOpacity ${
		  isDarkModeOn(this.props) ? "darkMode" : ""
		}`}
		id={"expandOpacity"}
	      ></div>
	      <div
		className={`expandOpacity2 ${
		  isDarkModeOn(this.props) ? "darkMode" : ""
		}`}
		id={"expandOpacity2"}
	      ></div>
	      <div
		class={`brandColor-${this.state.paper}`}
		style={{ display: "inline-block" }}
	      >
		<span>VIK UT</span>
	      </div>
	      <div style={{ display: "inline-block" }}>
		<i className={`arrow down border-${this.state.paper}`} />
	      </div>
	    </div>
	  )}
      </div>
    );
  }

  renderHtml(block, key) {
    return (
      <div
	className={`html ${isDarkModeOn(this.props) ? "darkMode" : ""}`}
	key={key}
	dangerouslySetInnerHTML={{ __html: block.html }}
      />
      //    {/*<div className={"html"} key={key}>{htmlToReactParser.parse(block.html)}</div>*/} // youplay videos does not work
    );
  }

  expandFactBox(key) {
    console.log("ayoo");
    document.getElementById("genericBox-" + key).style.height = "auto";
    document.getElementById("expandFactBox-" + key).style.display = "none";
    document.getElementById("expandOpacity").style.display = "none";
    document.getElementById("expandOpacity2").style.display = "none";
  }

  renderQuestion(block, key) {
    return (
      <h4 className={"headline headline-question"} key={key}>
	<i>{block.question}</i>
      </h4>
    );
  }

  renderFootnote(block, key) {
    return (
      <div className={"html text-footnote"} key={key}>
	<i>{block.footnote}</i>
      </div>
    );
  }

  render() {
    return (
      <div className={"row"}>
	<div
	  className={`col-sm-12 content text-left mt-2 ${this.state.paper}`}
	  id={"content"}
	  style={{ wordWrap: "break-word" }}
	>
	  <div id="MOBPARAD"></div>
	  {this.props.body != null
	    ? this.props.body.map((block, key) => {
		return this.conditionalRendering(block, key);
	      })
	    : ""}
	</div>
      </div>
    );
  }
}

export default Content;

function isDarkModeOn(props) {
  return props.mode === "dark";
}
