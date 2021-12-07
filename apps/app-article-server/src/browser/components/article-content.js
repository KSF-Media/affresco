import React, { Component } from "react";
import quoteIcon from "../assets/images/quotes-png-11.png";
const _ = require("lodash");

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
        className={`headline ${this.props.darkModeEnabled ? "darkMode" : ""}`}
        key={key}
        style={this.headlineFontSize()}
      >
        {block.headline}
      </h4>
    );
  }

  headlineFontSize() {
    return this.props.fontSize ? { fontSize: this.props.fontSize + 0.4 + "rem" } : {};
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
            this.props.showHighResolutionImage(block.image.url, caption + " " + appendBylineLabel + " " + byline)
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
        className={`genericBox ${this.props.darkModeEnabled ? "darkMode" : ""} ${
          this.countBlockChatacters(block.box.content) > 400 ? "" : "genericBoxAutoHeight"
        } genericBox-border-${this.state.paper} `}
        key={key}
        id={"genericBox-" + key}
      >
        {block.box && block.box.headline ? (
          <div className={`genericBox-headline ${this.props.darkModeEnabled ? "darkMode" : ""}`}>
            {block.box.headline}
          </div>
        ) : (
          <div className={`genericBox-headline ${this.props.darkModeEnabled ? "darkMode" : ""}`}>
            {block.box.type === "fact" ? "FAKTA" : ""}
          </div>
        )}
        <h3 className={this.props.darkModeEnabled ? "faktBoxdarkMode" : ""}>{block && block.box && block.box.title}</h3>
        <ul className={`factboxList ${this.props.darkModeEnabled ? "darkMode" : ""}`}>
          {block.box &&
            block.box.content.map((fact, key) => {
              return <li key={key} dangerouslySetInnerHTML={{ __html: fact }} />;
            })}
        </ul>

        {block && block.box && block.box.content && this.countBlockChatacters(block.box.content) > 400 && (
          <div
            className={`expand ${this.props.darkModeEnabled ? "darkMode" : ""}`}
            id={"expandFactBox-" + key}
            onClick={() => {
              this.expandFactBox(key);
            }}
          >
            <div className={`expandOpacity ${this.props.darkModeEnabled ? "darkMode" : ""}`} id={"expandOpacity"}></div>
            <div
              className={`expandOpacity2 ${this.props.darkModeEnabled ? "darkMode" : ""}`}
              id={"expandOpacity2"}
            ></div>
            <div className={`brandColor-${this.state.paper}`} style={{ display: "inline-block" }}>
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
        className={`html ${this.props.darkModeEnabled ? "darkMode" : ""}`}
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
          className={`col-sm-12 content text-left mt-2 ${this.state.paper} ${this.contentStyles()}`}
          id={"content"}
          style={_.merge({ wordWrap: "break-word" })}
        >
          <div id="MOBPARAD"></div>
          {this.props.body != null
            ? this.props.body.map((block, key) => {
                return this.conditionalRendering(block, key);
              })
            : ""}
          <div id="MOBNER"></div>
        </div>
      </div>
    );
  }

  contentStyles() {
    const classNames = new Map();
    classNames.set("1.06", "content-xs");
    classNames.set("1.5", "content-sm");
    classNames.set("2.0", "content-md");
    classNames.set("2.5", "content-lg");
    classNames.set("3.0", "content-xl");
    return classNames.get(this.props.fontSize);
  }
}

export default Content;
