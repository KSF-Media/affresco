import React, { Component } from "react";
const _ = require("lodash");
import * as cheerio from 'cheerio';
import PremiumBox from "./premium";

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
      if (blockType === "ad") {
        return this.renderAd(block, key);
      }
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

  getBrandQuoteIcon(paper) {
    switch (paper) {
      case ("on"):
        return quoteIconON
      case ("vn"):
        return quoteIconVN
      default:
        return quoteIconHBL
    }
  }

  renderQuotes(block, key) {
    return (
      <div className={"quotePlaceHolder"} key={key}>
        <div className={"row"}>
          <div className={"col-2 thojzat quote-" + this.state.paper}>
            <svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlnsXlink="http://www.w3.org/1999/xlink" x="0px" y="0px"
    width="100%" viewBox="0 0 359 326" xmlSpace="preserve">
              <path shapeRendering="geometricPrecision" opacity="1.000000" stroke="none"
                d="
                M216.255249,304.102295
                C201.816803,312.021271 205.385315,312.756470 198.310562,300.092773
                C192.656418,289.971954 193.157227,290.115265 202.665131,282.867554
                C214.933395,273.515656 226.930527,263.793243 238.794312,253.929565
                C251.662842,243.230545 263.571503,231.504700 273.048370,217.623291
                C281.470947,205.286148 288.025635,192.021744 287.127686,176.568863
                C286.548859,166.607986 278.619049,159.541824 268.182281,160.561035
                C254.609436,161.886505 240.968948,163.868225 227.520767,159.001602
                C210.606705,152.880737 200.127853,140.538788 193.225113,124.578003
                C188.907608,114.594894 187.315735,103.849968 187.440338,93.251686
                C187.661819,74.414818 193.015533,57.303493 206.541092,43.006817
                C218.148422,30.737732 232.064362,24.880281 248.452927,23.002638
                C269.783539,20.558786 289.674103,23.647978 307.628326,36.110722
                C321.886475,46.007893 331.062988,59.837055 337.735168,75.433334
                C345.246643,92.991470 347.483917,111.621666 346.687958,130.500412
                C345.916321,148.802063 341.270050,166.365326 334.088989,183.224060
                C325.513123,203.357315 315.360535,222.724228 300.731873,239.058868
                C290.657349,250.308304 279.238831,260.457611 267.738831,270.301636
                C252.159119,283.637939 234.605118,294.188171 216.255249,304.102295
              z"/>
              <path shapeRendering="geometricPrecision" opacity="1.000000" stroke="none"
                d="
                M19.836990,109.253517
                C15.946694,87.355743 19.138172,67.367111 32.103050,49.257465
                C43.329044,33.576740 58.690029,25.588673 77.488106,23.128525
                C91.429176,21.304026 105.056419,22.045086 118.565292,26.118174
                C127.341446,28.764288 135.157776,33.067894 142.064056,38.912651
                C162.194580,55.949032 172.327606,78.121834 175.918335,103.965393
                C180.129181,134.272247 174.163895,162.629517 161.823486,190.195160
                C149.509766,217.701233 132.601501,241.948196 109.576088,261.226532
                C94.444328,273.895813 77.695885,284.677887 61.347897,295.830872
                C54.100014,300.775543 46.022888,304.496613 38.384430,308.881531
                C35.857143,310.332306 34.325047,310.223602 32.929047,307.314148
                C30.659742,302.584625 28.190525,297.917938 25.389620,293.487793
                C23.423546,290.378052 24.696894,288.575226 27.004663,286.838379
                C37.869270,278.661621 48.888294,270.683807 59.604576,262.317780
                C74.074471,251.021347 87.511971,238.683823 98.892845,224.102295
                C106.972282,213.750626 113.551353,202.782501 116.705894,190.034332
                C118.532089,182.654251 119.385498,175.106888 115.221123,167.991547
                C112.412750,163.193100 105.310081,159.940475 99.771439,160.408752
                C91.200890,161.133331 82.584038,161.975830 74.005867,161.806030
                C56.178204,161.453171 42.272221,153.212555 31.824890,138.989319
                C25.398611,130.240448 21.449877,120.435944 19.836990,109.253517
              z"/>
            </svg>
          </div>
          <div className={`col-10 quote ${this.props.darkModeEnabled ? "darkMode" : ""}`} style={{ paddingLeft: "0px" }}>
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
            onClick={(ev) => {
              this.expandFactBox(ev);
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
    if(
      block.html.includes("hbl.fi/artikel/")
      || block.html.includes("ostnyland.fi/artikel/")
      || block.html.includes("vastranyland.fi/artikel/")
    ) {
      block.html = this.handleInternalLinks(block.html)
    }
    return (
      <div
        className={`html ${this.props.darkModeEnabled ? "darkMode" : ""}`}
        key={key}
        dangerouslySetInnerHTML={{ __html: block.html }}
      />
      //    {/*<div className={"html"} key={key}>{htmlToReactParser.parse(block.html)}</div>*/} // youplay videos does not work
    );
  }

  handleInternalLinks(htmlString) {
    const validHostnames = ['www.hbl.fi', 'hbl.fi', 'www.ostnyland.fi', 'ostnyland.fi', 'www.vastranyland.fi', 'vastranyland.fi']

    const $ = cheerio.load(htmlString, null, false)
    const $links = $('a')
    $links.each((i, el) => {
      const $el = $(el)
      const urlObj = new URL($el.attr('href'))

      if(validHostnames.includes(urlObj.hostname) && urlObj.pathname.startsWith('/artikel/')) {
        let linkUuid = urlObj.pathname.slice('/article/'.length)
        linkUuid = linkUuid.endsWith('/') ? linkUuid.slice(0, -1) : linkUuid

        $el.attr('href', '/article/' + linkUuid + this.props.queryString)
      }
    })
    return $.html()
  }

  expandFactBox(ev) {
    const key = /\d+/.exec(ev.currentTarget.id)[0];
    document.getElementById("genericBox-" + key).style.height = "auto";
    document.getElementById("expandFactBox-" + key).style.display = "none";
    document.getElementById("expandOpacity").style.display = "none";
    document.getElementById("expandOpacity2").style.display = "none";
  }

  renderQuestion(block, key) {
    return (
      <h4 className={`headline headline-question ${this.props.darkModeEnabled ? "darkMode" : ""}`} key={key}>
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

  renderAd(block, key) {
    if (this.props.adsAreShown) {
      return (
        <p
          key={key}
          className="ksf-app-ad"
          id={this.state.paper + "/" + this.state.paper + "_" + block.ad.toLowerCase()}
        ></p>
      );
    } else {
      console.log("Ads have been removed for this article.");
    }
  }

  renderAdOutsideMainBlock(adName) {
    if (this.props.adsAreShown) {
      return (
        <div className="ksf-app-ad" id={this.state.paper + "/" + this.state.paper + "_" + adName.toLowerCase()}></div>
      );
    } else {
      console.log("Ads have been removed for this article.");
    }
  }

  render() {
    return (
      <div className={"row"}>
        <div
          className={`col-sm-12 content text-left mt-2 ${this.state.paper} content-xs ${this.contentStyles()}`}
          id={"content"}
          style={_.merge({ wordWrap: "break-word" })}
        >
          {this.renderAdOutsideMainBlock("mobparad")}
          {this.props.body != null
            ? this.props.body.map((block, key) => {
                return this.conditionalRendering(block, key);
              })
            : ""}
          <div className={"row"}>
            {this.props.isPreview
              ? <div className={"col-sm-12 fade-premium"}><PremiumBox paper={this.props.paper} /></div>
              : ""
            }
          </div>
          {this.renderAdOutsideMainBlock("mobbox1")}
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
