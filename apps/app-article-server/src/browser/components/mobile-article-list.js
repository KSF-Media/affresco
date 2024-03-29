import React, { Component } from "react";
import { LazyLoadImage, trackWindowScroll } from "react-lazy-load-image-component";
import hblDefaultImage from "../assets/images/hbl-fallback-img.png";
const _ = require("lodash");

const isArray = (value) => {
  return value && typeof value === "object" && value.constructor === Array;
};

const getTag = (tags) => {
  let tag = "";
  if (tags.length > 0) {
    tag = tags[0];
  }
  return tag;
};

const formatTime = (date) => {
  let publishingDate = new Date(date);
  let newPublishingDate =
    publishingDate.getDate() +
    "." +
    (publishingDate.getMonth() + 1) +
    "." +
    publishingDate.getFullYear() +
    " " +
    (publishingDate.getHours() < 10 ? "0" : "") +
    publishingDate.getHours() +
    ":" +
    (publishingDate.getMinutes() < 10 ? "0" : "") +
    publishingDate.getMinutes();
  let todaysDate = new Date();
  let publishedTime =
    (publishingDate.getHours() < 10 ? "0" : "") +
    publishingDate.getHours() +
    ":" +
    (publishingDate.getMinutes() < 10 ? "0" : "") +
    publishingDate.getMinutes();

  if (publishingDate.setHours(0, 0, 0, 0) === todaysDate.setHours(0, 0, 0, 0)) {
    newPublishingDate = publishedTime;
  }
  return newPublishingDate;
};

const MobileList = (props) => {
  let relatedArticles = [];
  if (isArray(props.articles)) {
    relatedArticles = props.articles.map((item, index) => {
      return (
        <React.Fragment key={index}>
          <div className={"articleItem listItems"}>
            <div className={"row"}>
              <a href={"/article/" + item.uuid + props.queryString} className={"col-8"}>
                  <div className="listTitle">
                      {item.title.length > 80 ? item.title.substring(0, 80) + "..." : item.title}
                  </div>
                  <div className={"articleItemDetails"}>
                    <div className={`category brandColor-${props.paper}`}>{getTag(item.tags)}</div>
                    <div className={"date"}>{formatTime(item.publishingTime)}</div>
                  </div>
              </a>
              <a href={"/article/" + item.uuid + props.queryString} className={"col-4"}>
                <div className="listImageContainer">
                  {item.listImage === null ? (
                    <img className="listImage" src={hblDefaultImage} alt="" />
                  ) : (
                    <>
                    <LazyLoadImage
                      src={item.listImage.url.includes("imengine")
                        ? item.listImage.url + "&function=hardcrop&width=360&height=200&q=65"
                        : item.listImage.tinyThumb
                      }
                      className="listImage"
                      scrollPosition={props.scrollPosition}
                      placeholderSrc={hblDefaultImage}
                      effect="opacity"
                    />
                    </>
                  )}
                </div>
              </a>
            </div>
          </div>
        </React.Fragment>
      );
    });

    return relatedArticles;
  }
};

export default trackWindowScroll(MobileList);
