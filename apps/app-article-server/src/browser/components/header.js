import React, { Component } from "react";
import hblDefaultImage from "../assets/images/hbl-fallback-img.png";

const Header = (props) => {
  const classNames = new Map();
  classNames.set("1.06", "caption-xs");
  classNames.set("1.5", "caption-sm");
  classNames.set("2.0", "caption-md");
  classNames.set("2.5", "caption-lg");
  classNames.set("3.0", "caption-xl");
  return (
    <div className={"row"}>
      <div>
        {props.mainImage != null ? (
          <img
            className={"header headerImage"}
            onClick={() =>
              props.showHighResolutionImg(
                props.mainImage.url,
                props.caption + " " + props.appendBylineLabel + " " + props.byline
              )
            }
            src={isFallbackImage(props.mainImage.url) ? props.mainImage.url : props.mainImage.url + "&width=1400"}
            alt=""
          />
        ) : (
          ""
        )}
      </div>
      <div className={"col-12"}>
        <p
          className={`caption ${classNames.get(props.fontSize)}`}
          dangerouslySetInnerHTML={{
            __html: props.caption + " " + props.appendBylineLabel + " " + props.byline,
          }}
        />
      </div>
    </div>
  );
};

function isFallbackImage(url) {
  return url === hblDefaultImage;
}

export default Header;
