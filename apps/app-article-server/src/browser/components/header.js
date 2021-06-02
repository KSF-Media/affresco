import React, { Component } from "react";
import hblDefaultImage from "../assets/images/hbl-fallback-img.png";

const Header = (props) => {
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
          className={"caption"}
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
