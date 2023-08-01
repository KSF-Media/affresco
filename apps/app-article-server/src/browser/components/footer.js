import React, { Component } from "react";

const getSiteDomainName = (brand) => {
  switch (brand) {
    case "vn":
      return "vastranyland"
    case "on":
      return "ostnyland"
    default:
      return "hbl"
  }
}

const Footer = (props) => {
  return (
    <div className={"footer"}>
      <a href={`https://www.${getSiteDomainName(props.brandValueName)}.fi/sida/kontakt`}>
        Ta kontakt
      </a>
      <br />
      <strong>KSF Media {new Date().getFullYear()}</strong>
      <p><small>KSF Media ägs av Konstsamfundet</small></p>
    </div>
  );
};

export default Footer;
