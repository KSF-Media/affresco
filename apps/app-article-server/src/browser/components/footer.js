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
      <strong>Hufvudstadsbladet Ab {new Date().getFullYear()}</strong>
      <p class="company-info"><small>Hufvudstadsbladet Ab ger ut Hufvudstadsbladet, Västra Nyland, Östnyland och HBL Junior.</small></p>
      <p class="company-info"><small>Hufvudstadsbladet Ab ägs av Bonnier News och Konstsamfundet.</small></p>
    </div>
  );
};

export default Footer;
