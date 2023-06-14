import React, { useEffect, useState } from "react";
import { LazyLoadImage } from "react-lazy-load-image-component";
const axios = require("axios");

export function AdvertorialLiftup({ darkModeEnabled, paper }) {
  const [advertorial, setAdvertorial] = useState(null);
  useEffect(() => {
    const advertorialsReq = process.env.LETTERA_URL + "/list/active-advertorial?paper=" + paper;
    axios(advertorialsReq).then((res) => {
      setAdvertorial(res.data[Math.floor(Math.random() * res.data.length)]);
    });
  }, []);
  if (!advertorial) return <div />;
  const image =
    advertorial.listImage.thumb ||
    advertorial.mainImage.thumb ||
    (paper === "ON" && "https://cdn.ksfmedia.fi/mosaico/on-og-fallback.png") ||
    (paper === "VN" && "https://cdn.ksfmedia.fi/mosaico/vn-og-fallback.png") ||
    "https://cdn.ksfmedia.fi/mosaico/hbl-og-fallback.png";
  return (
    <a className={`adv-link ${darkModeEnabled ? "darkMode" : ""}`} href={"/article/" + advertorial.uuid}>
      <div className="adv-annons-container">
        <span className={`adv-annons-text ${darkModeEnabled ? "darkMode" : ""}`}>
          {advertorial.company ? "ANNONS:" : "ANNONS"}
        </span>
        {(advertorial.company || "") && (
          <span className={`adv-annons-company ${darkModeEnabled ? "darkMode" : ""}`}>
            {advertorial.company.toUpperCase()}
          </span>
        )}
      </div>
      <div className="adv-image-container">
        <LazyLoadImage className={"adv-image"} width="100%" src={image} threshold="200" effect="opacity" alt="" />
      </div>
      <h2 className="adv-title">{advertorial.listTitle}</h2>
    </a>
  );
}
