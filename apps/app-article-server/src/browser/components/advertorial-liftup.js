import React, { useEffect, useState } from "react";
const axios = require("axios");

export function AdvertorialLiftup({ darkModeEnabled, paper }) {
  const [advertorial, setAdvertorial] = useState(null);
  useEffect(() => {
    const advertorialsReq = process.env.LETTERA_V4_URL + "/list/active-advertorial?paper=" + paper;
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
        <img className="adv-image" src={image} alt="" />
      </div>
      <h2 className="adv-title">{advertorial.listTitle}</h2>
    </a>
  );
}
