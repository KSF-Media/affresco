import React from "react";
import CoronaSvg from "./assets/covid-virus-icon.svg";
import Chevron from "../../../images/chevron.svg";

export default function App() {
  return (
    <div className="corona-container">
      <header className="container-header">
        <h1 className="banner-title">
          Covid-19 <br /> i Finland
        </h1>
        <img className="virus-image" src={CoronaSvg} alt="Coronavirus cell" />
      </header>
      <div className="stat">
        <div className="stat-value">590</div>
        <div className="stat-label">smittade nu</div>
      </div>
      <div className="stat mobile-hidden">
        <div className="stat-value">186</div>
        <div className="stat-label">på sjukhus</div>
      </div>
      <div className="stat">
        <div className="stat-value">734</div>
        <div className="stat-label">avlidna</div>
      </div>
      <div className="stat">
        <div className="stat-value">
          288476 <span className="stat-percent">(5,2%)</span>
        </div>
        <div className="stat-label">vaccinerade</div>
      </div>
      <div className="chevron-container">
        <a>
          <img className="chevron-right" src={Chevron} alt="" />
        </a>
      </div>
    </div>
  );
}
