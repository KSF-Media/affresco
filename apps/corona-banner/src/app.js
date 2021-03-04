import React from "react";
import CoronaSvg from "./assets/covid-virus-icon.svg";
// import "tailwindcss/tailwind.css";

export default function App() {
  return (
    <div className="corona-container">
      <header className="container-header">
        <h1 className="banner-title text-sm">
          Covid-19 <br className="mobile-hidden" /> i Finland
        </h1>{" "}
        <img className="virus-image" src={CoronaSvg} alt="Coronavirus cell" />
      </header>
      <div>
        <div className="stat-value">590</div>
        <div className="stat-label">Smittade Nu</div>
      </div>
      <div>
        <div className="stat-value">186</div>
        <div className="stat-label">Pa Sjukhus</div>
      </div>
      <div>
        <div className="stat-value">734</div>
        <div className="stat-label">Avliona</div>
      </div>
      <div>
        <div className="stat-value">
          288476 <span className="stat-percent">(5,2%)</span>
        </div>
        <div className="stat-label">Vaconerade</div>
      </div>
    </div>
  );
}
