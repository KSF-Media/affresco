import React from "react";

const AdvertorialLiftup = (props) => {
  return (
    <a className={`adv-link ${props.darkModeEnabled ? "darkMode" : ""}`}
       href={"/article/" + props.uuid}>
      <div className="adv-annons-container">
        <span className={`adv-annons-text ${props.darkModeEnabled ? "darkMode" : ""}`}>
          {!props.company ? "ANNONS" : "ANNONS:"}
        </span>
        {!props.company ? "" : (
          <span className={`adv-annons-company ${props.darkModeEnabled ? "darkMode" : ""}`}>
            {props.company.toUpperCase()}
          </span>
        )}
      </div>
      <div className="adv-image-container">
        <img className="adv-image"
             src={props.image}
             alt=""
        />
      </div>
      <h2 className="adv-title">
        {props.listTitle}
      </h2>
    </a>
  );
};

export default AdvertorialLiftup;
