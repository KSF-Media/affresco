import React, { Component } from "react";
import { getBrandValueParam } from "../helper";

const Loading = () => {
  return (
    <div className="loading-wrapper">
      <div className={`loading loading-${getBrandValueParam()}`}></div>
    </div>
  );
};

export default Loading;
