import React, { Component } from "react";
import shareIcon from "../assets/images/share.png";
import SvgIcon from "../svgIcon";
import soundIcon from "../assets/images/sound.png";
import { isDarkModeOn } from "../helper";

class Additional extends Component {
  constructor(props) {
    super(props);
  }

  componentDidMount() {}
  onShare() {
    Android.showIntent(process.env.SHARE_URL + this.state.uuid);
  }

  render() {
    return (
      <div className={"row"}>
        <div className="col-12">
          <p className={`preamble mt-3 ${isDarkModeOn() ? "darkMode" : ""}`}>{this.props.preamble}</p>
        </div>
      </div>
    );
  }
}

export default Additional;
