import React, { Component } from "react";
import shareIcon from "../assets/images/share.png";
import SvgIcon from "../svgIcon";
import soundIcon from "../assets/images/sound.png";

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
	  <p
	    className={`preamble mt-3 ${
	      this.props.darkModeEnabled ? "darkMode" : ""
	    }`}
	    style={this.customStyle()}
	  >
	    {this.props.preamble}
	  </p>
	</div>
      </div>
    );
  }

  customStyle() {
    return this.props.fontSize
      ? { fontSize: this.props.fontSize + 0.05 + "rem", lineHeight: "120%" }
      : {};
  }
}

export default Additional;
