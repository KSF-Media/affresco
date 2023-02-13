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
            } additional-xs ${this.customStyle()}`}
          >
            {this.props.preamble}
          </p>
        </div>
      </div>
    );
  }

  customStyle() {
    const classNames = new Map();
    classNames.set("1.06", "additional-xs");
    classNames.set("1.5", "additional-sm");
    classNames.set("2.0", "additional-md");
    classNames.set("2.5", "additional-lg");
    classNames.set("3.0", "additional-xl");
    return classNames.get(this.props.fontSize);
  }
}

export default Additional;
