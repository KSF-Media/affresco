import React from 'react';
import render from 'react-dom';


export default class Parliament extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    const parliamentSVG = require('parliament-svg');
    const toStr = require('virtual-dom-stringify');
    const { seats } = this.props;
    const svg = parliamentSVG(seats, false);
    const getSvg = () => {
        return {
          __html: toStr(svg)
        }
    }

    return (
      <div
        className="parliament-spread"
        dangerouslySetInnerHTML={getSvg()}
      >
      </div>
    )
  }

}
