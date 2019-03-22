import React from 'react';
import render from 'react-dom';


export default class Parliament extends React.Component {
  constructor(props) {
    super(props);
  }

  render() {
    const parliamentSVG = require('parliament-svg');
    const toStr = require('virtual-dom-stringify');
    const { parties } = this.props;
    const svg = parliamentSVG(parties, false);
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
