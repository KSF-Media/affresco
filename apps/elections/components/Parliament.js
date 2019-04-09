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

    const totalSeats = 200;

    const reducer = (accumulator, key) => accumulator + seats[key].seats;
    const occupiedSeats = Object.keys(seats).reduce(reducer, 0);

    const seatsInParliement = {
      ...seats,
      'unfilled':
        {
          'seats': totalSeats-occupiedSeats,
        }
    };
    console.log("Seats in parliament", seatsInParliement);

    const svg = parliamentSVG(seatsInParliement, false);
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
