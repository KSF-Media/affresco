import React from 'react';
import render from 'react-dom';

import Table from './Table.js';
import Parliament from './Parliament.js';
import AreaPicker from './AreaPicker.js'
import Status from './Status.js';
import Chart from './Chart.js';

import '../assets/less/app.less';

import testData from './TestData';
import testSeats from './TestParties.js';
import { getArea } from './Backend';

export default class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      selectedAreaId: "001",
      selectedAreaResponse: null,
      seats: {},
      votesInArea: [],
    };
  }
  componentDidMount() {
    getArea(this.state.selectedAreaId)
      .then(areaResponse => {

        // Get data for selected area
        this.setState({selectedAreaResponse: areaResponse});
        this.setState({votesInArea: areaResponse.nominators});

        // Make object for the parliamentSVG
        let s = { seats: areaResponse.nominators.map(
          nominator => {
            return {[nominator.abbreviation.finnish]: {
              'seats': nominator.seats
            }}
          }
        )};
        this.setState({ seats: Object.assign(...s.seats) });
      });
  }
  render(){
    return (
      <div className="ksf-elections">
        <Status
          percentage={78}
        />
        <Parliament
          seats={this.state.seats}
        />
        <AreaPicker
          onSelection={(option) => console.log("SELECTION", option) }
        />
        <Chart
          areaResponse={this.state.selectedAreaResponse}
        />
        <Table
          areaResponse={this.state.selectedAreaResponse}
        />
      </div>
    )
  }
}
