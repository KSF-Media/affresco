import React from 'react';
import render from 'react-dom';

import Table from './Table.js';
import Parliament from './Parliament.js';
import AreaPicker from './AreaPicker.js'
import Status from './Status.js';
import Chart from './Chart.js';

import '../assets/less/app.less';

import { getArea, getCountry } from './Backend';

export default class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      selectedArea: null,
      selectedAreaResponse: null,
      seats: {},
      votesInArea: [],
    };
  }
  componentDidMount() {
    this.onAreaSelection();
  }
  onAreaSelection(area) {
    console.info("onAreaSelection", area);
    if (!area) { // if no area was provided (e.g. on start or reset)
      return getCountry().then(area => { // get the whole country
        if (area) { return this.onAreaSelection(area) } // and roll with that instead
      })
    }
    let selectedArea = area;
    this.setState({ selectedArea })
    return getArea(selectedArea.info.identifier)
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
          onSelection={ (area) => this.onAreaSelection(area) }
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
