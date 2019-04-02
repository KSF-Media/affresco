import React from 'react';
import render from 'react-dom';

import Table from './Table.js';
import Parliament from './Parliament.js';
import AreaPicker from './AreaPicker.js'
import Status from './Status.js';

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
      seats: testSeats
    };
  }
  componentDidMount() {
    getArea(this.state.selectedAreaId)
      .then(areaResponse => {
        this.setState({selectedAreaResponse: areaResponse})
      });
  }
  render(){
    const { selectedAreaResponse } = this.state;
    const { seats } = this.state;

    return (
      <div className="ksf-elections">
        <Status
          percentage={78}
        />
        <Parliament
          seats={seats}
        />
        <AreaPicker
          onSelection={(option) => console.log("SELECTION", option) }
        />
        <Table
          areaResponse={selectedAreaResponse}
        />
      </div>
    )
  }
}
