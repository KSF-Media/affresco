import React from 'react';
import render from 'react-dom';

import Table from './Table.js';
import Parliament from './Parliament.js';

import '../assets/less/app.less';

import testData from './TestData';
import testSeats from './TestParties.js';

export default class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      data: testData,
      seats: testSeats
    }
  }
  render(){
    const { data } = this.state;
    const { seats } = this.state;

    return (
      <div className="ksf-elections">
        <Parliament
          seats={seats}
        />
        <Table
          data={data}
        />
      </div>
    )
  }
}
