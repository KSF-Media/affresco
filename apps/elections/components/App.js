import React from 'react';
import render from 'react-dom';

import Table from './Table.js';
import Parliament from './Parliament.js';

import '../assets/less/app.less';

import testData from './TestData';
import testParties from './TestParties.js';

export default class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      data: testData,
      parties: testParties
    }
  }
  render(){
    const { data } = this.state;
    const { parties } = this.state;

    return (
      <div className="ksf-elections">
        <Parliament
          parties={parties}
        />
        <Table
          data={data}
        />
      </div>
    )
  }
}
