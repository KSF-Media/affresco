import React from 'react';
import render from 'react-dom';

import Table from './Table.js';

import '../less/ksf-colors.less';
import '../less/ksf-fonts.less';
import '../assets/less/table.less';

import testData from './TestData';

export default class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      data: testData
    }
  }
  render(){
    const { data } = this.state;
    return (
      <div className="ksf-elections">
        <Table
          data={data}
        />
      </div>
    )
  }
}
