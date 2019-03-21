import React from 'react';
import render from 'react-dom';

import Table from './Table.js';
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
      <Table 
        data={data}
      />
    )
  }
}
