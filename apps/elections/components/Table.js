import React from 'react';
import { render } from 'react-dom';
import ReactTable from 'react-table';
import 'react-table/react-table.css';

import testData from './TestData.js'

export default class Table extends React.Component{

  constructor(props) {
    super(props);
    this.state = {
      data: testData
    }
    console.log(this.state);
  }

  render() {
    const { data } = this.state;
    return (
      <div>
        <ReactTable
          data={data}
          columns={[
              {
                Header: '#',
                accessor: 'id'
              },
              {
                Header: 'Parti',
                accessor: 'party'
              },
              {
                Header: 'Förnamn',
                accessor: 'firstName'
              },
              {
                Header: 'Efternamn',
                accessor: 'lastName'
              },
              {
                Header: 'Röster Totalt',
                accessor: 'votesTotal'
              },
              {
                Header: 'Förhandsröster',
                accessor: 'votesAdvance'
              },
              {
                Header: 'Valdagsröster',
                accessor: 'votesElectionDay'
              },
              {
                Header: 'Jämförelsetal',
                accessor: 'comparativeIndex'
              },
              {
                Header: 'Status',
                accessor: 'status'
              },
          ]}
          className="-striped -highlight"
        />
      </div>
    )
  }

}
