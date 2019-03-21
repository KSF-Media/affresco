import React from 'react';
import { render } from 'react-dom';
import ReactTable from 'react-table';
import 'react-table/react-table.css';

export default class Table extends React.Component{

  constructor(props) {
    super(props);
  }

  render() {
    const { data } = this.props;
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
