import React from 'react';
import { render } from 'react-dom';
import ReactTable from 'react-table';
import 'react-table/react-table.css';

import { AreaResponse, Nominator, Status } from 'election';

export default class Table extends React.Component {
  constructor(props) {
    super(props);
  }
  render() {
    const { areaResponse } = this.props;
    const data = areaResponse === null ? null : areaResponseData(areaResponse);
    return (
      <div>
        <ReactTable
          loading={ areaResponse === null }
          data={data || []}
          columns={[
              {
                Header: '#',
                accessor: 'candidateNumber'
              },
              {
                Header: 'Parti',
                // TODO: when hovering over that cell, we could show a box with extended party info
                id: 'nominator.abbreviation',
                accessor: ({ nominator }) =>
                  // ???: Self-nominated candidates have 'E<candidateNumber>' abbreviation
                  //      We could render some text instead of that.
                  nominator.abbreviation.swedish
                    || nominator.abbreviation.finnish
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
                accessor: 'votes.totalVotes'
              },
              {
                Header: 'Förhandsröster',
                accessor: 'votes.advanceVotes'
              },
              {
                Header: 'Valdagsröster',
                accessor: 'votes.electionDayVotes'
              },
              {
                Header: 'Jämförelsetal',
                accessor: 'comparativeIndex'
              },
              {
                Header: 'Status',
                id: 'status',
                accessor: ({status}) =>
                    status === Status.ELECTED     ? "Invald"
                  : status === Status.SUBSTITUTE  ? "Suppleant"
                  : status === Status.NOT_ELECTED ? "Ej invald"
                  : status === Status.INELIGIBLE  ? "Ej valbar"
                  : status
              },
          ]}
          className="-striped -highlight"
          previousText="Föregående"
          nextText="Nästa"
          loadingText="Laddar..."
          noDataText="Inga rader hittades"
          pageText="Sida"
          ofText="av"
          rowsText="rader"
          pageJumpText="Gå till sida"
          rowsSelectorText="rader per sida"
        />
      </div>
    )
  }
}

// Converts 'AreaResponse' into table data format
function areaResponseData(areaResponse) {
  return areaResponse.nominators.reduce(
    (rows, nominator) => rows.concat(nominatorRows(nominator)),
    []
  );
}

// Turns a 'Nominator' into an array of candidate rows.
function nominatorRows({candidates, ...nominator}) {
  return (candidates || []).map(candidate => {
    candidate.nominator = nominator;
    return candidate;
  });
}
