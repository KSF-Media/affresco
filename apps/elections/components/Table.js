import React from "react";
import { render } from "react-dom";
import ReactTable from "react-table";
import "react-table/react-table.css";
import { isMobile } from "react-device-detect";

import { AreaResponse, Nominator, Status } from "election";

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
          showPagination={true}
          pageSizeOptions={[20, 50, 100]}
          showPageJump={false}
          loading={areaResponse === null}
          data={data || []}
          columns={[
            {
              Header: "#",
              accessor: "candidateNumber",
              width: 50,
            },
            {
              Header: "Parti",
              // TODO: when hovering over that cell, we could show a box with extended party info
              id: "nominator.abbreviation",
              accessor: ({ nominator }) =>
                // ???: Self-nominated candidates have 'E<candidateNumber>' abbreviation
                //      We could render some text instead of that.
                nominator.abbreviation.swedish || nominator.abbreviation.finnish,
              Cell: (row) => {
                const className = row.value ? "fill-" + row.value : "";
                return (
                  <span>
                    <svg>
                      <rect className={className} width="11" height="12" />
                    </svg>
                    {row.value}
                  </span>
                );
              },
              width: 80,
            },
            {
              Header: "Namn",
              id: "name",
              accessor: (row) => row.lastName + ", " + row.firstName,
            },
            {
              Header: "Röster",
              accessor: "votes.totalVotes",
              maxWidth: 100,
            },
            {
              Header: "Förhand",
              accessor: "votes.advanceVotes",
              show: !isMobile,
              maxWidth: 100,
            },
            {
              Header: "Valdag",
              accessor: "votes.electionDayVotes",
              show: !isMobile,
              maxWidth: 100,
            },
            {
              Header: "Jämförelsetal",
              accessor: "comparativeIndex",
              show: !isMobile,
              Cell: (row) => {
                const val = row.value ? row.value.toFixed(2) : null;
                return <span>{val}</span>;
              },
            },
            {
              Header: "Status",
              id: "status",
              accessor: ({ status }) =>
                status === Status.ELECTED
                  ? "Invald"
                  : status === Status.SUBSTITUTE
                  ? "Suppleant"
                  : status === Status.NOT_ELECTED
                  ? ""
                  : status === Status.INELIGIBLE
                  ? "Ej valbar"
                  : status,
              Cell: (row) => {
                const className = row.value ? row.value.toLowerCase().replace(/\s+/g, "") : "";
                return <div className={className + " status-cell"}>{row.value}</div>;
              },
            },
          ]}
          resizable={false}
          defaultSorted={[
            {
              id: "votes.totalVotes",
              desc: true,
            },
          ]}
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
    );
  }
}

// Converts 'AreaResponse' into table data format
function areaResponseData(areaResponse) {
  return areaResponse.nominators.reduce((rows, nominator) => rows.concat(nominatorRows(nominator)), []);
}

// Turns a 'Nominator' into an array of candidate rows.
function nominatorRows({ candidates, ...nominator }) {
  return (candidates || []).map((candidate) => {
    candidate.nominator = nominator;
    return candidate;
  });
}
