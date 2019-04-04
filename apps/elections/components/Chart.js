import React from 'react';
import render from 'react-dom';

import { AreaResponse, Nominator, Status } from 'election';

export default class Chart extends React.Component{
  constructor(props){
    super(props);
  }

  render(){
    const { areaResponse } = this.props;
    const data = areaResponse === null ? null : areaResponseData(areaResponse);
    const getRows = (data) => {
      console.log("GetRows: ", data);
      return data.map(
        (obj, i) => {
          let votes = obj.votes ? obj.votes.totalPercent : 0;
          let name = obj.abbreviation.swedish || obj.abbreviation.finnish;
          if(votes){
            return(
              <div
                key={i}
                className={'background-'+name}
                style={{color: 'white', width: votes+'%'}}
              >
                <span className="name">{name}</span>
                <span className="votes">{votes.toFixed(1)}%</span>
              </div>
            )
          }
        }
      );
    }
    return(
      <div className="chart">
        {getRows(data || [])}
      </div>
    )
  }
}

function areaResponseData(areaResponse) {
  return areaResponse.nominators;
}
