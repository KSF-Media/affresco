import React from 'react';
import render from 'react-dom';

import { AreaResponse, Nominator, Status } from 'election';

export default class Chart extends React.Component{
  constructor(props){
    super(props);
  }

  render(){
    const relevantParties = ['CENT', 'SAML', 'BLÅ', 'SDP', 'SANNF', 'GRÖNA', 'VF', 'SFP'];

    const { areaResponse } = this.props;
    const data = areaResponse === null ? null : areaResponseData(areaResponse);

    const getParties = (data) => {
      const relevantData = data.filter( val => relevantParties.includes(val.abbreviation.swedish) );
      const relevantPartyVotes = relevantData.reduce((total, val) => {
        return parseFloat(total + val.votes.totalPercent)
      }, 0);
      const relevantAndOthers = relevantData.concat([
        {
          'abbreviation': {
            'swedish': 'Övriga'
          },
          'votes': {
            'totalPercent': 100 - relevantPartyVotes
          }
        }
      ])

      return relevantAndOthers.map(
        (obj, i) => {
          let countStarted = relevantPartyVotes > 0;
          let votes = obj.votes ? obj.votes.totalPercent : 0;
          let name = obj.abbreviation.swedish || obj.abbreviation.finnish;
          if(votes && countStarted){
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
        {getParties(data || [])}
      </div>
    )
  }
}

function areaResponseData(areaResponse) {
  return areaResponse.nominators;
}
