import React from 'react';
import render from 'react-dom';
import { sortBy } from 'lodash'

import { AreaResponse, Nominator, Status } from 'election';

export default class Chart extends React.Component{
  constructor(props){
    super(props);
  }

  render(){
    const relevantParties = ['CENT', 'SAML', 'BLÅ', 'SDP', 'SAF', 'GRÖNA', 'VÄNST', 'SFP', 'KD'];

    const { areaResponse } = this.props;
    const data = areaResponse === null ? null : areaResponseData(areaResponse);


    const getParties = (data) => {

      // Include data only for parties defined in relevatParties
      const relevantData = data.filter( val => relevantParties.includes(val.abbreviation.swedish) );

      // Sort the data by totalPercent
      let relevantDataSorted = []
      if(relevantData.length !== 0) {
        relevantDataSorted = sortBy( relevantData, obj => obj.votes.totalPercent ).reverse();
      }

      // Calculate sum of all vote percentage of the relevant parties
      const relevantPartyVotes = relevantData.reduce((total, val) => {
        return parseFloat(total + val.votes.totalPercent)
      }, 0);

      // Append others
      const relevantAndOthers = relevantDataSorted.concat([
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
              <div className="party"
                key={i}
              >
                <div className="info">
                  <div>
                    <span className="name">{name}</span>
                    <span className="votes">{votes.toFixed(1)}%</span>
                  </div>
                </div>
                <div
                  className={'bar background-'+name}
                  style={{height: votes * 5 +'px'}}
                >
                </div>
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
