import React from 'react';
import render from 'react-dom';
import { isMobile } from 'react-device-detect';

import Table from './Table.js';
import Parliament from './Parliament.js';
import AreaPicker from './AreaPicker.js'
import Status from './Status.js';
import Chart from './Chart.js';
import AreaInfo from './AreaInfo.js';

import '../assets/less/app.less';

import { getArea, getCountry } from './Backend';

export default class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      // selectedArea: null,
      selectedAreaResponse: null,
      seats: {},
      votesInArea: [],
      selectedAreaId: null
    };
  }
  componentDidMount(){
    this.onAreaSelection(this.props.match.params.areaId);
  }
  componentDidUpdate(prevProps) {
    if (prevProps.match.params.areaId !== this.props.match.params.areaId){
      this.onAreaSelection(this.props.match.params.areaId);
    }
  }
  onAreaSelection(areaId) {
    console.log("state: ", this.state);
    console.info("onAreaSelection", areaId);

    if (!areaId) { // if no area was provided (e.g. on start or reset)
      return getCountry().then(area => { // get the whole country
        if (area) { return this.onAreaSelection(area.info.identifier) } // and roll with that instead
      })
    }
    return getArea(areaId)
      .then(areaResponse => {
        // Get data for selected area
        this.setState({selectedAreaResponse: areaResponse});
        this.setState({votesInArea: areaResponse.nominators});

        // Make object for the parliamentSVG
        let s = { seats: areaResponse.nominators.map(
          nominator => {
            return {[nominator.abbreviation.finnish]: {
              'seats': nominator.seats
            }}
          }
        )};
        this.setState({ seats: Object.assign(...s.seats) });
      });
  }
  render(){
    return (
      <div className={isMobile ? 'mobile ksf-elections' : 'ksf-elections'}>
        <Status
          percentage={
            this.state.selectedAreaResponse
              ? this.state.selectedAreaResponse.area.info.calculationStatus
              : null
          }
        />
        <Parliament
          seats={this.state.seats}
        />
        <AreaPicker
          onSelection={ (area) => {
            if (area === null) {
              this.props.history.push('/')
            } else {
              this.props.history.push('/area/' + area.info.identifier)  
            }
          }}
          selection={this.props.match.params.areaId}
        />
        <AreaInfo
          areaResponse={this.state.selectedAreaResponse}
        />
        <Chart
          areaResponse={this.state.selectedAreaResponse}
        />
        <Table
          areaResponse={this.state.selectedAreaResponse}
        />
      </div>
    )
  }
}
