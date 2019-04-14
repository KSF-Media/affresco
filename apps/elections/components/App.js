import React from 'react';
import render from 'react-dom';
import { isMobile } from 'react-device-detect';

import Table from './Table.js';
import Parliament from './Parliament.js';
import AreaPicker from './AreaPicker.js'
import Status from './Status.js';
import Chart from './Chart.js';
import Timestamp from './Timestamp.js';
import AreaInfo from './AreaInfo.js';

import '../assets/less/app.less';

import { getArea, getCountry } from './Backend';

export default class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      selectedAreaResponse: null,
      seats: {},
      votesInArea: [],
      selectedAreaId: null,
      countryResponse: null
    };
  }
  componentDidMount(){
    this.onAreaSelection(this.props.match.params.areaId);
    this.getCountryData();
  }
  componentDidUpdate(prevProps) {
    if (prevProps.match.params.areaId !== this.props.match.params.areaId){
      this.onAreaSelection(this.props.match.params.areaId);
    }
  }
  onAreaSelection(areaId) {
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
      });
  }
  getCountryData() {
    return getArea("MAA").then(countryResponse => {
      this.setState({countryResponse: countryResponse});
      // Make object for the parliamentSVG
      console.log("countryResponse: ", countryResponse);
      let s = { seats: countryResponse.nominators.map(
        nominator => {
          return {[nominator.abbreviation.swedish]: {
            'seats': nominator.seats
          }}
        }
      )};
      this.setState({ seats: Object.assign(...s.seats) });
    })
  }
  render() {
    if(this.props.match.path === "/compact") {
      return (
        <div className={isMobile ? 'mobile ' : '' + 'ksf-elections compact'}>
          <AreaInfo
            areaResponse={this.state.selectedAreaResponse}
          />
          <Chart
            areaResponse={this.state.selectedAreaResponse}
          />
        </div>
      )
    } else {
      return (
        <div className={isMobile ? 'mobile ' : '' + 'ksf-elections'}>
        <Status
        percentage={
          this.state.countryResponse
          ? this.state.countryResponse.area.info.calculationStatus
          : null
        }
        />
        <Timestamp timestamp={
          this.state.countryResponse
          ? this.state.countryResponse.timestamp
          : null
        }/>
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
}
