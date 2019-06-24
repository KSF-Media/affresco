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
      countryResponse: null,
      fuse: null
    };
  }
  componentDidMount(){
    this.onAreaSelection(this.props.match.params.areaId);
    this.getCountryData();
  }
  componentDidUpdate(prevProps) {
    // console.log("componentDidUpdate");
    if (prevProps.match.params.areaId !== this.props.match.params.areaId){
      this.onAreaSelection(this.props.match.params.areaId);
    }
  }
  componentWillUnmount() {
    this.stopFuse();
  }
  onAreaSelection(areaId) {
    this.stopFuse();
    if (!areaId) { // if no area was provided (e.g. on start or reset)
      return this.onAreaSelection("MAA")
    }
    return getArea(areaId)
      .then(areaResponse => {
        // Get data for selected area
        this.setState({selectedAreaResponse: areaResponse});
        this.setState({votesInArea: areaResponse.nominators});
        this.startFuse();
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
  startFuse() {
    this.stopFuse();
    var timeOut = setTimeout(() => {
      this.onAreaSelection(this.props.match.params.areaId);
      this.getCountryData();
    }, 2*60*1000);
    this.setState({fuse: timeOut});
    // console.log('Timeout: ' + timeOut + ' is set');
  }
  stopFuse() {
    clearTimeout(this.state.fuse);
    // console.log('Timeout: ' + this.state.fuse + ' cleared');
  }
  render() {
    switch(this.props.match.path) {
      case "/compact":
        return (
          <div className={isMobile ? 'mobile ksf-elections compact' : 'ksf-elections compact'}>
            <AreaInfo
            areaResponse={this.state.selectedAreaResponse}
            />
            <Chart
            areaResponse={this.state.selectedAreaResponse}
            />
          </div>
        )
      default:
        switch(process.env.ELECTION_TYPE){
          case 'PARLIAMENT':
            return (
              <div className={isMobile ? 'mobile ksf-elections' : 'ksf-elections'}>
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
          case 'EU':
            return (
              <div className={isMobile ? 'mobile ksf-elections eu' : 'ksf-elections eu'}>
              <div className="status">
                {this.state.countryResponse ? this.state.countryResponse.area.info.calculationStatus : null}% Räknat
              </div>
              <Timestamp timestamp={
                this.state.countryResponse
                ? this.state.countryResponse.timestamp
                : null
              }/>
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
}
