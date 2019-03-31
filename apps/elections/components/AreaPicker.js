import React from 'react';
import Select from 'react-select';

import { getElectoralDistricts, getMunicipalities, getPollingDistricts } from './Backend.js'

"use strict";

const NOT_SELECTED = "NOT_SELECTED";
const LOADING = "LOADING";
const DISABLED = "DISABLED";
const SELECTED = "SELECTED";

function selector() {
  return {
    status: DISABLED,
    selection: null,
    options: []
  }
}

export default class AreaPicker extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      electoralDistrict: selector,
      municipality: selector,
      pollingDistrict: selector,
    }
  }
  componentDidMount() {
    getElectoralDistricts()
      .then(({ areas }) => {
        this.setState({
          electoralDistrict: {
            status: NOT_SELECTED,
            value: null,
            options: areas.map(areaOption)
          }
        })
      });
  }
  onSelection(selection) {
    if (this.props.onSelection) {
      this.props.onSelection(selection);
    }
  }
  onSelectedElectoralDistrict(electoralDistrict) {
    this.onSelection(electoralDistrict);
    this.setState({
      electoralDistrict: {
        status: SELECTED,
        selection: electoralDistrict,
        options: this.state.electoralDistrict.options
      },
      municipality: {
        selection: null,
        options: [],
        status: LOADING
      }
    });
    getMunicipalities(electoralDistrict.value).then(({ areas } ) => {
      this.setState({
        // reset the children selectors        
        municipality: {
          status: NOT_SELECTED,
          selection: null,
          options: areas.map(areaOption)
        },
        pollingDistrict: {
          status: DISABLED,
          selection: null,
          options: []
        }
      });
    });
  }
  onSelectedMunicipality(municipality) {
    this.setState({
      municipality: {
        ...this.state.municipality,
        selection: municipality
      }
    });
    this.onSelection(municipality);
    getPollingDistricts(municipality.value).then(({ areas } ) => {
      this.setState({
        pollingDistrict: {
          status: NOT_SELECTED,
          selection: null,
          options: areas.map(areaOption)
        }
      });
    });
  }
  onSelectedPollingDistrict(pollingDistrict) {
    this.setState( { pollingDistrict: {
      selection: pollingDistrict,
      status: SELECTED,
      options: this.state.pollingDistrict.options
    }});
    this.onSelection(pollingDistrict);
  }
  render() {
    return (
      <div>
        <Select
          value={ this.state.electoralDistrict.selection }
          options={ this.state.electoralDistrict.options }
          isDisabled={ this.state.electoralDistrict.status == "DISABLED" }
          isLoading={ this.state.electoralDistrict.status == "LOADING" }
          onChange={ (option) => this.onSelectedElectoralDistrict(option) }
        />
        <Select
          value={ this.state.municipality.selection }      
          isDisabled={ this.state.municipality.status == "DISABLED" }
          isLoading={ this.state.municipality.status == "LOADING" }
          options={ this.state.municipality.options }
          onChange={ (option) => this.onSelectedMunicipality(option) }
        />
        <Select
          value={ this.state.pollingDistrict.selection }
          isDisabled={ this.state.pollingDistrict.status == "DISABLED" }
          isLoading={ this.state.pollingDistrict.status == "LOADING" }
          options={ this.state.pollingDistrict.options || [] }
          onChange={ (option) => this.onSelectedPollingDistrict(option) }      
        />
      </div>
    )
  }
}

function areaOption(area) {
  return {
    value: area.info.identifier,
    label: area.info.name.swedish || area.info.name.finnish,
    info: area.info,
    parent: area.parent
  }
}
