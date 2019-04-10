import React from 'react';
import {withRouter} from 'react-router';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';

class BackBtn extends React.Component{
  constructor(props){
    super(props);
    this.handleClick = this.handleClick.bind(this);
  };

  handleClick(e){
    e.preventDefault(e);
    this.props.history.push('/');
    };

  render(){
    return(
    <MuiThemeProvider>
      <i className="material-icons" onClick={this.handleClick} style={{cursor: 'pointer', textAlign: 'left',}}>arrow_back</i>
    </MuiThemeProvider>

    );
  }
};

export default withRouter(BackBtn);
