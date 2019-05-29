import React from 'react';
import Dialog from 'material-ui/Dialog';
import Button from '@material-ui/core/Button';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import {withRouter} from 'react-router';


class ExitDialog extends React.Component {
  constructor(props){
    super(props);
    this.state = {open: false};
    this.handleOpen = this.handleOpen.bind(this);
    this.handleClose = this.handleClose.bind(this);
    this.handleBack = this.handleBack.bind(this);
  };

  handleOpen(){
    this.setState({open: true});
  };

  handleClose(){
    this.setState({open: false});
  };

  handleBack(e){
    e.preventDefault();
    this.props.history.push('/');
  };

  render() {
    const actions = [
      <Button primary={true} onClick={this.handleBack}>Lämna spelet</Button>,
      <Button primary={true} onClick={this.handleClose}>Fortsätt spela</Button>,
    ];

    return (
      <div>
        <MuiThemeProvider>
        <i className="material-icons" onClick={this.handleOpen} style={{cursor: 'pointer'}}>arrow_back</i>
        </MuiThemeProvider>
        <MuiThemeProvider>
        <Dialog
          actions={actions}
          modal={false}
          open={this.state.open}
          onRequestClose={this.handleClose}
        >

          Är du säker på att du vill avsluta duellen? Alla dina poäng kommer förloras.

        </Dialog>
        </MuiThemeProvider>
      </div>
    );
  }
}

export default withRouter(ExitDialog);
