import React from "react";
import Dialog from "material-ui/Dialog";
import Button from "@material-ui/core/Button";
import TextField from "material-ui/TextField";
import MuiThemeProvider from "material-ui/styles/MuiThemeProvider";

/**
 * A modal dialog can only be closed by selecting one of the actions.
 */
export default class EmailDialog extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      open: false,
      disabled: true,
      newValue: "",
      content: "Skriv ner din epost för att delta i utlottningen",
    };
    this.handleOpen = this.handleOpen.bind(this);
    this.handleClose = this.handleClose.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
    this.onChange = this.onChange.bind(this);
  }

  onChange(e) {
    let re =
      /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;

    if (re.test(e.target.value)) {
      this.setState({ disabled: false });
    } else {
      this.setState({ disabled: true });
    }
  }

  handleOpen() {
    this.setState({ open: true, disabled: true });
  }

  handleClose() {
    this.setState({ open: false });
  }

  handleSubmit() {
    if (this.state.disabled == false) {
      this.setState({ open: true, content: "Tack för ditt deltagande!" }, () =>
        setTimeout(() => this.setState({ open: false }), 850)
      );
    } else {
      this.setState({ open: true });
    }
  }

  render() {
    const actions = [
      <Button primary={true} onClick={this.handleClose}>
        Ångra
      </Button>,
      <Button primary={true} disabled={this.state.disabled} onClick={this.handleSubmit}>
        Skicka
      </Button>,
    ];

    return (
      <div>
        <MuiThemeProvider>
          <div>
            <Button variant="contained" fullWidth={true} primary={true} onClick={this.handleOpen}>
              Delta i veckans utlottning här!
            </Button>
            <Dialog
              style={{ textAlign: "center", margin: "auto" }}
              actions={actions}
              modal={true}
              open={this.state.open}
            >
              <div style={{ height: 80, textAlign: "center", paddingTop: 15 }}>
                <h3>{this.state.content}</h3>
              </div>
              <TextField id="email" name="email" floatingLabelText="Din epost här" onChange={this.onChange} />
            </Dialog>
          </div>
        </MuiThemeProvider>
      </div>
    );
  }
}
