import React from 'react';
import ExitDialog from './ExitDialog.jsx';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import { ContentBackspace } from 'material-ui/svg-icons';



export default class PastHints extends React.Component{
    constructor(props){
        super(props);
        this.state = {
            hints: [{
                name: "",
                hint: "",
            }],
            hasMounted: false,
            show: false,
            lastHintPoint: 0
        }
      };
    // After each question this screen  is loaded to show the player how they did
    componentDidMount(){
        if (this.props.hints.length > 0){
            this.setState({
                hints: this.props.hints.map(function(key){
                    return {name: this.props.hints[key][0], hint: this.props.hints[key][1]}
                }),
                show: this.props.show,
                hasMounted: true,
                lastHintPoint: this.props.lasthint,
            })
        }
    }

    componentDidUpdate(){
        var hintsArray = this.props.hints
        if (this.props.show != this.state.show || (this.state.lastHintPoint != this.props.lasthint && hintsArray.length > 0)){
            this.setState({
                hints: hintsArray.map(function(key){
                    return {name: key[0], hint: key[1]}
                }),
                show: this.props.show,
                hasMounted: true,
                lastHintPoint: this.props.lasthint,
            })
        }
    }
  
  render(){
    // If there is no check if it has mounted it will just render a empty screeen
    if (this.state.hasMounted == true && this.state.show == true){  
      return(
        <div>
           {this.state.hints.map(({name, hint}) => (
              <div className="w-100 mb-2">
                  <p>
                    <b>{name}</b>
                    <br/>
                    {hint}
                  </p>
              </div>
            ))}
        </div>
      );
    }else{
      return(
        <div></div>
      );
    }
  };
}
