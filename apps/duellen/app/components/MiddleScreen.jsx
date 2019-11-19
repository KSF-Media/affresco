import React from 'react';
import ExitDialog from './ExitDialog.jsx';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';



export default class MiddleScreen extends React.Component{
    constructor(props){
        super(props);
        this.state = {
          players: [],
          hasMounted: false,
          progress: 0,
          question: "",
          category: "",
          answer: '',
          nextButtonText: 'Nästa fråga',
          player1CurrentPoints: 0,
          player2CurrentPoints: 0,
        }
      };
    // every time something new is typed in to the input field this will search for it in wikipedia
    componentDidMount(){
      var nextQuestionText = 'Nästa fråga'
      console.log(this.props.progress)
      if (this.props.progress == 4){
        nextQuestionText = "Avsluta och se dina resultat"
      }
      var p1Score = this.props.players.player1.allScores[0]
      var p2Score = this.props.players.player2.allScores[0]
      this.setState({
        players: this.props.players,
        question: this.props.question,
        category: this.props.category,
        progress: this.props.progress,
        answer: this.props.answer,
        nextButtonText: nextQuestionText,
        player1CurrentPoints: p1Score,
        player2CurrentPoints: p2Score,
        hasMounted: true,
      })
    }
  
  render(){
    // The map funktion returns an error if ther is an empty array 
    // And we don't want anything to show it there hasn't been a search for anything
    if (this.state.hasMounted == true){  
      return(
        <div>
          <div className="row">
            <div className="col">
              <MuiThemeProvider>
                <ExitDialog />
              </MuiThemeProvider>
            </div>
          </div>
          <div className="row">
            <div className="col text-center">
              <h2>{this.state.category} : {this.state.question}</h2>
            </div>
          </div>
          <div className="row">
            <div className="col text-center">
              <h4>Rätta svaret för frågan var: {this.state.answer}</h4>
            </div>
          </div>
          <div className="row mb-3">
            <div className="col-sm-6 text-center">
              <img src={this.state.players.player1.img} className="introImg"></img>
              <h4>{this.state.players.player1.name}<br/> fick {this.state.player1CurrentPoints} poäng på den här frågan</h4>
            </div>
            <div className="col-sm-6 text-center">
              <img src={this.state.players.player2.img} className="introImg"></img>
              <h4>{this.state.players.player2.name}<br/> fick {this.state.player2CurrentPoints} poäng på den här frågan</h4>
            </div>
          </div>
          <div className="row">
            <div className="col mt-3 text-center">
              <button onClick={() => this.props.onClick()} className='start introBtn'>{this.state.nextButtonText}</button>
            </div>
          </div>
        </div>
      );
    }else{
      return(
        <div></div>
      );
    }
  };
}

