import React from 'react';
import Intro from './Intro.jsx';
import RaisedButton from 'material-ui/RaisedButton';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import {quizIntro} from './data/quizData.jsx';
import ReactGA from 'react-ga';

const styles = {
};

const btnstyles ={
  height: 60,
  marginBottom: 20,
};

ReactGA.initialize('UA-119802236-1');


export default class Start extends React.Component {

  constructor(props){
    super(props);
    this.state={
      quizData: [],
      weeklyQuiz: [],
      price: ''
    };
    this.handleClick = this.handleClick.bind(this);
  }


    async componentDidMount() {
       ReactGA.pageview(window.location.pathname + window.location.search);
     try {
       const res = await fetch('/duellen/api/');
       const quizData = await res.json();
       this.setState({
         quizData,
       });
       this.setState({
           weeklyQuiz: this.state.quizData[0]
       });
       } catch (e) {
        console.log(e);
       }
       if(this.state.weeklyQuiz.price === ''){
         this.setState({price: ''});
       }else{
         this.setState({price: 'Denna vecka lottar vi ut ' + this.state.weeklyQuiz.price});
       }
     }


  handleClick(e) {
    e.preventDefault();
    this.props.history.push('/Intro');
  }


  render() {

    return (

      <MuiThemeProvider>
        <div>
          <div>
            <h1>Duellen</h1>
            <p><b>{this.state.price}</b></p>
            <p className="header">Veckans Quiz</p>
            <a href={'/Intro/' + this.state.weeklyQuiz.id}><RaisedButton label={this.state.weeklyQuiz.title} fullWidth={true} primary={true} style={btnstyles} /></a>
              <p className="header">Tidigare Quiz</p>
            {this.state.quizData.map(item => (
              <div key={item.id} style={styles}>
                <a href={'/Intro/' + item.id}><RaisedButton label={item.title} fullWidth={true} primary={true} style={btnstyles} /></a>
              </div>
            ))}
          </div>
        </div>
      </MuiThemeProvider>

    );
  }
}
