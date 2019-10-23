import React from 'react';
import Intro from './Intro.jsx';
import Button from '@material-ui/core/Button';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import {quizIntro} from './data/quizData.jsx';
import ReactGA from 'react-ga';
import {backendURL} from '../backend.js'
import { Script } from 'vm';

const styles = {
};

const btnstyles ={
  height: 80,
  marginBottom: 20,
};

ReactGA.initialize('UA-119802236-1');


export default class Start extends React.Component {

  constructor(props){
    super(props);
    this.state={
      quizData: [],
      weeklyQuiz: {},
      price: ''
    };
    this.handleClick = this.handleClick.bind(this);
  }


    async componentDidMount() {
       ReactGA.pageview(window.location.pathname + window.location.search);
     try {
       const res = await fetch(backendURL + 'get/all/quizzes/as/json');
       const quizData = await res.json();
       this.setState({
        weeklyQuiz: quizData[0],
        quizData: quizData.slice(1, quizData.length + 1)
      }); 
      } catch (e) {
        console.log(e);
       }
       if(this.state.weeklyQuiz.price === null){
         this.setState({price: ''});
       }else{
        this.setState({price: 'Denna vecka lottar vi ut ' + this.state.weeklyQuiz.price});
       }
       this.setState({loaded: true})
      }

  getweek(date){
    Date.prototype.getWeek = function (dowOffset) {      
          dowOffset = typeof(dowOffset) == 'int' ? dowOffset : 0; //default dowOffset to zero
          var newYear = new Date(this.getFullYear(),0,1);
          var day = newYear.getDay() - dowOffset; //the day of week the year begins on
          day = (day >= 0 ? day : day + 7);
          var daynum = Math.floor((this.getTime() - newYear.getTime() - 
          (this.getTimezoneOffset()-newYear.getTimezoneOffset())*60000)/86400000) + 1;
          var weeknum;
          //if the year starts before the middle of a week
          if(day < 4) {
              weeknum = Math.floor((daynum+day-1)/7) + 1;
              if(weeknum > 52) {
                  nYear = new Date(this.getFullYear() + 1,0,1);
                  nday = nYear.getDay() - dowOffset;
                  nday = nday >= 0 ? nday : nday + 7;
                  /*if the next year starts before the middle of
                    the week, it is week #1 of that year*/
                  weeknum = nday < 4 ? 1 : 53;
              }
          }
          else {
              weeknum = Math.floor((daynum+day-1)/7);
          }
          return weeknum;
      };
      var mydate = new Date(Number(date.slice(0,4)), Number(date.slice(5,7))-1,Number(date.slice(8,10)));
    return mydate.getWeek()
  }

  handleClick(e) {
    e.preventDefault();
    this.props.history.push('/Intro');
  }


  render() {
    try{
      return (
        <div class="duellen--button-container">
          <MuiThemeProvider>
            <div>
              <div>
                <h1>Duellen</h1>
                <p><b>{this.state.price}</b></p>
                <p className="header">Veckans Quiz</p>
                <a href={'/Intro/' + this.state.weeklyQuiz.id}>
                  <Button variant="contained" fullWidth={true} color="primary" style={btnstyles}>Vecka {this.getweek(this.state.weeklyQuiz.publication_date)}</Button>
                </a>
                  <p className="header">Tidigare Quiz</p>
                {this.state.quizData.map(item => (
                  <div key={item.id} style={styles}>
                    <a href={'/Intro/' + item.id}>
                      <Button variant="contained" color="primary" fullWidth={true} style={btnstyles}>Vecka {this.getweek(item.publication_date)}</Button>
                    </a>
                  </div>
                ))}
              </div>
            </div>
          </MuiThemeProvider>
        </div>
      );
    } catch (e){
      console.log('has not loaded yet')
      return (
        <div class="duellen--button-container">
          <MuiThemeProvider>
            <div>
              <div>
                <h1>Duellen</h1>
                <p><b>{this.state.price}</b></p>
                <p className="header">Veckans Quiz</p>
                <a href={'/Intro/' + this.state.weeklyQuiz.id}>
                  <Button variant="contained" fullWidth={true} color="primary" style={btnstyles}>{this.state.weeklyQuiz.title}</Button>
                </a>
                  <p className="header">Tidigare Quiz</p>
                {this.state.quizData.map(item => (
                  <div key={item.id} style={styles}>
                    <a href={'/Intro/' + item.id}>
                      <Button variant="contained" color="primary" fullWidth={true} style={btnstyles}>{item.title}</Button>
                    </a>
                  </div>
                ))}
              </div>
            </div>
          </MuiThemeProvider>
        </div>
      );  
    }
  }
}
