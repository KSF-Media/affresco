import React from 'react';
import Button from '@material-ui/core/Button';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import ReactGA from 'react-ga';
import {backendURL} from '../backend.js'

ReactGA.initialize('UA-119802236-1');

// to get the moment module to work
var moment = require('moment');

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
       // The backendUrl needs to be the same as from where you want the quizzes to be fetched
       // 'get/all/quizzes/as/json' can't have a / in the end because the backend address dosen't have it
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
        this.setState({price: 'Denna vecka lottar vi ut: ' + this.state.weeklyQuiz.price});
       }
       // This will make the quizzes show need tho be here so the map funktion can be activated otherwise the program will crash
       this.setState({loaded: true})
      }

  // gets the week of eatch quiz and displays the week
  getweek(date){
    var dateObject = moment(date.slice(0,10)).add(-1, 'days')
    return moment(dateObject).week()
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
              <div class="text-center m-auto col-12">
                <h1>Duellen</h1>
                <h3><b>{this.state.price}</b></h3>
                <h2 className="header">Veckans Quiz</h2>
                <a href={'/Intro/' + this.state.weeklyQuiz.id}>
                  <Button variant="contained" class="start" fullWidth={true} color="primary">Vecka {this.getweek(this.state.weeklyQuiz.publication_date)}</Button>
                </a>
                  <h2 className="slimh2">Tidigare Quiz</h2>
                {this.state.quizData.map(item => (
                  <div key={item.id} class="btnOrange">
                    <a href={'/Intro/' + item.id}>
                      <Button variant="contained" class="start" color="primary" fullWidth={true}>Vecka {this.getweek(item.publication_date)}</Button>
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
      // this will render befor the quiz have loaded in
      // this is due to that the map needs to have an populated array to be abel to render otherwise the render function gets stuck
      // if it gets stuck it will never render porperli
      return (
        <div class="duellen--button-container">
          <MuiThemeProvider>
            <div>
              <div>
                <h1>Duellen</h1>
                <p><b>{this.state.price}</b></p>
                <h2 className="header">Veckans Quiz</h2>
                <a href={'/Intro/' + this.state.weeklyQuiz.id}>
                  <Button variant="contained" class="start" fullWidth={true} color="primary">{this.state.weeklyQuiz.title}</Button>
                </a>
                  <p className="header">Tidigare Quiz</p>
                {this.state.quizData.map(item => (
                  <div key={item.id} style={styles}>
                    <a href={'/Intro/' + item.id}>
                      <Button variant="contained" class="start" color="primary" fullWidth={true}>{item.title}</Button>
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
