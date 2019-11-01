import React from 'react';
import Intro from './Intro.jsx';
import Button from '@material-ui/core/Button';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import ReactGA from 'react-ga';
import {backendURL} from '../backend.js'
import { Script } from 'vm';

const Logo = require('../../../../images/duellen-logo-vit.svg');

ReactGA.initialize('UA-119802236-1');
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
       this.setState({loaded: true})
      }

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
          <div className="mb-4 row justify-content-center">
            <div className='col-4'>
              <img src={Logo} alt="logo" style={{width: '100%', height:'100%'}}/>
            </div>
          </div>
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
      return (
        <div class="duellen--button-container">
          <div className="mb-4 row justify-content-center">
            <div className='col-4'>
              <img src={Logo} alt="logo" style={{width: '100%', height:'100%'}}/>
            </div>
          </div>
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
