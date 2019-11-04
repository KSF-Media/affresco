import React from 'react';
import BackBtn from './BackBtn.jsx';
import Button from '@material-ui/core/Button';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import ReactGA from 'react-ga';
import {backendURL} from '../backend.js'

ReactGA.initialize('UA-119802236-1');
var moment = require('moment');

export default class Intro extends React.Component{

  constructor(props){
    super (props);
    this.state ={
      animate: false,
      player1_img: '',
      player2_img: '',
      quizData: [],
      sponsor: ''
    };
    this.handleClick = this.handleClick.bind(this);
  };

  async componentDidMount() {
    ReactGA.pageview(window.location.pathname + window.location.search);
    try {
      const res = await fetch(backendURL + 'get/all/quizzes/as/json/' + this.props.match.params.id);
      const quizData = await res.json();
      this.setState({
        quizData: quizData,
      });
    } catch (e) {
      console.log(e);
    }
    this.setState({
      player1_img: this.state.quizData.players.player1.img,
      player2_img: this.state.quizData.players.player2.img,
      player1_name : this.state.quizData.players.player1.name,
      player2_name : this.state.quizData.players.player2.name
    });
    if(this.state.quizData.sponsor === null){
      this.setState({sponsorText: '', sponsor: ''});
    }else{
      this.setState({sponsorText: 'Veckans pris är sponsrat av: ', sponsor: this.state.quizData.sponsor});
    }
    this.getweek()
  }

  handleClick(e){
    e.preventDefault();
    this.props.history.push('/Question/' + this.props.match.params.id);
    ReactGA.event({
      category: 'User',
      action: 'Started Quiz'
    });
  }
  getweek(){
    var day = moment(this.state.quizData.publication_date.slice(0,10)).add(-1, 'days');
      
    this.setState({
      week: day.week(),
    });
      
  }

  render(){
    return(
		<div>
			<div class="row">
				<div class="col">
					<BackBtn/>
				</div>
				<h3 class="textRight font-italic col">Vecka {this.state.week}</h3>
			</div>
			<div class="row">
				<h2 class="col text-center font-weight-bold">{this.state.quizData.title}</h2>
			</div>
			<div class="row mb-3">
				<div class="col-sm-6 text-center">
					<img src={this.state.player1_img} class="introImg"></img>
					<h3>{this.state.player1_name}</h3>
				</div>
				<div class="col-sm-6 text-center">
					<img src={this.state.player2_img} class="introImg"></img>
					<h3>{this.state.player2_name}</h3>
				</div>
			</div>
			<div class="row mt-4 mb-4">
				<div class="col">
					<h4>{ this.state.quizData.description }</h4>
				</div>
			</div>
			<div class="row mb-3">
				<div class="col">
					<p>{ this.state.sponsorText }<strong>{this.state.sponsor}</strong></p>
				</div>
			</div>
			<MuiThemeProvider>
				<Button variant="contained" onClick={this.handleClick} class="start introBtn" fullWidth={true} primary={true}> Börja </Button>
			</MuiThemeProvider>
		</div>
    );
  }
}
