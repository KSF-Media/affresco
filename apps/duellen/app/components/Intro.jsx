import React from 'react';
import BackBtn from './BackBtn.jsx';
import Button from '@material-ui/core/Button';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import {quizIntro} from './data/quizData.jsx';
import EmailDialog from './EmailDialog.jsx';
import ReactGA from 'react-ga';
import {backendURL} from '../backend.js'

const styles = {
listStyle: 'none',
};

const btnstyles ={
  height: 80,
  marginBottom: 20,
  marginTop: '3em',
};

ReactGA.initialize('UA-119802236-1');

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
        quizData,
      });
      console.log(this.state.quizData)
    } catch (e) {
      console.log(e);
    }
    this.setState({
      player1_img: this.state.quizData.players.player1.img,
      player2_img: this.state.quizData.players.player2.img,
      player1_name : this.state.quizData.players.player1.name,
      player2_name : this.state.quizData.players.player2.name
    });
    if(this.state.quizData.sponsor === ''){
      this.setState({sponsor: ''});
    }else{
      this.setState({sponsor: 'Veckans pris är sponsrat av ' + this.state.quizData.sponsor});
    }
  }

  handleClick(e){
    e.preventDefault();
    this.props.history.push('/Question/' + this.props.match.params.id);
    ReactGA.event({
      category: 'User',
      action: 'Started Quiz'
    });
  }

  render(){
    return(

      <div style={styles}>

        <BackBtn />
        <p className="header">{quizIntro.week}</p>
        <h2>{this.state.quizData.title}</h2>
          <div className="players">
            <div style={{float: 'left', width: '50%',textAlign: 'center'}}>
              <img src={this.state.player1_img} style={{objectFit: 'cover', width:200, height:200, borderRadius: '50%'}}></img>
              <p>{this.state.player1_name}</p>
            </div>
            <div style={{float: 'left', width: '50%', textAlign: 'center'}}>
              <img src={this.state.player2_img} style={{objectFit: 'cover', width:200, height:200, borderRadius: '50%'}}></img>
              <p>{this.state.player2_name}</p>
            </div>
          </div>
        <p>{this.state.quizData.description}</p>
        <p><b>{this.state.sponsor}</b></p>
        <MuiThemeProvider>
            <Button variant="contained" onClick={this.handleClick} fullWidth={true} primary={true} style={btnstyles}> Börja </Button>
        </MuiThemeProvider>
      </div>
    );
  }
}
