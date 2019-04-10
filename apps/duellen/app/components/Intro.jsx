import React from 'react';
import BackBtn from './BackBtn.jsx';
import RaisedButton from 'material-ui/RaisedButton';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import {quizIntro} from './data/quizData.jsx';
import EmailDialog from './EmailDialog.jsx';
import ReactGA from 'react-ga';

const styles = {
listStyle: 'none',
};

const btnstyles ={
  height: 60,
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
     const res = await fetch('/duellen/api/' + this.props.match.params.id + '/');
     const quizData = await res.json();
     this.setState({
       quizData,
     });
     this.setState({
       player1_img: this.state.quizData.player1_img,
       player2_img: this.state.quizData.player2_img
     });
     }catch (e) {
      console.log(e);
     }
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
              <img src={this.state.player1_img} style={{objectFit: 'cover', width:160, height:160, borderRadius: '50%'}}></img>
              <p>{this.state.quizData.player1}</p>
            </div>
            <div style={{float: 'left', width: '50%', textAlign: 'center'}}>
              <img src={this.state.player2_img} style={{objectFit: 'cover', width:160, height:160, borderRadius: '50%'}}></img>
              <p>{this.state.quizData.player2}</p>
            </div>
          </div>
        <p>{this.state.quizData.description}</p>
        <p><b>{this.state.sponsor}</b></p>
        <MuiThemeProvider>
            <RaisedButton onClick={this.handleClick} label="Börja" fullWidth={true} primary={true} style={btnstyles} />
        </MuiThemeProvider>
      </div>
    );
  }
}
