import React from 'react';
import Button from '@material-ui/core/Button';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import ReactGA from 'react-ga';
import {backendURL} from '../backend.js'

const Logo = require('../../../../images/duellen-logo-vit.svg');

ReactGA.initialize('UA-119802236-1');

// to get the moment module to work
var moment = require('moment');

export default class Start extends React.Component {

  constructor(props){
    super(props);
    this.state={
      quizData: [],
      weeklyQuiz: {},
      price: '',
      showingAll: false,
      // The gap and the upperLim must be the same!!!
      // The upperLim must be BIGGER then the lowerLim
      gap: 20,
      upperLim: 20,

      lowerLim: 0,
    };
    this.handleClick = this.handleClick.bind(this);
  }


    async componentDidMount() {
       ReactGA.pageview(window.location.pathname + window.location.search);
     try {
       // The backendUrl needs to be the same as from where you want the quizzes to be fetched
       // 'get/all/quizzes/as/json' can't have a / in the end because the backend address dosen't have it
       const res = await fetch(backendURL + 'get/all/quizzes/as/json/?lowerlim=' + this.state.lowerLim.toString() + '&upperlim='+ this.state.gap.toString());
       const quizData = await res.json();
       console.log(quizData)
       this.setState({
        weeklyQuiz: quizData[0],
        quizData: quizData.slice(1, quizData.length + 1),
        lowerLim: this.state.lowerLim + this.state.gap,
        upperLim: this.state.upperLim + this.state.gap,
        showingAll: quizData.length < this.state.gap,
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

  // Loads more quizzes
  loadMore(e){
    e.preventDefault();
    try {
      // Gets the more quizzes from the backend        
      fetch(backendURL + 'get/all/quizzes/as/json/?lowerlim=' + this.state.lowerLim.toString() + '&upperlim='+ this.state.upperLim.toString()
          ).then(
            (res) => {return res.json()}
          ).then(
            (quizData) => {
              this.setState({
                quizData: [...this.state.quizData,  ...quizData],
                lowerLim: this.state.lowerLim + this.state.gap,
                upperLim: this.state.upperLim + this.state.gap,
                showingAll: quizData.length < this.state.gap,
              });
            }
          )
     } catch (e) {
       console.log(e);
     }

  }


  render() {
    try{
      return (
        <div className="duellen--button-container" style={{marginTop: '-50px'}}>
          <div className="mb-4 row justify-content-center">
            <div className='col-10 col-sm-10 col-md-7 col-lg-6'>
              <img src={Logo} alt="logo" style={{width: '100%', height:'100%'}}/>
            </div>
          </div>
          <MuiThemeProvider>
            <div>
              <div className="text-center m-auto col-12">
                <h3><b>{this.state.price}</b></h3>
                <h2 className="header">Veckans Quiz</h2>
                <a href={'/Intro/' + this.state.weeklyQuiz.id}>
                  <Button variant="contained" className="start" fullWidth={true} color="primary">Vecka {this.getweek(this.state.weeklyQuiz.publication_date)}</Button>
                </a>
                  <h2 className="slimh2">Tidigare Quiz</h2>
                {this.state.quizData.map(item => (
                  <div key={item.id} className="btnOrange">
                    <a href={'/Intro/' + item.id}>
                      <Button variant="contained" className="start" color="primary" fullWidth={true}>Vecka {this.getweek(item.publication_date)}</Button>
                    </a>
                  </div>
                ))}
                {this.state.showingAll == false &&
                  <div className="btnOrange">
                    <Button variant="contained" className="start" color="primary" fullWidth={true} onClick={this.loadMore.bind(this)}>Ladda mera</Button>
                  </div>
                }
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
        <div className="duellen--button-container" style={{marginTop: '-50px'}}>
          <div className="mb-4 row justify-content-center">
            <div className='col-10 col-sm-10 col-md-7 col-lg-6 text-center'>
              <img src={Logo} alt="logo" style={{width: '100%', height:'100%'}}/>
              <h2 className="header">Laddar</h2>
            </div>
          </div>
        </div>
      );  
    }
  }
}
