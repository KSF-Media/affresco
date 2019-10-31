import React from 'react';
import AutoComplete from 'material-ui/AutoComplete';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import WikiLink from './WikiLink.jsx';
import getMuiTheme from 'material-ui/styles/getMuiTheme';
import MenuItem from 'material-ui/MenuItem';
import {indigo500, indigo700, redA200} from 'material-ui/styles/colors';
import Button from '@material-ui/core/Button';
import {withRouter} from 'react-router';
import ExitDialog from './ExitDialog.jsx';
import LinearProgress from 'material-ui/LinearProgress';
import Resultat from './Resultat.jsx';
import $ from 'jquery';
import ReactGA from 'react-ga';
import {backendURL} from '../backend.js'
import ReactDOM from 'react-dom';
import { Login , logout } from '@ksf-media/user';
import spacing from '@material-ui/core/styles/spacing';
import { Hidden } from '@material-ui/core';
import cogoToast from 'cogo-toast';

const red = '#EF5350';
const green = '#66BB6A';

ReactGA.initialize('UA-119802236-1');

export default class Question extends React.Component {
  constructor(props){
    super(props);
    this.state = {
      searchText: "",
      question: '',
      category: '',
      hint: '',
      progress: 1,
      hintPoint: 5,
      completed: 0,
      tally: 0,
      displayResult: false,
      check: 'Skriv in ditt svar här',
      opacity: 0,
      color: 'white',
      userInput: '',
      quizData: [],
      right: [],
      logged_in: true,
      is_loading: 'hidden',
      name: '',
      options:'',
      message: '',
    };
    this.handleClick = this.handleClick.bind(this);
  };


  async componentDidMount() {
    ReactGA.pageview(window.location.pathname + window.location.search);
   try {
     const res = await fetch(backendURL + 'get/all/quizzes/as/json/' + this.props.match.params.id);
     var quizData = await res.json();
     this.setState({
       quizData: quizData,
       question: quizData.questions.question1.question,
       category: quizData.questions.question1.category,
       hint: quizData.questions.question1.hints.hint1,
     });
     }catch (e) {
      console.log(e);
     }
  }

  inputChange(event){
    this.setState({
      searchText: event.target.value,
      userInput: event.target.value
    })
  }
  setInputValue(title){
    this.setState({
      userInput: title,
      //this will remove all wikilinks components
      searchText: ''
    })
  }

  handleClick(e){
    e.preventDefault();
    this.handleAnswer(e);
    this.handleWrongRight(e);
    this.setState({
      userInput: '',
    })
  };

  handleWrongRight(e){
    e.preventDefault();
    const {tally, hintPoint} = this.state;
    if(this.state.userInput === this.checkIfCorrect()){
      cogoToast.success('Du fick den rätt');
      this.setState({tally: tally + hintPoint});

    }else{
      if(hintPoint === 1){
        hide()
        cogoToast.info('Den fo tyvärr fel men nu var det dags för nästa')
      }else{
        cogoToast.error('Fel nytt försök')
      }
    }
  };
  checkIfCorrect(){
    const questionOptions = Object.getOwnPropertyNames(this.state.quizData.questions)
    return this.state.quizData.questions[questionOptions[this.state.progress-1]].answer
  }

  handleResults(e){
    e.preventDefault();
    if(this.state.userInput === this.checkIfCorrect()){
      this.setState({
        right: [...this.state.right, ' Du svarade rätt på ledtråden värd ' + this.state.hintPoint + 'p']
      });
    }else{
      this.setState({
      right: [...this.state.right, ' Du svarade fel på denna fråga.']
      });
    }
  }

  getNextQuestion(e){
    this.handleResults(e)
    if (this.state.progress === 5){
      this.setState({displayResult: true});
    }
    else{
      const questionOptions = Object.getOwnPropertyNames(this.state.quizData.questions)
      this.setState({
        question: this.state.quizData.questions[questionOptions[this.state.progress]].question, 
        category: this.state.quizData.questions[questionOptions[this.state.progress]].category, 
        hint: this.state.quizData.questions[questionOptions[this.state.progress]].hints.hint1,
        hintPoint: 5, 
        completed: this.state.completed + 20, 
        progress: this.state.progress + 1, 
      });
    }
  }

  getNextHint(e){
    if (this.hintPoint === 1){
      this.getNextQuestion(e)
    }
    else{
      const questionOptions = Object.getOwnPropertyNames(this.state.quizData.questions)
      const hintOptions = (Object.getOwnPropertyNames(this.state.quizData.questions[questionOptions[this.state.progress - 1]].hints)).sort();
      for(var i = 0; i < 4; i++){
        if (this.state.hint === this.state.quizData.questions[questionOptions[this.state.progress - 1]].hints[hintOptions[i]]){
          this.setState({
            hint: this.state.quizData.questions[questionOptions[this.state.progress - 1]].hints[hintOptions[i+1]] ,
            hintPoint: 4 - i, 
          });
          return 0  
        }
      }
      return this.getNextQuestion(e)
    }


  }

  handleAnswer(e){
    e.preventDefault();
    if (this.state.userInput === this.checkIfCorrect()){
      this.getNextQuestion(e)
    }else{
      this.getNextHint(e)
    }
  }

  logg_in_worked(user){
    this.setState({logged_in: true, name: user['firstName']});
  };


  loaded(){
    this.setState({is_loading: "visible"});
  };

  render() {
    const {displayResult, logged_in, is_loading} = this.state;
    if (logged_in === false){
        return(
          <div style={{visibility: is_loading}} >
            <Login onUserFetchSuccess={(user) => this.logg_in_worked(user)} onLoadingEnd={ () => this.loaded() } />
          </div>
          );
    }else {
      if (displayResult === true){
        return(<Resultat tally={this.state.tally} quizData={this.state.quizData} right={this.state.right}/>);
      }else {
        return (
          <div className='question'>
       {/*bug in ksf-media/user
          ksf-media/user.logout return a function that it is not supposed to 
          this is the solution for now 
          <button id='logout' onClick={() => logout(() => this.setState({logged_in: false, is_loading: "visible"}))() } style={{boxShadow: 'none',}}>Byt konto</button>
*/}
            <div className="row">
              <div className="col-1">
                <MuiThemeProvider>
                  <ExitDialog />
                </MuiThemeProvider>
                </div>
              <p className="col-10 text-center font-italic">Fråga {this.state.progress} av 5</p>
              <div className="col-1"></div>
            </div>
            <div className="row">
              <div className="col-12">
              <MuiThemeProvider>
                <LinearProgress mode="determinate" value={this.state.completed} />
              </MuiThemeProvider>
              </div>
              <div className="col-12">
              <p className="header">{this.state.hintPoint} poängs fråga</p>
              </div>
            </div>
            <div className="row">
              <div className="col text-center">
                <h2>{this.state.category} <br /> {this.state.question}</h2>
              </div>
            </div>
            <div className="row">
              <div className="col">
                <h3>{this.state.hint}</h3>
              </div>
            </div>
            <div className="row">
              <div className="col">
              <input id="input_text" className="w-100 mt-3 mb-4" type="text" value={this.state.userInput} onChange={this.inputChange.bind(this)}></input>
              <div id='output_options' className="d-flex flex-column">
                <WikiLink search={this.state.searchText} onClick={this.setInputValue.bind(this)}>

                </WikiLink>
              </div>
            </div>
          </div>

            <div className="row">
              <div className="col-md mt-3">
              <button onClick={this.handleClick} className='start questionBtn'>Hoppa över</button>
              </div>
              <div className="col-md mt-3">
              <button onClick={this.handleClick} className='start questionBtn'>Svara</button>
              </div>
            </div>
        </div>

        );
      };
    };
  };
};
