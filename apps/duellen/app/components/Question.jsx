import React from 'react';
import AutoComplete from 'material-ui/AutoComplete';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
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

const red = '#EF5350';
const green = '#66BB6A';

ReactGA.initialize('UA-119802236-1');

export default class Question extends React.Component {
  constructor(props){
    super(props);
    this.state = {
      searchText: '',
      question: '',
      category: '',
      hint: '',
      progress: 1,
      hintPoint: 5,
      completed: 20,
      tally: 0,
      displayResult: false,
      check: 'Skriv in ditt svar här',
      opacity: 0,
      color: 'white',
      dataSource: [],
      quizData: [],
      right: [],
      logged_in: true,
      is_loading: 'hidden',
      name: ''
    };
    this.handleUpdateInput = this.handleUpdateInput.bind(this);
    this.handleClick = this.handleClick.bind(this);
  };


  async componentDidMount() {
    ReactGA.pageview(window.location.pathname + window.location.search);
   try {
     const res = await fetch(backendURL + 'get/all/quizzes/as/json/' + this.props.match.params.id);
     var quizData = await res.json();
     this.setState({
       quizData,
     });
     this.setState({
       question: this.state.quizData.questions.question1.question,
       category: this.state.quizData.questions.question1.category,
       hint: this.state.quizData.questions.question1.hints.hint1,
     });
     }catch (e) {
      console.log(e);
     }
  }


  handleUpdateInput(searchText){
     $.ajax({
       url: "https://sv.wikipedia.org/w/api.php?action=opensearch&format=json&suggest=true&formatversion=2&search=" + searchText ,
       dataType: 'jsonp',
       success: function(response) { 
         var pagesFiltered = Object.keys(response[1]).filter(function(key) {
          return [key]
         }).map(function(key) {
             var title = response[1][key];
             var extract = response[2][key];
             const children = (
               <div>
                 <h4>{title}</h4>
                </div>
             );
             const dataSourceItem = {
               text: title,
               value: (<MenuItem children={children} />)
             };
             return (
               dataSourceItem
             );
         });
         console.log(pagesFiltered)
       this.setState({dataSource: pagesFiltered});
     }.bind(this)
   });
  this.setState({dataSource: [], searchText: searchText});
  };


  handleClick(e){
    e.preventDefault();
    this.handleAnswer(e);
    this.handleWrongRight(e);
  };

  handleWrongRight(e){
    e.preventDefault();
    const {tally, hintPoint} = this.state;
    if(this.state.searchText === this.checkIfCorrect()){
      this.setState({check: 'Den fick du rätt!', opacity: 1, color: green,});
      this.setState({tally: tally + hintPoint});

    }else{
      console.log(hintPoint)
      if(hintPoint === 1){
      this.setState({
        check: 'Nu var det dags för nästa fråga, skriv in svaret här',
        color: "white",
      });
      }else{
        this.setState({check: 'Den fo lite fel, nytt försök!', opacity: 1, color: red,});  
      }
    }
  };

  checkIfCorrect(){
    const questionOptions = Object.getOwnPropertyNames(this.state.quizData.questions)
    return this.state.quizData.questions[questionOptions[this.state.progress-1]].answer
  }

  handleResults(e){
    e.preventDefault();
    if(this.state.searchText === this.checkIfCorrect()){
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
        searchText: '',
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
            hint: this.state.quizData.questions[questionOptions[this.state.progress - 1]].hints[hintOptions[i+1]] ,hintPoint: 4 - i, searchText: '' 
          });
          return 0  
        }
      }
      return this.getNextQuestion(e)
    }


  }

  handleAnswer(e){
    e.preventDefault();
    if (this.state.searchText === this.checkIfCorrect()){
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
            <div class="row">
              <div class="col-1">
                <MuiThemeProvider>
                  <ExitDialog />
                </MuiThemeProvider>
                </div>
              <p class="col-10 text-center font-italic">Fråga {this.state.progress} av 5</p>
              <div class="col-1"></div>
            </div>
            <div class="row">
              <div class="col-12">
              <MuiThemeProvider>
                <LinearProgress mode="determinate" value={this.state.completed} />
              </MuiThemeProvider>
              </div>
              <div class="col-12">
              <p className="header">{this.state.hintPoint} poängs fråga</p>
              </div>
            </div>
            
            <div class="row">
              <h2>{this.state.category} <br /> {this.state.question}</h2>
            </div>
            <div class="row">
              <h3 style={{margin:10,}}>{this.state.hint}</h3>
            </div>
            <div class="row">
              <div class="col">
              <input type="text" name="question_input" maxlength="200" class="textinput textInput form-control" id="question_input"></input>
              <div id='output_options' class="d-flex flex-column"></div>
            </div>
          </div>

            <div class="row">
              <div class="col-md mt-3">
              <button onClick={this.handleClick} class='start questionBtn'>Hoppa över</button>
              </div>
              <div class="col-md mt-3">
              <button onClick={this.handleClick} class='start questionBtn'>Svara</button>
              </div>
            </div>
        </div>

        );
      };
    };
  };
};
