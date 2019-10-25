import React from 'react';
import AutoComplete from 'material-ui/AutoComplete';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import MenuItem from 'material-ui/MenuItem';
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
      check: '',
      opacity: 0,
      color: '',
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
     console.log(this.props.match.params.id)
     const res = await fetch(backendURL + 'get/all/quizzes/as/json/' + this.props.match.params.id);
     var quizData = await res.json();
     console.log(quizData)
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
                 <h4 style={{marginBottom: '-30px'}}>{title}</h4>
                 <p style={{fontSize:'16px', color: '#808080', overflow: 'hidden'}}>{extract}</p>
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
      this.setState({check: 'Rätt!', opacity: 1, color: green,}, () => setTimeout(() => this.setState({check: '', opacity:0}), 3000));
      this.setState({tally: tally + hintPoint});

    }else{
      this.setState({check: 'Fel!', opacity: 1, color: red,}, () => setTimeout(() => this.setState({check: '', opacity:0}), 3000));  
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
        searchText: ''
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
            <MuiThemeProvider>
              <ExitDialog />
            </MuiThemeProvider>

            <p className="progress">Fråga {this.state.progress} av 5</p>

            <MuiThemeProvider>
              <LinearProgress mode="determinate" value={this.state.completed} />
            </MuiThemeProvider>
            <p className="header">{this.state.hintPoint} poängs fråga</p>

            <h2>{this.state.category} <br /> {this.state.question}</h2>

            <p style={{height: 70,}}>{this.state.hint}</p>

            <div style={{height: 10, textAlign: 'right', padding: 20}}>
                <p className="progress" style={{color: this.state.color}}>{this.state.check}</p>
            </div>

          <MuiThemeProvider>
            <AutoComplete
              hintText="Ditt svar här"
              searchText={this.state.searchText}
              onUpdateInput={this.handleUpdateInput}
              dataSource={this.state.dataSource}
              filter={AutoComplete.fuzzyFilter}
              animated={false}
              fullWidth={true}
              maxSearchResults={3}
              style={{marginBottom: 30}}
              ></AutoComplete>
          </MuiThemeProvider>


          <MuiThemeProvider>
            <span>
              <Button variant="contained"onClick={this.handleClick} primary={true} style={{paddingRight: '1%', width: '49%', marginRight: '1%', boxShadow: 'none',}}>Hoppa över</Button>
              <Button variant="contained" onClick={this.handleClick} primary={true} style={{paddingLeft: '1%', width: '49%', marginLeft: '1%', boxShadow: 'none',}}>Svara</Button>
            </span>
          </MuiThemeProvider>

        </div>

        );
      };
    };
  };
};
