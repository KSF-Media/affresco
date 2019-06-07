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
      progress: '1',
      hintPoint: '5',
      completed: 20,
      tally: '0',
      displayResult: false,
      check: '',
      opacity: 0,
      color: '',
      dataSource: [],
      quizData: [],
      right: []
    };
    this.handleUpdateInput = this.handleUpdateInput.bind(this);
    this.handleClick = this.handleClick.bind(this);
  };

  async componentDidMount() {
    ReactGA.pageview(window.location.pathname + window.location.search);
   try {
     const res = await fetch(backendURL + 'duellen/api/' + this.props.match.params.id + '/');
     const quizData = await res.json();
     this.setState({
       quizData,
     });
     this.setState({
       question: this.state.quizData.first_question,
       category: this.state.quizData.first_category,
       hint: this.state.quizData.first_hint5,
     });
     }catch (e) {
      console.log(e);
     }
     console.log(this.state.quizData);
   }


   handleUpdateInput(searchText){
     this.setState({dataSource: [], searchText: searchText});
     $.ajax({
       url: "https://sv.wikipedia.org/w/api.php?format=json&action=query&generator=search&gsrnamespace=0&gsrlimit=10&prop=extracts&exintro&explaintext&exchars=38&gsrsearch=" + this.state.searchText ,
       dataType: 'jsonp',
       success: function(response) {
         var pages = response.query.pages;
         var pagesFiltered = Object.keys(pages).filter(function(key) {
             return [key];
         }).map(function(key) {
             var title = pages[key].title;
             var extract = pages[key].extract;
             const children = (
               <div>
                 <h4 style={{marginBottom: '-30px'}}>{title}</h4>
                 <p style={{fontSize:'16px', color: '#808080'}}>{extract}</p>
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
   };

  handleClick(e){
    e.preventDefault();
    this.handleScore(e);
    this.handleAnswer(e);
    this.handleResults(e);
    this.handleWrongRight(e);
  };

  handleWrongRight(e){
    e.preventDefault();
    var searchText = this.state.searchText.toLocaleLowerCase();

    if(searchText === this.state.quizData.first_answer.toLocaleLowerCase() && this.state.completed === 20 || searchText === this.state.quizData.second_answer.toLocaleLowerCase() && this.state.completed === 40 || searchText === this.state.quizData.third_answer.toLocaleLowerCase() && this.state.completed === 60 || searchText === this.state.quizData.fourth_answer.toLocaleLowerCase() && this.state.completed === 80 || searchText === this.state.quizData.fifth_answer.toLocaleLowerCase() && this.state.completed === 100 ){
    this.setState({check: 'Rätt!', opacity: 1, color: green,}, () => setTimeout(() => this.setState({check: '', opacity:0}),750));
  }else{
    this.setState({check: 'Fel!', opacity: 1, color: red,}, () => setTimeout(() => this.setState({check: '', opacity:0}),750));  }
  };

  handleResults(e){
    e.preventDefault();
    var quizData = this.state.quizData;
    var searchText = this.state.searchText.toLocaleLowerCase();

    if(searchText === this.state.quizData.first_answer.toLocaleLowerCase() && this.state.completed === 20 || searchText === this.state.quizData.second_answer.toLocaleLowerCase() && this.state.completed === 40 || searchText === this.state.quizData.third_answer.toLocaleLowerCase() && this.state.completed === 60 || searchText === this.state.quizData.fourth_answer.toLocaleLowerCase() && this.state.completed === 80 || searchText === this.state.quizData.fifth_answer.toLocaleLowerCase() && this.state.completed === 100 ){
      this.setState({
      right: [...this.state.right, ' Du svarade rätt på ledtråden värd ' + this.state.hintPoint + 'p!']
      }, () => {
          console.log(this.state.right);
      });
    }else if(this.state.hint === quizData.first_hint1 && this.state.searchText !== this.state.quizData.first_answer || this.state.hint === quizData.second_hint1 && this.state.searchText !== this.state.quizData.second_answer ||
      this.state.hint === quizData.third_hint1 && this.state.searchText !== this.state.quizData.third_answer || this.state.hint === quizData.fourth_hint1 && this.state.searchText !== this.state.quizData.fourth_answer || this.state.hint === quizData.fifth_hint1 && this.state.searchText !== this.state.quizData.fifth_answer){
      this.setState({
      right: [...this.state.right, ' Du svarade fel på denna fråga.']
      }, () => {
          console.log(this.state.right);
      });
    }
  }

  handleScore(e){
    e.preventDefault();
    const {tally, hintPoint} = this.state;
    var searchText = this.state.searchText.toLocaleLowerCase();
    if(searchText === this.state.quizData.first_answer.toLocaleLowerCase() && this.state.completed === 20 || searchText === this.state.quizData.second_answer.toLocaleLowerCase() && this.state.completed === 40 || searchText === this.state.quizData.third_answer.toLocaleLowerCase() && this.state.completed === 60 || searchText === this.state.quizData.fourth_answer.toLocaleLowerCase() && this.state.completed === 80 || searchText === this.state.quizData.fifth_answer.toLocaleLowerCase() && this.state.completed === 100){
      this.setState({tally: +tally + +hintPoint});
    }
  };


  handleAnswer(e){
  e.preventDefault();
  const {displayResult} = this.state;
  var quizData = this.state.quizData;
  var searchText = this.state.searchText.toLocaleLowerCase();


  if (searchText === quizData.first_answer.toLocaleLowerCase() && this.state.completed === 20){
    this.setState({
      question: quizData.second_question,category: quizData.second_category,hint: quizData.second_hint5,hintPoint: 5, completed: 40, progress: 2, searchText: '',});
    }else if(this.state.hint === quizData.first_hint5){
      this.setState({
        hint: quizData.first_hint4, hintPoint: 4, searchText: '', });
    }else if(this.state.hint === quizData.first_hint4){
      this.setState({
        hint: quizData.first_hint3,hintPoint: 3, searchText: '',});
    }else if(this.state.hint === quizData.first_hint3){
      this.setState({
        hint: quizData.first_hint2,hintPoint: 2, searchText: '', });
    }else if(this.state.hint === quizData.first_hint2){
      this.setState({
        hint: quizData.first_hint1,hintPoint: 1, searchText: '', });
    }else if(this.state.hint === quizData.first_hint1){


      this.setState({
        question: quizData.second_question,category: quizData.second_category,hint: quizData.second_hint5,hintPoint: 5,completed: 40,progress: 2, searchText: '', });
    }else if(searchText === quizData.second_answer.toLocaleLowerCase() && this.state.completed === 40){
      this.setState({
        question: quizData.third_question,category: quizData.third_category,hint: quizData.third_hint5,hintPoint: 5,completed: 60,progress: 3, searchText: '', });
    }else if(this.state.hint === quizData.second_hint5){
      this.setState({
        hint: quizData.second_hint4,hintPoint: 4, searchText: '', });
    }else if(this.state.hint === quizData.second_hint4){
      this.setState({
        hint: quizData.second_hint3,hintPoint: 3, searchText: '', });
    }else if(this.state.hint === quizData.second_hint3){
      this.setState({
        hint: quizData.second_hint2,hintPoint: 2, searchText: '', });
    }else if(this.state.hint === quizData.second_hint2){
      this.setState({
        hint: quizData.second_hint1,hintPoint: 1, searchText: '', });
    }else if(this.state.hint === quizData.second_hint1){


      this.setState({
        question: quizData.third_question,category: quizData.third_category,hint: quizData.third_hint5,hintPoint: 5,completed: 60,progress: 3, searchText: '',});
    }else if(searchText === quizData.third_answer.toLocaleLowerCase() && this.state.completed === 60){
      this.setState({
        question: quizData.fourth_question,category: quizData.fourth_category,hint: quizData.fourth_hint5,hintPoint: 5,completed: 80,progress: 4, searchText: '',});
    }else if(this.state.hint === quizData.third_hint5){
      this.setState({
        hint: quizData.third_hint4,hintPoint: 4, searchText: '',});
    }else if(this.state.hint === quizData.third_hint4){
      this.setState({
        hint: quizData.third_hint3,hintPoint: 3, searchText: '',});
    }else if(this.state.hint === quizData.third_hint3){
      this.setState({
        hint: quizData.third_hint2,hintPoint: 2, searchText: '',});
    }else if(this.state.hint === quizData.third_hint2){
      this.setState({
        hint: quizData.third_hint1,hintPoint: 1, searchText: '',});
    }else if(this.state.hint === quizData.third_hint1){


      this.setState({
        question: quizData.fourth_question,category: quizData.fourth_category,hint: quizData.fourth_hint5,hintPoint: 5,completed: 80,progress: 4, searchText: '',});
    }else if(searchText === quizData.fourth_answer.toLocaleLowerCase() && this.state.completed === 80){
      this.setState({
        question: quizData.fifth_question,category: quizData.fifth_category,hint: quizData.fifth_hint5,hintPoint: 5,completed: 100,progress: 5, searchText: '',});
    }else if(this.state.hint === quizData.fourth_hint5){
      this.setState({
        hint: quizData.fourth_hint4,hintPoint: 4, searchText: '',});
    }else if(this.state.hint === quizData.fourth_hint4){
      this.setState({
        hint: quizData.fourth_hint3,hintPoint: 3, searchText: '',});
    }else if(this.state.hint === quizData.fourth_hint3){
      this.setState({
        hint: quizData.fourth_hint2,hintPoint: 2, searchText: '',});
    }else if(this.state.hint === quizData.fourth_hint2){
      this.setState({
        hint: quizData.fourth_hint1,hintPoint: 1, searchText: '',});
    }else if(this.state.hint === quizData.fourth_hint1){


      this.setState({
        question: quizData.fifth_question,category: quizData.fifth_category,hint: quizData.fifth_hint5,hintPoint: 5,completed: 100,progress: 5, searchText: '',});
    }else if(searchText === quizData.fifth_answer.toLocaleLowerCase() && this.state.completed === 100){
        this.setState({displayResult: true});
    }else if(this.state.hint === quizData.fifth_hint5){
      this.setState({
        hint: quizData.fifth_hint4,hintPoint: 4, searchText: '',});
    }else if(this.state.hint === quizData.fifth_hint4){
      this.setState({
        hint: quizData.fifth_hint3,hintPoint: 3, searchText: '',});
    }else if(this.state.hint === quizData.fifth_hint3){
      this.setState({
        hint: quizData.fifth_hint2,hintPoint: 2, searchText: '',});
    }else if(this.state.hint === quizData.fifth_hint2){
      this.setState({
        hint: quizData.fifth_hint1,hintPoint: 1, searchText: '',});
    }else if(this.state.hint === quizData.fifth_hint1){
      this.setState({displayResult: true});
    }
  };



  render() {

    const {displayResult, check} = this.state;

    if (displayResult === true){
      return(<Resultat tally={this.state.tally} quizData={this.state.quizData} right={this.state.right}/>);
    }else {
      return (
        <div className='question'>
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
            <Button variant="contained"onClick={this.handleClick} primary={true} style={{paddingRight: '1%', width: '50%', boxShadow: 'none',}}>Hoppa över</Button>
            <Button variant="contained" onClick={this.handleClick} primary={true} style={{paddingLeft: '1%', width: '50%', boxShadow: 'none',}}>Svara</Button>
          </span>
        </MuiThemeProvider>

      </div>

      );
    };
  };
};
