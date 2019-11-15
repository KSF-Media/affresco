import React from 'react';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import WikiLink from './WikiLink.jsx';
import ExitDialog from './ExitDialog.jsx';
import LinearProgress from 'material-ui/LinearProgress';
import Resultat from './Resultat.jsx';
import ReactGA from 'react-ga';
import {backendURL} from '../backend.js'
import { Login , logout } from '@ksf-media/user';
import cogoToast from 'cogo-toast';

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
      userInput: '',
      quizData: [],
      right: [],
      logged_in: true,
      is_loading: 'hidden',
      name: '',
      message: null,
      helpMessage: '',
      hasSelectedAnswer: false
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

  // will run for each key press on on the input field
  inputChange(event){
    // So the help text dosen't show if you have not typed anything into the searchfield
    if (event.target.value !== ''){
      this.setState({
        helpMessage: 'Välj ett svar på listan och klicka "Svara".'
      })
    }else{
      this.setState({
        helpMessage: ''
      })
    }
    this.setState({
      // For the wikilink component so it searches the latest user input
      searchText: event.target.value,
      // So the value displayed in the input field is the same as the user enters
      userInput: event.target.value
    })
  }
  
  // This will set the input fields value to the correct wikipedia pages title  
  setInputValue(title){
    this.setState({
      userInput: title,
      //this will remove all wikilinks components
      searchText: '',
      helpMessage: '',
      hasSelectedAnswer: true
    })
  }

  // This will run when you click 'svara'
  handleClick(e){
    if (!this.state.hasSelectedAnswer){
      this.hideMessage()
      // Gives a notification that the question was skip
      this.setState({message: cogoToast.info('Clicka på ett av svaren från listan', {toastContainerID: '1'})})
    }else{
      e.preventDefault();
      this.handleAnswer(e);
      this.handleWrongRight(e);
      this.setState({
        userInput: '',
        hasSelectedAnswer: false
      })
    }
  };

  // If user skips a question this will run so the user won't get a notification like wrong answer
  skipHint(e){
    e.preventDefault();
    this.hideMessage()
    // Gives a notification that the question was skip
    this.setState({message: cogoToast.info('Här är nästa ledtråd', {toastContainerID: '1'})})
    this.handleAnswer(e);
    // Sets the user input to nothing so the input field is empty
    this.setState({
      userInput: '',
    })
  };
  skipAnswer(e){
    e.preventDefault();
    this.hideMessage()
    // Gives a notification that the question was skip
    this.setState({message: cogoToast.info('Här är nästa fråga', {toastContainerID: '1'})})
    this.getNextQuestion(e)
    this.handleResults(e)
    // Sets the user input to nothing so the input field is empty
    this.setState({
      userInput: '',
    })
  };

  // If user answers on a question this function will run
  // This will give the user a notification how they answer
  // It will also add the number of points the question was worth to the total score
  handleWrongRight(e){
    e.preventDefault();
    const {tally, hintPoint} = this.state;
    if(this.state.userInput === this.checkIfCorrect()){
      this.hideMessage()
      var notification = cogoToast.success('Du fick den rätt', {toastContainerID: '1'});
      this.setState({tally: tally + hintPoint, message: notification});

    }else{
      if(hintPoint === 1){
        this.hideMessage()
        var notification = cogoToast.info('Den fo tyvärr fel men nu var det dags för nästa', {toastContainerID: '1'})
        this.setState({message: notification})
      }else{
        this.hideMessage()
        var notification = cogoToast.error('Fel nytt försök', {toastContainerID: '1'})
        this.setState({message: notification})
      }
    }
  };

  // hides the message so the notifications won't stack on each other
  // needs to check if there is something to hide away other wise it will crash
  hideMessage(){
    if (this.state.message !== null){
      this.state.message.hide()
    }
  }

  // returns the current questions answer
  checkIfCorrect(){
    const questionOptions = Object.getOwnPropertyNames(this.state.quizData.questions)
    return this.state.quizData.questions[questionOptions[this.state.progress-1]].answer
  }

  // Adds how the player did for each quiz
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

  // gets the next question
  // If all questions have been then it will load the result screen
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
        // tells witch question you are on
        progress: this.state.progress + 1, 
      });
    }
  }

  //gets the next hint
  getNextHint(e){
    if (this.state.hintPoint === 1){
      this.getNextQuestion(e)
    }
    else{
      const questionOptions = Object.getOwnPropertyNames(this.state.quizData.questions)
      const hintOptions = (Object.getOwnPropertyNames(this.state.quizData.questions[questionOptions[this.state.progress - 1]].hints)).sort();
      for(var i = 0; i < 4; i++){
        if (this.state.hint === this.state.quizData.questions[questionOptions[this.state.progress - 1]].hints[hintOptions[i]]){
          this.setState({
            hint: this.state.quizData.questions[questionOptions[this.state.progress - 1]].hints[hintOptions[i+1]] ,
            // It's 4 - i becouse if i=0 we willload the fourth hint
            // The fifth hint will never be loaded here so thats why it's 4 and not 5 
            hintPoint: 4 - i, 
          });
        }
      }
    }
  }

  // Gets the next question if the user answers correctly and the next hint if tha answer is wrong
  handleAnswer(e){
    e.preventDefault();
    if (this.state.userInput === this.checkIfCorrect()){
      this.getNextQuestion(e)
    }else{
      this.getNextHint(e)
    }
  }

  // For the loging               THIS FUNCTION IS DISABELD RIGHT NOW BECAUSE LOGIN IS DISABELD
  logg_in_worked(user){
    this.setState({logged_in: true, name: user['firstName']});
  };

  // For the loging to show the quisses if you are logged in     THIS FUNCTION IS DISABELD RIGHT NOW BECAUSE LOGIN IS DISABELD
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
                <h2>{this.state.category} : {this.state.question}</h2>
              </div>
            </div>
            <div className="row">
              <div className="col">
                <h4>{this.state.hint}</h4>
              </div>
            </div>
            <div className="row">
              <div className="col-9 mt-3">
                <input id="input_text" className="w-100 p-1 text-dark bg-black" type="text" value={this.state.userInput} onChange={this.inputChange.bind(this)} placeholder="Skriv in svaret här..."></input>
              </div>
              <div className="col-3 mt-3 text-center">
                <button onClick={this.handleClick} className='svarButton pr-3 pl-3'>Svara</button>
              </div>
            </div>
            <div className="row">
              <div className="col">
                <div className="mb-3">{this.state.helpMessage}</div>
                <div id='output_options' className="d-flex flex-column">
                  <WikiLink search={this.state.searchText} onClick={this.setInputValue.bind(this)}>
                  </WikiLink>
                </div>
              </div>
            </div>

            <div className="row">
              <div className="col mt-3 text-right">
                <button onClick={this.skipHint.bind(this)} className='skipButton questionBtn'>Nästa ledtråd</button>
              </div>
              <div className="col mt-3 text-left">
                <button onClick={this.skipAnswer.bind(this)} className='skipButton questionBtn'>Nästa fråga</button>
              </div>
            </div>
        </div>

        );
      };
    };
  };
};
