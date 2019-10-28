import React from 'react';
import BackBtn from './BackBtn.jsx';
import EmailDialog from './EmailDialog.jsx';
import Button from '@material-ui/core/Button';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import {Tabs, Tab} from 'material-ui/Tabs';
import SwipeableViews from 'react-swipeable-views';
import {FacebookShareButton, FacebookIcon, TwitterShareButton, TwitterIcon, EmailShareButton, EmailIcon} from 'react-share';
import ReactGA from 'react-ga';

const styles = {
  headline: {
    fontSize: 26,
    paddingTop: 16,
    marginBottom: 12,
    fontWeight: 400,
  },
  progress: {
    padding: '5px', 
    backgroundColor:'rgba(0, 188, 212, 0.1',
  },
  border: {
    borderTop: '1px solid #00BCD4',
  },
};

ReactGA.initialize('UA-119802236-1');

export default class Resultat extends React.Component{
  constructor(props){
    super(props);
    this.state = {
      slideIndex: 0,
      player1: this.props.quizData.players.player1.name,
      player2: this.props.quizData.players.player2.name,
      player1_img: this.props.quizData.players.player1.img,
      player2_img: this.props.quizData.players.player2.img,
      player1_score: this.props.quizData.players.player1.score,
      player2_score: this.props.quizData.players.player2.score,
      questions: [[this.props.quizData.questions.question1,0], 
                  [this.props.quizData.questions.question2,1],
                  [this.props.quizData.questions.question3,2],
                  [this.props.quizData.questions.question4,3],
                  [this.props.quizData.questions.question5,4],
      ],

    };
    this.handleChange = this.handleChange.bind(this);
  }

  componentDidMount(){
    ReactGA.pageview(window.location.pathname + window.location.search);
    ReactGA.event({
      category: 'User',
      action: 'Completed Quiz'
    });
  }

  handleChange(value){
    if(this.state.slideIndex === 0){
      this.setState({slideIndex: 1,});
    }else{
      this.setState({slideIndex: 0});
    }

 };

 render() {
    const shareUrl = 'http://hbl.fi';
    const title = 'Duellen Digitalt!';
    const hashtag = '#duellen';
    const hashtags = ['duellen','hbl'];
    return (
      <MuiThemeProvider>
        <div>
          <div style={{backgroundColor:'#f07e26',padding: 10}}>
            <BackBtn />
          </div>

          <Tabs
            onChange={this.handleChange}
            value={this.state.slideIndex}
            onClick={this.handleChange}
          >
            <Tab label="Dina resultat" value={0} />
            <Tab label="Rätta svar" value={1} />
          </Tabs>
          <EmailDialog />
          <SwipeableViews
            index={this.state.slideIndex}
            onChangeIndex={this.handleChange}
          >

            <div>
              <div style={{paddingTop: 12}}>

              <div style={{textAlign: 'center'}}>
                  <h2 style={{paddingBottom: '12%', paddingTop: '12%'}}>Du fick <br/> {this.props.tally} poäng!</h2>
              </div>

                    <div className="players">
                      <div style={{float: 'left', width: '50%',textAlign: 'center'}}>
                        <img src={this.state.player1_img} style={{objectFit: 'cover', width:160, height:160, borderRadius: '50%'}}></img>
                        <p>{this.state.player1} fick <br></br><b>{this.state.player1_score}</b> poäng!</p>
                      </div>
                      <div style={{float: 'left', width: '50%', textAlign: 'center'}}>
                        <img src={this.state.player2_img} style={{objectFit: 'cover', width:160, height:160, borderRadius: '50%'}}></img>
                        <p>{this.state.player2} fick <br></br><b>{this.state.player2_score}</b> poäng!</p>
                      </div>
                    </div>


                  <div className="share_buttons" style={{display: 'inline-block', textAlign: 'center', width: '100%', cursor: 'pointer'}}>
                    <p style={{padding:'5%'}}>Dela ditt resultat med vänner!</p>
                      <FacebookShareButton
                        style={{display: 'inline-block', margin: '5px'}}
                        url={shareUrl}
                        quote={title}
                        hashtag={hashtag}
                        className="Demo__some-network__share-button">
                        <FacebookIcon
                          size={45}
                          round />
                      </FacebookShareButton>

                      <TwitterShareButton
                        style={{display: 'inline-block', margin: '5px'}}
                        url={shareUrl}
                        title={title}
                        hashtags={hashtags}
                        className="Demo__some-network__share-button">
                        <TwitterIcon
                          size={45}
                          round />
                      </TwitterShareButton>

                      <EmailShareButton
                        style={{display: 'inline-block', margin: '5px'}}
                         url={shareUrl}
                         subject={title}
                         body="body"
                         className="Demo__some-network__share-button">
                         <EmailIcon
                           size={45}
                           round />
                       </EmailShareButton>
                  </div>
              </div>
            </div>



            <div style={styles.slide}>              
              <div style={{textAlign: 'left', padding: '7%', lineHeight: '1.8'}}>
              {this.state.questions.map(item => (
                  <div style={{marginBottom: '10px'}}>
                    <p className='facit'>{item[0].category} {item[0].question}</p>
                    <p className='progress' style={{padding: '5px', backgroundColor:'rgba(0, 188, 212, 0.1)'}}>{this.props.right[item[1]]}</p>
                    <b>5 poäng</b> {item[0].hints.hint1} <br/>
                    <b>4 poäng</b> {item[0].hints.hint2} <br/>
                    <b>3 poäng</b> {item[0].hints.hint3} <br/>
                    <b>2 poäng</b> {item[0].hints.hint4} <br/>
                    <b>1 poäng</b> {item[0].hints.hint5} <br/>
                    <p className='progress'>Rätt svar: {item[0].answer}</p>
                  </div>
                ))}
              </div>
            </div>
          </SwipeableViews>
        </div>
      </MuiThemeProvider>
    );
  }


};
