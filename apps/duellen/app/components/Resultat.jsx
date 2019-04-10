import React from 'react';
import BackBtn from './BackBtn.jsx';
import EmailDialog from './EmailDialog.jsx';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';
import {Tabs, Tab} from 'material-ui/Tabs';
import {fruit, first, second,third, fourth, fifth} from './data/quizData.jsx';
import SwipeableViews from 'react-swipeable-views';
import {FacebookShareButton, FacebookIcon, TwitterShareButton, TwitterIcon, EmailShareButton, EmailIcon} from 'react-share';
import ReactGA from 'react-ga';

const styles = {
  headline: {
    fontSize: 24,
    paddingTop: 16,
    marginBottom: 12,
    fontWeight: 400,
  },
};

ReactGA.initialize('UA-119802236-1');

export default class Resultat extends React.Component{
  constructor(props){
    super(props);
    this.state = {
      slideIndex: 0,
      player1: this.props.quizData.player1,
      player2: this.props.quizData.player2,
      player1_img: this.props.quizData.player1_img,
      player2_img: this.props.quizData.player2_img,
      player1_score: this.props.quizData.player1_score,
      player2_score: this.props.quizData.player2_score
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
          <div style={{backgroundColor:'#00BCD4',padding: 10}}>
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
                  <p className='facit'>{this.props.quizData.first_category} {this.props.quizData.first_question}</p>
                  <p className='progress' style={{padding: '5px', backgroundColor:'rgba(0, 188, 212, 0.1)'}}>{this.props.right[0]}</p>
                  <b>1.</b> {this.props.quizData.first_hint5} <br/>
                  <b>2.</b> {this.props.quizData.first_hint4} <br/>
                  <b>3.</b> {this.props.quizData.first_hint3} <br/>
                  <b>4.</b> {this.props.quizData.first_hint2} <br/>
                  <b>5.</b> {this.props.quizData.first_hint1} <br/>
                  <p className='progress'>Rätt svar: {this.props.quizData.first_answer}</p><br/>

                  <hr style={{borderTop: '1px solid #00BCD4'}}></hr><br/>

                  <p className='facit'>{this.props.quizData.second_category} {this.props.quizData.second_question}</p>
                  <p className='progress' style={{padding: '5px', backgroundColor:'rgba(0, 188, 212, 0.1)'}}>{this.props.right[1]}</p>
                  <b>1.</b> {this.props.quizData.second_hint5} <br/>
                  <b>2.</b> {this.props.quizData.second_hint4} <br/>
                  <b>3.</b> {this.props.quizData.second_hint3} <br/>
                  <b>4.</b> {this.props.quizData.second_hint2} <br/>
                  <b>5.</b> {this.props.quizData.second_hint1} <br/>
                  <p className='progress'>Rätt svar: {this.props.quizData.second_answer}</p>
                <br/><hr style={{borderTop: '1px solid #00BCD4'}}></hr><br/>
                  <p className='facit'>{this.props.quizData.third_category} {this.props.quizData.third_question}</p>
                  <p className='progress' style={{padding: '5px', backgroundColor:'rgba(0, 188, 212, 0.1)'}}>{this.props.right[2]}</p>
                  <b>1.</b> {this.props.quizData.third_hint5} <br/>
                  <b>2.</b> {this.props.quizData.third_hint4} <br/>
                  <b>3.</b> {this.props.quizData.third_hint3} <br/>
                  <b>4.</b> {this.props.quizData.third_hint2} <br/>
                  <b>5.</b> {this.props.quizData.third_hint1} <br/>
                  <p className='progress'>Rätt svar: {this.props.quizData.third_answer}</p>
                <br/><hr style={{borderTop: '1px solid #00BCD4'}}></hr><br/>
                  <p className='facit'>{this.props.quizData.fourth_category} {this.props.quizData.fourth_question}</p>
                  <p className='progress' style={{padding: '5px', backgroundColor:'rgba(0, 188, 212, 0.1)'}}>{this.props.right[3]}</p>
                  <b>1.</b> {this.props.quizData.fourth_hint5} <br/>
                  <b>2.</b> {this.props.quizData.fourth_hint4} <br/>
                  <b>3.</b> {this.props.quizData.fourth_hint3} <br/>
                  <b>4.</b> {this.props.quizData.fourth_hint2} <br/>
                  <b>5.</b> {this.props.quizData.fourth_hint1} <br/>
                  <p className='progress'>Rätt svar: {this.props.quizData.fourth_answer}</p>
                <br/><hr style={{borderTop: '1px solid #00BCD4'}}></hr><br/>
                  <p className='facit'>{this.props.quizData.fifth_category} {this.props.quizData.fifth_question}</p>
                  <p className='progress' style={{padding: '5px', backgroundColor:'rgba(0, 188, 212, 0.1)'}}>{this.props.right[4]}</p>
                  <b>1.</b> {this.props.quizData.fifth_hint5} <br/>
                  <b>2.</b> {this.props.quizData.fifth_hint4} <br/>
                  <b>3.</b> {this.props.quizData.fifth_hint3} <br/>
                  <b>4.</b> {this.props.quizData.fifth_hint2} <br/>
                  <b>5.</b> {this.props.quizData.fifth_hint1} <br/>
                  <p className='progress'>Rätt svar: {this.props.quizData.fifth_answer}</p>
              </div>
            </div>
          </SwipeableViews>
        </div>
      </MuiThemeProvider>
    );
  }


};
