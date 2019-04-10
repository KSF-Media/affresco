import React from 'react';
import {BrowserRouter, Route, Switch, browserHistory} from 'react-router-dom';

import Start from './Start.jsx';
import Intro from './Intro.jsx';
import Question from './Question.jsx';
import Resultat from './Resultat.jsx';

export default () => (
  <BrowserRouter  history={browserHistory}>
    <Switch>
      <Route exact path="/" exact component={Start} />
      <Route exact path="/Intro" exact component={Intro} />
      <Route exact path="/Intro/:id" exact component={Intro} />
      <Route exact path="/Question" exact component={Question} />
      <Route exact path="/Question/:id" exact component={Question} />
      <Route exact path="/Resultat" exact component={Resultat} />
      <Route exact path="/Resultat/:id" exact component={Resultat} />
    </Switch>
  </BrowserRouter>
);
