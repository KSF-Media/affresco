import React from 'react';
import "core-js/stable";
import {BrowserRouter, Route, Switch} from 'react-router-dom';

import Start from './components/Start.jsx';
import Intro from './components/Intro.jsx';
import Question from './components/Question.jsx';
import Resultat from './components/Resultat.jsx';

export default () => (
    <BrowserRouter>
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
