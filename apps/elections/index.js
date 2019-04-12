import React from 'react';
import ReactDOM from 'react-dom';
import App from './components/App.js';
import { HashRouter as Router, Route } from "react-router-dom";

ReactDOM.render(
  <Router>
    <Route
      path="/area/:areaId"
      component={App}>
    </Route>
    <Route
      exact path="/"
      component={App}>
    </Route>
    <Route
      path="/"
      component={App}>
    </Route>
  </Router>
  , document.getElementById('root')
);
