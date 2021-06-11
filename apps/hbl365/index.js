import React from 'react';
import ReactDOM from 'react-dom';

import createReactClass from 'create-react-class';
React.createClass = createReactClass;
var Main = require('./output/HBL365.Main/index.js');

function main() {
  const myComponent = (
    <Main.jsApp/>
  );

  ReactDOM.render(myComponent, document.getElementById('app'));
}


if (module.hot) {
  module.hot.accept(function () {
    console.log('running main again');
    main();
  });
}

console.log('starting');
main();
