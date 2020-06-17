import React from 'react';
import ReactDOM from 'react-dom';

// yup, welcome to react 16
import createReactClass from 'create-react-class';
React.createClass = createReactClass;
var Main = require('./output/Article/index.js');

function main() {
  const myComponent = (
    <Main.article article={{title: "yolo"}}/>
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
