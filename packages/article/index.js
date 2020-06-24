import React from 'react';
import ReactDOM from 'react-dom';

// yup, welcome to react 16
import createReactClass from 'create-react-class';
React.createClass = createReactClass;
var Main = require('./output/Article/index.js');

function main() {

  fetch("https://lettera.api.ksfmedia.fi/v2/article/24f942aa-a4e7-4d18-92c6-0137b0a22309?textonly=false")
    .then(resp => resp.json())
    .then(json => {
      const myComponent = (
        <Main.article article={json}/>
      );
      ReactDOM.render(myComponent, document.getElementById('app'));
    });

}


if (module.hot) {
  module.hot.accept(function () {
    console.log('running main again');
    main();
  });
}

console.log('starting');
main();
