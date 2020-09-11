import React from 'react';
import ReactDOM from 'react-dom';

import '../../apps/app-article/src/index.css'

// yup, welcome to react 16
import createReactClass from 'create-react-class';
React.createClass = createReactClass;
var Main = require('./output/Article/index.js');

function main() {

  const articleUuid = location.hash.replace("#", "");

  fetch(`https://lettera.api.ksfmedia.fi/v2/article/${articleUuid}?textonly=false`)
    .then(resp => resp.json())
    .then(json => {
      const myComponent = (
        <Main.article 
          article={json}
          brand="hbl"
        />
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
