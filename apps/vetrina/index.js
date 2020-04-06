import React from 'react';
import ReactDOM from 'react-dom';

// yup, welcome to react 16
import createReactClass from 'create-react-class';
React.createClass = createReactClass;
var Main = require('./output/Vetrina.Main/index.js');

var hblPremium = {
  name: "HBL Premium",
  id: "HBL WEBB",
  description: [
    "För 6,90€/månad får du tillgång till alla artiklar på hbl.fi",
    "Du kan avsluta när du vill."
  ],
  price: 6.9,
  packageName: "HblPremium",
  imageUrl: null
}

function main() {
  const myComponent = (
    <Main.app products={[hblPremium]} onClose={() => { location.reload() }}/>
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
