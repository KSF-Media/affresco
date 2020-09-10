import React from 'react';
import ReactDOM from 'react-dom';

// yup, welcome to react 16
import createReactClass from 'create-react-class';
React.createClass = createReactClass;
var Main = require('./output/VetrinaTest.Main/index.js');

function main() {
  const myComponent = (
	  <Main.app products={[hblPremium, hbl365]} accessEntitlements={["hbl-365", "articles-365"]}/>
  );

  ReactDOM.render(myComponent, document.getElementById('app'));
}


var hblPremium = {
  id: "HBL WEBB",
  description:
    <div>
      För 6,90€/månad får du tillgång till alla artiklar på hbl.fi <br />
      Du kan avsluta när du vill.
    </div>,
  priceCents: 690,
}

var hbl365 = {
  id: "HBL 365",
  description:
    <div>
	För 14,90€/månad för tillgång till alla e-tidningar i e-tidningsappen HBL 365, alla artiklar på HBL.fi och våra andra sajter, och alla artiklar i nyhetsappen HBL Nyheter och våra andra nyhetsappar. <b>Första månaden för 1 €!</b>
    </div>,
  priceCents: 1490,
}

if (module.hot) {
  module.hot.accept(function () {
    console.log('running main again');
    main();
  });
}

console.log('starting');
main();
