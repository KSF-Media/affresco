import React from 'react';
import ReactDOM from 'react-dom';

// yup, welcome to react 16
import createReactClass from 'create-react-class';
React.createClass = createReactClass;
var Main = require('./output/VetrinaTest.Main/index.js');

function main() {
  // HACK: As we only support one product now, we can easily switch between paper and digital product by an url hash.
  // This also makes testing the thing a lot easier
  const product = document.location.hash === '#paperProduct' ? hblPaper : hblPremium;

  const myComponent = (
    <Main.jsComponent
      products={[product]}
      accessEntitlements={["hbl-365", "articles-365"]}
      headline={<div>Läs HBL digitalt för <span className="vetrina--price-headline">endast 1€</span></div>}
      paper="HBL" />
  );

  ReactDOM.render(myComponent, document.getElementById('app'));
}

var hblPaper = {
  id: "HBL P+D LÖ",
  description:
    <div>
      Kvalitetsjournalistik när, var och hur du vill <br />
      HBL WEEKEND LÖ-SÖ! A paper product yo.
    </div>,
  priceCents: 2990,
  descriptionPurchaseCompleted: "Du kan nu läsa Premiumartiklar på HBL.fi.",
  name: "Hufvudstadsbladet Premium",
  contents: [
    {
      title: "Premium",
      description: "Alla artiklar på hbl.fi"
    },
    {
      title: "Nyhetsappen HBL Nyheter",
      description: "Nyheter på mobilen och surfplattan, pushnotiser"
    },
    {
      title: "Digitalt månadsbrev",
      description: "Nyheter & förmåner"
    }
  ]
}

var hblPremium = {
  id: "HBL WEBB",
  description:
    <div>
      Kvalitetsjournalistik när, var och hur du vill <br />
      Läs Hufvudstadsbladet för 1€ i en månad, därefter 6,90€ / månad tills vidare. Du kan avsluta när du vill.
    </div>,
  priceCents: 690,
  descriptionPurchaseCompleted: "Du kan nu läsa Premiumartiklar på HBL.fi.",
  name: "Hufvudstadsbladet Premium",
  contents: [
    {
      title: "Premium",
      description: "Alla artiklar på hbl.fi"
    },
    {
      title: "Nyhetsappen HBL Nyheter",
      description: "Nyheter på mobilen och surfplattan, pushnotiser"
    },
    {
      title: "Digitalt månadsbrev",
      description: "Nyheter & förmåner"
    }
  ],
  campaignNo: 4052 // NOTE! This id exists only in staging!
}

var hbl365 = {
  id: "HBL 365",
  description:
    <div>
      För 14,90€/månad för tillgång till alla e-tidningar i e-tidningsappen HBL 365, alla artiklar på HBL.fi och våra andra sajter, och alla artiklar i nyhetsappen HBL Nyheter och våra andra nyhetsappar.<br />
      <b>Första månaden för 1 €!</b>
    </div>,
  priceCents: 1490,
  descriptionPurchaseCompleted: "Du kan nu läsa Premiumartiklar på HBL.fi.",
  name: "HBL 365",
  campaignNo: 3842
}

if (module.hot) {
  module.hot.accept(function () {
    console.log('running main again');
    main();
  });
}

console.log('starting');
main();
