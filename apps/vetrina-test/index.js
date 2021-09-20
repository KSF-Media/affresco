import React from "react";
import ReactDOM from "react-dom";

// yup, welcome to react 16
import createReactClass from "create-react-class";
React.createClass = createReactClass;
var Main = require("./output/VetrinaTest.Main/index.js");

function main() {
  // NOTE: These are mainly for automated tests
  let queryString = new URLSearchParams(window.location.search);
  let paperInvoice = queryString.get("paperInvoice") == "true";
  let paperProduct = queryString.get("paperProduct") == "true";
  let multipleProducts = queryString.get("multipleProducts") == "true";
  let minimalLayout = queryString.get("minimalLayout") == "true";

  let products = [];
  let paymentMethods = ["creditcard"];

  if (paperProduct) {
    products.push(hblPaper);
  } else if (multipleProducts) {
    products.push(hblPremium, hblPaper);
  } else {
    products.push(hblPremium);
  }

  if (paperInvoice) {
    paymentMethods.push("paperinvoice");
  }

  const myComponent = (
    <Main.jsComponent
      products={products}
      accessEntitlements={["hbl-365", "articles-365"]}
      paymentMethods={paperInvoice ? ["creditcard", "paperinvoice"] : ["creditcard"]}
      minimalLayout={minimalLayout}
      headline={
        <div>
          Läs HBL digitalt för <span className="vetrina--price-headline">endast 1€</span>
        </div>
      }
      onClose={() => console.log("ok")}
      paper="HBL"
      orderSource="PaywallSource"
    />
  );

  ReactDOM.render(myComponent, document.getElementById("app"));
}

var hblPaper = {
  id: "HBL P+D LÖ",
  description: (
    <div>
      Kvalitetsjournalistik när, var och hur du vill <br />
      HBL WEEKEND LÖ-SÖ! A paper product yo.
    </div>
  ),
  priceCents: 2990,
  descriptionPurchaseCompleted: "Du kan nu läsa Premiumartiklar på HBL.fi.",
  name: "Hufvudstadsbladet LÖ-SÖ",
  contents: [
    {
      title: "Premium",
      description: "Alla artiklar på hbl.fi",
    },
    {
      title: "Nyhetsappen HBL Nyheter",
      description: "Nyheter på mobilen och surfplattan, pushnotiser",
    },
    {
      title: "Digitalt månadsbrev",
      description: "Nyheter & förmåner",
    },
  ],
};

var hblPremium = {
  id: "HBL WEBB",
  description: (
    <div>
      Kvalitetsjournalistik när, var och hur du vill <br />
      Läs Hufvudstadsbladet för 1€ i en månad, därefter 6,90€ / månad tills vidare. Du kan avsluta när du vill.
    </div>
  ),
  priceCents: 690,
  descriptionPurchaseCompleted: "Du kan nu läsa Premiumartiklar på HBL.fi.",
  name: "Hufvudstadsbladet Premium",
  contents: [
    {
      title: "Premium",
      description: "Alla artiklar på hbl.fi",
    },
    {
      title: "Nyhetsappen HBL Nyheter",
      description: "Nyheter på mobilen och surfplattan, pushnotiser",
    },
    {
      title: "Digitalt månadsbrev",
      description: "Nyheter & förmåner",
    },
  ],
  campaign: {
    length: 3,
    priceEur: 19.9,
    lengthUnit: "Month",
    no: 4033,
    name: "VÅR PRE 2020",
    id: "VÅREN 2020",
  },
};

var hbl365 = {
  id: "HBL 365",
  description: (
    <div>
      För 14,90€/månad för tillgång till alla e-tidningar i e-tidningsappen HBL 365, alla artiklar på HBL.fi och våra
      andra sajter, och alla artiklar i nyhetsappen HBL Nyheter och våra andra nyhetsappar.
      <br />
      <b>Första månaden för 1 €!</b>
    </div>
  ),
  priceCents: 1490,
  descriptionPurchaseCompleted: "Du kan nu läsa Premiumartiklar på HBL.fi.",
  name: "HBL 365",
};

if (module.hot) {
  module.hot.accept(function () {
    console.log("running main again");
    main();
  });
}

console.log("starting");
main();
