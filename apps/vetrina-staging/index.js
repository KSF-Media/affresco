import React from 'react';
import ReactDom from 'react-dom';
import {Vetrina} from '@ksf-media/vetrina';

var hblPremium = {
  id: "HBL WEBB",
  description: [
    "För 6,90€/månad får du tillgång till alla artiklar på hbl.fi",
    "Du kan avsluta när du vill."
  ],
  priceCents: 690,
}
ReactDom.render(
<div>
    <Vetrina  products={[hblPremium]}
              onClose={() => { location.reload() }}
              onLogin={() => console.log("Let me in!") }
              unexpectedError={<div>ERROR :~(</div>}
              accessEntitlements={["hbl-web"]} />
</div>, document.getElementById("root"));
