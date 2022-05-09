import React from "react";
import ReactDOM from "react-dom";
import { BrowserRouter as Router } from "react-router-dom";
import { rehydrateMarks } from "react-imported-component";
window.Buffer = window.Buffer || { isBuffer: () => false };
import "../../../less/mosaico.less";
import "../../../less/Vetrina.less";
import "../../../less/Login.less";

// yup, welcome to react 16
import createReactClass from "create-react-class";
React.createClass = createReactClass;
var Mosaico = require("../output/Mosaico/index.js").jsApp();

function main() {
  rehydrateMarks().then(() => {
    const mosaico = (
      <Mosaico
	article={window.article || null}
	articleType={window.articleType || null}
	mostReadArticles={window.mostReadArticles || null}
	staticPageName={window.staticPageName || null}
	categoryStructure={window.categoryStructure || null}
	globalDisableAds={window.globalDisableAds || null}
	initialFrontpageFeed={window.frontpageFeed || null}
	latestArticles={window.latestArticles || null}
	user={window.user || null}
	entitlements={window.entitlements || null}
      />
    );
    ReactDOM.hydrate(mosaico, document.getElementById("app"));
  });
}

if (module.hot) {
  module.hot.accept(function () {
    console.log("running main again");
    main();
  });
}

console.log("starting");

window.googletag = window.googletag || { cmd: [] };

// My suggestion would be to set all the ad slots in viewport on the initial
// page load as non-lazy and all the lower ad slots as lazy.
// Those would be Maxparad, parad and box_0 on desktop and mobparad_0
// on mobile I think. Rest can be set as lazy.

window.adSlots = {
  mobile: [
    {
      gamId: "MOBPARAD",
      sizes: [ [300,100], [300,250], [300,300], [300,431], [300,600] ],
      targetId: "mosaico-ad__top-parade",
      isLazy: false
    },
    {
      gamId: "MOBMITT",
      sizes: [ [300,100], [300,250], [300,300], [300,431], [300,600] ],
      targetId: "mosaico-ad__bigbox1",
      isLazy: true
    },
    {
      gamId: "MOBNER",
      sizes: [ [300,100], [300,250], [300,300], [300,431], [300,600] ],
      targetId: "mosaico-ad__bigbox2",
      isLazy: true
    },
    {
      gamId: "MOBBOX1",
      sizes: [ [300,100], [300,250], [300,300], [300,431], [300,600] ],
      targetId: "mosaico-ad__box1",
      isLazy: true
    },
    {
      gamId: "MOBBOX2",
      sizes: [ [300,100], [300,250], [300,300], [300,431], [300,600] ],
      targetId: "mosaico-ad__box2",
      isLazy: true
    },
    {
      gamId: "MOBBOX3",
      sizes: [ [300,100], [300,250], [300,300], [300,431], [300,600] ],
      targetId: "mosaico-ad__box3",
      isLazy: true
    },
    {
      gamId: "MOBBOX4",
      sizes: [ [300,100], [300,250], [300,300], [300,431], [300,600] ],
      targetId: "mosaico-ad__box4",
      isLazy: true
    },
    {
      gamId: "MOBBOX5",
      sizes: [ [300,100], [300,250], [300,300], [300,431], [300,600] ],
      targetId: "mosaico-ad__box5",
      isLazy: true
    },
    {
      gamId: "DIGIHELMOB",
      sizes: [300,431],
      targetId: "mosaico-ad__digihelmob",
      isLazy: true
    },
    {
      gamId: "INTERMOB",
      sizes: [300,500],
      targetId: "mosaico-ad__firstbox",
      isLazy: true
    },
  ],
  desktop: [
    {
      gamId: "DIGIHEL",
      sizes: [ [620,891], [620,991] ],
      targetId: "mosaico-ad__bigbox1",
      isLazy: true
    },
    {
      gamId: "JATTEBOX",
      sizes: [468,400],
      targetId: "mosaico-ad__bigbox2",
      isLazy: true
    },
    {
      gamId: "DIGIHELMOB",
      sizes: [300,431],
      targetId: "mosaico-ad__digihelmob",
      isLazy: true
    },
    {
      gamId: "PARAD",
      sizes: [ [980, 120], [980,400], [980,552] ],
      targetId: "mosaico-ad__parade",
      isLazy: false
    },
    {
      gamId: "MAXPARAD",
      sizes: [ [980, 120], [980,400], [980,480], [980,552], [1920,1080] ],
      targetId: "mosaico-ad__top-parade",
      isLazy: false
    },
    // {
    //   gamId: "STORTAVLA",
    //   sizes: [160,600],
    //   targetId: "mosaico-ad__tallbox",
    //   isLazy: true
    // },
    {
      gamId: "FIRSTBOX",
      sizes: [ [300, 250], [300, 300], [300, 431], [300,600] ],
      targetId: "mosaico-ad__firstbox",
      isLazy: true
    },
    {
      gamId: "BOX1",
      sizes: [ [300, 250], [300, 300], [300, 431], [300,600] ],
      targetId: "mosaico-ad__box1",
      isLazy: true
    },
    {
      gamId: "BOX2",
      sizes: [ [300, 250], [300, 300], [300, 431], [300,600] ],
      targetId: "mosaico-ad__box2",
      isLazy: true
    },
    {
      gamId: "BOX3",
      sizes: [ [300, 250], [300, 300], [300, 431], [300,600] ],
      targetId: "mosaico-ad__box3",
      isLazy: true
    },
    {
      gamId: "BOX4",
      sizes: [ [300, 250], [300, 300], [300, 431], [300,600] ],
      targetId: "mosaico-ad__box4",
      isLazy: true
    },
    {
      gamId: "BOX5",
      sizes: [ [300, 250], [300, 300], [300, 431], [300,600] ],
      targetId: "mosaico-ad__box5",
      isLazy: true
    },
    {
      gamId: "BOX",
      sizes: [ [300,250], [300,300], [300, 431], [300,600] ],
      targetId: "mosaico-ad__box",
      isLazy: false
    },
    // {
    //   gamId: "BOXFORPRINT",
    //   sizes: [300,300],
    //   targetId: "mosaico-ad__printbox",
    //   isLazy: true
    // },
    // {
    //   gamId: "WALLPAPER",
    //   sizes: [ [1600,1200], [1920,1080] ],
    //   targetId: "mosaico-ad__wallpaper",
    //   isLazy: true
    // },
  ]
}

window.googletag.cmd.push(function () {

  // googletag.pubads().setTargeting("Test", "mosaico_test");
  googletag.pubads().setTargeting("Newspaper", process.env.PAPER || "hbl");

  /* Ad slots to use */
  const networkCode = "/21664538223/";

  /* define gam slots */
  const slots = window.innerWidth < 1020 ? window.adSlots.mobile : window.adSlots.desktop;
  slots.map(
    slot => {
      googletag.defineSlot(
        networkCode + slot.gamId,
        slot.sizes,
        slot.targetId
      ).addService(googletag.pubads());
    }
  );
  window.definedSlots = googletag.pubads().getSlots().map(s => s.getSlotElementId())
  googletag.pubads().collapseEmptyDivs();
  googletag.enableServices();
  googletag.pubads().addEventListener('slotRenderEnded', event => {
    if (!event.isEmpty) {
      let elementId = event.slot.getSlotElementId();
      document.querySelector("#" + elementId).classList.add("populated");
    };
  });
  window.addEventListener("message", event => {
    let message = event.data
    if ( ["BIGMAX", "WALLPAPER"].indexOf(message.cmd) != -1 ) {
      switch (message.cmd) {
        case "BIGMAX":
          var cu = document.getElementById("mosaico-ad__top-parade");
          cu.classList.add("BIGMAX");
          cu.innerHTML = `
            <div>
              <a target="_blank" href="${message.link}">
                <img src="${message.img}">
              </a>
              <div onclick="closeAdDiv('.mosaico-ad.mosaico-ad__top-parade')" class="ad-close">Stäng</div>
            </div>`;
      }
    }
  })
});
window.closeAdDiv = function (adDiv) {
  document.querySelector(adDiv).innerHTML = "";
}

main();
