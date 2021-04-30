var googletag = googletag || {};
var ksfDfp = {};

(function () {
  "use strict";
  googletag.cmd = googletag.cmd || [];

  ksfDfp.displayBanner = function (id) {
    googletag.cmd.push(function () {
      googletag.display(id);
    });
  };

  ksfDfp.getBannerWidth = function (banner) {
    var bannerWidth;
    // Banners sometimes have several heights, so we need to check how deep an array we are dealing with
    if (!Array.isArray(banner[1][0])) {
      // handle slots with only one height
      bannerWidth = banner[1][0];
    } else {
      // handle slots with more than one height
      bannerWidth = banner[1][0][0];
    }
    return bannerWidth;
  };

  ksfDfp.adsAreShown = true;
  if (window.disableAds) {
    ksfDfp.adsAreShown = false;
    console.log("We will not show ads on this page load!");
  }

  if (ksfDfp.adsAreShown) {
    ksfDfp.account = "/21664538223/";
    ksfDfp.activated = false;
    ksfDfp.mobile = true; // app is all mobile
    ksfDfp.width = Math.max(
      document.documentElement.clientWidth,
      window.innerWidth || 0
    );

    // this var is available in KSF sites
    ksfDfp.site = "app"; // there is only one site in the app
    googletag.pubads().setTargeting("newspaper", ksfDfp.site);
    googletag.pubads().setTargeting("consent", 1);

    ksfDfp.startUp = function () {
      if (!ksfDfp.activated) {
        ksfDfp.activated = true;
        // activate display for all slots
        var slotIds = [];
        var slotWidths = [];
        var slotsAreOkay = [];
        for (var i = 0; i < ksfDfp.numberOfSlots; i++) {
          // Avoid pushing slots that are not to be filled. Due to timing issues with activeSlotsFlatArray I need to do a duplicate check here for width and presence in the desktop-only list. Should be reworked.
          slotIds[i] = ksfDfp.slots[i][0];
          slotWidths[i] = ksfDfp.getBannerWidth(ksfDfp.slots[i]);
          slotsAreOkay[i] = false;
          if (slotWidths[i] <= ksfDfp.width) {
            slotsAreOkay[i] = true;
          }
          if (window.document.getElementById(slotIds[i]) && slotsAreOkay[i]) {
            ksfDfp.displayBanner(slotIds[i]);
          }
        }
      } else {
        console.log("Already activated. Not running again!");
      }
    };

    // The order of sizes should be the same as in dfp:s ad unit definition. This array is used to block publication unless the display size is mobile.
    ksfDfp.mobileOnlySlots = [
      [
        "MOBPARAD",
        [
          [300, 100],
          [300, 250],
          [300, 300],
          [300, 600],
        ],
      ],
      [
        "MOBMITT",
        [
          [300, 100],
          [300, 250],
          [300, 300],
          [300, 600],
        ],
      ],
      [
        "MOBNER",
        [
          [300, 100],
          [300, 250],
          [300, 300],
          [300, 600],
        ],
      ],
      [
        "DIGIHELMOB",
        [
          [300, 100],
          [300, 250],
          [300, 300],
          [300, 431],
          [300, 600],
        ],
      ],
    ];

    ksfDfp.allPageSlots = [];

    // Slots that are not interstitials but need a header with a close button should be listed here. These guys need an enveloping extra div to be closed correctly.
    ksfDfp.closableAdSlots = [];
    // load mobile-only slot if the page load is mobile
    if (ksfDfp.mobile) {
      ksfDfp.allPageSlots = ksfDfp.mobileOnlySlots;
    }

    ksfDfp.slots = [];
    ksfDfp.slots = ksfDfp.allPageSlots;
    ksfDfp.numberOfSlots = ksfDfp.slots.length;
    ksfDfp.slotObjects = [];

    googletag.cmd.push(function () {
      var bannerWidthInstance;
      var bannerSizeList;
      // flat array of slots for quick checkup later
      ksfDfp.activeSlotsFlatArray = [];

      // stuck to decrementing i here, just in case the array created in this loop needs to stay in the same order. (I don't know yet if it does.)
      for (var i = ksfDfp.numberOfSlots - 1; i >= 0; i--) {
        // Check that the banner is smaller than the size of the screen. Also make sure that the slot is allowed on mobile, if the page load is mobile.
        bannerWidthInstance = ksfDfp.getBannerWidth(ksfDfp.slots[i]);

        if (bannerWidthInstance <= ksfDfp.width) {
          ksfDfp.activeSlotsFlatArray.push(ksfDfp.slots[i][0]);
          // Array of banner heights, as some slots have several
          bannerSizeList = ksfDfp.slots[i][1];
          ksfDfp.slotObjects[ksfDfp.slots[i][0]] = googletag
            .defineSlot(
              ksfDfp.account + ksfDfp.slots[i][0],
              bannerSizeList,
              ksfDfp.slots[i][0]
            )
            .addService(googletag.pubads());
        } // slot size to screen comparison end of if
      }

      // Send the users tracking banner consent choice to DFP
      googletag.pubads().setRequestNonPersonalizedAds(1); // Latest 2021-04-30. This is hard coded to no consent for now. This does get asked in the app already, but we are not yet making use of this by allowing personalised ads. 0 means consent, 1 means no tracking ads should be shown.
      googletag.pubads().collapseEmptyDivs();
      googletag.pubads().enableLazyLoad({
        fetchMarginPercent: 0.85, // Fetch slots within 0.5 viewports.
        renderMarginPercent: 0.85, // Render slots within 0.2 viewports.
        mobileScaling: 1.5, // Double the above values on mobile.
      });
      googletag.enableServices();

      googletag.pubads().addEventListener("slotRenderEnded", function (event) {
        if (typeof event !== "undefined" && event.size != null) {
          var contentUnitDiv;
          var headerToShow;
          contentUnitDiv = event.slot.getSlotElementId();

          // show ad headline fitting to the ad format. Out of page slots should have a headline that allows viewers to close the ad
          // only show ad headline for ads that are actually supposed to render
          headerToShow = document.getElementById(contentUnitDiv);

          // show ad header or a special ad header for ads that require a close button
          if (ksfDfp.closableAdSlots.indexOf(contentUnitDiv) === -1) {
            headerToShow.insertAdjacentHTML(
              "afterbegin",
              '<header class="ksfDFPadHeader ' +
                contentUnitDiv +
                '" style="width: ' +
                event.size[0] +
                'px"><span>annons</span></header>'
            );
            headerToShow.insertAdjacentHTML(
              "beforeend",
              '<header class="ksfDFPadHeader ' +
                contentUnitDiv +
                '"><span>annons slut</span></header>'
            );
          } else {
            headerToShow.insertAdjacentHTML(
              "afterbegin",
              '<header class="ksfDFPadHeader  sticky"  id="' +
                contentUnitDiv +
                '_header" ><span>annons</span><p><a href="#" title="stäng annons" onclick="return ksfDfp.closeInterstitial(\'' +
                contentUnitDiv +
                "');\">x</a></p></header>"
            );
          }
        }
      });
    });

    ksfDfp.closeInterstitial = function (interstitialDivName) {
      var divToClose = document.getElementById(interstitialDivName).parentNode;
      document.body.className = document.body.className.replace("noScroll", "");
      divToClose.style.display = "none";
      return false;
    };

    ksfDfp.closeSlotAfterCallback = function (slot) {
      // this is used to close third party ad calls that has been abandoned and returned using a callback
      var divToClose = document.getElementById(slot);
      if (typeof divToClose !== "undefined" && divToClose != null) {
        divToClose.style.display = "none";
        var headerToClose = document.getElementsByClassName(
          "ksfDFPadHeader " + slot
        );
        if (
          typeof headerToClose[0] !== "undefined" &&
          headerToClose[0] != null
        ) {
          var unclosed = headerToClose.length - 1;
          while (unclosed >= 0) {
            headerToClose[unclosed].style.display = "none";
            unclosed = unclosed - 1;
          }
        }
      }
    };

    // handle ads that cannot work in an iframe. Create them based on messages.
    ksfDfp.allowedPostMessages = ["closeSlot"];
    ksfDfp.createBannerOnMessage = function (e) {
      // this will allow only post messages we expect
      if (!(e.data && ksfDfp.allowedPostMessages.indexOf(e.data.cmd) !== -1)) {
        return;
      }
      var message = e.data;
      var bannerIdString = message.id;

      switch (message.cmd) {
        case "closeSlot":
          ksfDfp.closeSlotAfterCallback(bannerIdString);
          break;

        default:
          return;
      }
    };
    // add listener for post message events from ads
    if (window.addEventListener) {
      window.addEventListener("message", ksfDfp.createBannerOnMessage, false);
    } else {
      if (window.attachEvent) {
        window.attachEvent("onmessage", ksfDfp.createBannerOnMessage);
      } else {
        window.onmessage = ksfDfp.createBannerOnMessage;
      }
    }
  }
})();
