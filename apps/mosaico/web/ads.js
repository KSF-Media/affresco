window.googletag = window.googletag || { cmd: [] };

window.googletag.cmd.push(function () {

  googletag.pubads().setTargeting("newspaper", "hbl");

  /* Ad slots to use */
  const networkCode = "/21664538223/";
  const adSlots = {
    mobile: [
      {
        gamId: "MOBPARAD",
        sizes: [ [300,100], [300,250], [300,300], [300,431], [300,600] ],
        targetId: "mosaico-ad__top-parade"
      },
      {
        gamId: "MOBMITT",
        sizes: [ [300,100], [300,250], [300,300], [300,431], [300,600] ],
        targetId: "mosaico-ad__bigbox1"
      },
      {
        gamId: "MOBNER",
        sizes: [ [300,100], [300,250], [300,300], [300,431], [300,600] ],
        targetId: "mosaico-ad__bigbox2"
      },
      {
        gamId: "MOBBOX1",
        sizes: [ [300,100], [300,250], [300,300], [300,431], [300,600] ],
        targetId: "mosaico-ad__box1"
      },
      {
        gamId: "MOBBOX2",
        sizes: [ [300,100], [300,250], [300,300], [300,431], [300,600] ],
        targetId: "mosaico-ad__box2"
      },
      {
        gamId: "MOBBOX3",
        sizes: [ [300,100], [300,250], [300,300], [300,431], [300,600] ],
        targetId: "mosaico-ad__box3"
      },
      {
        gamId: "MOBBOX4",
        sizes: [ [300,100], [300,250], [300,300], [300,431], [300,600] ],
        targetId: "mosaico-ad__box4"
      },
      {
        gamId: "MOBBOX5",
        sizes: [ [300,100], [300,250], [300,300], [300,431], [300,600] ],
        targetId: "mosaico-ad__box5"
      },
      {
        gamId: "DIGIHELMOB",
        sizes: [300,431],
        targetId: "mosaico-ad__digihelmob"
      },
      {
        gamId: "INTERMOB",
        sizes: [300,500],
        targetId: "mosaico-ad__firstbox"
      },
    ],
    desktop: [
      {
        gamId: "JATTEBOX",
        sizes: [468,400],
        targetId: "mosaico-ad__bigbox1"
      },
      {
        gamId: "DUBBELBOX",
        sizes: [468,600],
        targetId: "mosaico-ad__bigbox2"
      },
      {
        gamId: "DIGIHELMOB",
        sizes: [300,431],
        targetId: "mosaico-ad__digihelmob"
      },
      {
        gamId: "PARAD",
        sizes: [ [980, 120], [980,400], [980,552] ],
        targetId: "mosaico-ad__parade"
      },
      {
        gamId: "MAXPARAD",
        sizes: [ [980, 120], [980,400], [980,480], [980,552], [1920,1080] ],
        targetId: "mosaico-ad__top-parade"
      },
      // {
      //   gamId: "STORTAVLA",
      //   sizes: [160,600],
      //   targetId: "mosaico-ad__tallbox"
      // },
      {
        gamId: "FIRSTBOX",
        sizes: [ [300, 250], [300, 300], [300, 431], [300,600] ],
        targetId: "mosaico-ad__firstbox"
      },
      {
        gamId: "BOX1",
        sizes: [ [300, 250], [300, 300], [300, 431], [300,600] ],
        targetId: "mosaico-ad__box1"
      },
      {
        gamId: "BOX2",
        sizes: [ [300, 250], [300, 300], [300, 431], [300,600] ],
        targetId: "mosaico-ad__box2"
      },
      {
        gamId: "BOX3",
        sizes: [ [300, 250], [300, 300], [300, 431], [300,600] ],
        targetId: "mosaico-ad__box3"
      },
      {
        gamId: "BOX4",
        sizes: [ [300, 250], [300, 300], [300, 431], [300,600] ],
        targetId: "mosaico-ad__box4"
      },
      {
        gamId: "BOX5",
        sizes: [ [300, 250], [300, 300], [300, 431], [300,600] ],
        targetId: "mosaico-ad__box5"
      },
      {
        gamId: "BOX",
        sizes: [ [300,250], [300,300], [300, 431], [300,600] ],
        targetId: "mosaico-ad__box"
      },
      // {
      //   gamId: "BOXFORPRINT",
      //   sizes: [300,300],
      //   targetId: "mosaico-ad__printbox"
      // },
      // {
      //   gamId: "WALLPAPER",
      //   sizes: [ [1600,1200], [1920,1080] ],
      //   targetId: "mosaico-ad__wallpaper"
      // },
    ]
  }

  /* define gam slots */
  const slots = window.innerWidth < 1020 ? adSlots.mobile : adSlots.desktop;
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
});
