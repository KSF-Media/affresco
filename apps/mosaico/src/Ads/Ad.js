exports.fetchAd = function (contentUnit) {
  window.googletag.cmd.push(function () {
    window.mosaicoAdSlots.map(s => {
      // console.log(s.getSlotElementId());
      if(s.getSlotElementId() === contentUnit) {
        console.log("Refreshing ", contentUnit);
        window.googletag.pubads().refresh([s]);
      } else {
        window.googletag.display(contentUnit);
      }
    });
  });
};
