exports.fetchAd = function (contentUnit) {
  window.googletag.cmd.push(function () {
    window.mosaicoAdSlots.map(s => {
      if(s.getSlotElementId() === contentUnit) {
        window.googletag.pubads().refresh([s]);
      } else {
        window.googletag.display(contentUnit);
      }
    });
  });
};
