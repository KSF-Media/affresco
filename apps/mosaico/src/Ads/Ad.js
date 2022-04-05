exports.fetchAdImpl = function (contentUnit) {
  window.googletag.cmd.push(function () {
    if (window.definedSlots.includes(contentUnit)) {
      window.googletag.pubads().getSlots().map(s => {
        if(s.getSlotElementId() === contentUnit) {
            window.googletag.pubads().refresh([s]);
            // console.log("Refreshing ad: " + s.getSlotElementId());
          } else {
            window.googletag.display(contentUnit);
          }
        }
      );
    }
  });
};
