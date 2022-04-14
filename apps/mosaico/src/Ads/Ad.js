exports.fetchAdImpl = function (contentUnit) {
  try {
    window.googletag.cmd.push(function () {
      if (window.definedSlots.includes(contentUnit)) {
	window.googletag
	  .pubads()
	  .getSlots()
	  .map((s) => {
	    if (s.getSlotElementId() === contentUnit) {
	      window.googletag.pubads().refresh([s]);
	      // console.log("Refreshing ad: " + s.getSlotElementId());
	    } else {
	      window.googletag.display(contentUnit);
	    }
	  });
      }
    });
  } catch (err) {}
};

exports.getGamId = function (contentUnit) {
  try {
    const slots = window.innerWidth < 1020 ? window.adSlots.mobile : window.adSlots.desktop;
    const slot = slots.find((element) => contentUnit === element.targetId);
    if (typeof slot === "undefined") {
      return null;
    } else {
      return slot.gamId;
    }
  } catch (err) {
    return null;
  }
};

exports.getIsLazy = function (contentUnit) {
  try {
    const slots = window.innerWidth < 1020 ? window.adSlots.mobile : window.adSlots.desktop;
    const slot = slots.find((element) => contentUnit === element.targetId);
    if (typeof slot === "undefined") {
      return null;
    } else {
      return slot.isLazy;
    }
  } catch (err) {
    return null;
  }
};
