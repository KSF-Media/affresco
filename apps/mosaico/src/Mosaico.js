exports.refreshAdsImpl = function (slotArray) {
  if (typeof window.googletag.pubads === "function") {
    window.googletag
      .pubads()
      .getSlots()
      .map((s) => {
	if (slotArray.includes(s.getSlotElementId())) {
	  window.googletag.pubads().refresh([s]);
	  // console.log("Refreshing ad: " + s.getSlotElementId());
	}
      });
  }
};
exports.sentryDsn_ = function () {
  return process.env.SENTRY_DSN;
};
