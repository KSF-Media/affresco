export function refreshAdsImpl(slotArray) {
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
export function sentryDsn_() {
  return process.env.SENTRY_DSN;
};

export function setManualScrollRestoration() {
  history.scrollRestoration = 'manual';
}
