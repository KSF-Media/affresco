exports.refreshAdsImpl = function (str) {
  if (typeof window.googletag.pubads === "function") {
    window.googletag.pubads().refresh();
  }
}