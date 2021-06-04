exports.fetchAd = function (contentUnit) {
  // console.log("Fetching ad ", contentUnit);
  window.googletag.cmd.push(function () {
    window.googletag.display(contentUnit);
  });
};
