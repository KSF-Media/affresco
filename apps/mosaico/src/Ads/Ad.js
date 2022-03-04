exports.fetchAdImpl = function (contentUnit) {
  window.googletag.cmd.push(function () {
    window.googletag.display(contentUnit);
  });
};
