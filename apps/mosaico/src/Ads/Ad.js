exports.fetchAdImpl = function (contentUnit) {
  window.googletag.cmd.push(function () {
    if (window.definedSlots.includes(contentUnit)) {
      window.googletag.display(contentUnit);
    }
  });
};
