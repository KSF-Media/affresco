window.dataLayer = window.dataLayer || [];

function Tracker() {
  this.dataLayer = window.dataLayer;
}

Tracker.prototype.pushPageLoad = function() {
  var pushData = {'event': 'page_data'};
  // TODO: Add user UUID to the request
  this.dataLayer.push(pushData);
}

exports.tracker_ = new Tracker();
