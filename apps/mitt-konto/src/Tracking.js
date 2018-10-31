window.dataLayer = window.dataLayer || [];

function Tracker() {
  this.dataLayer = window.dataLayer;
}

exports.newTracker = function() {
  return new Tracker();
}

exports.pushPageLoad = function(tracker) {
  return function() {
    tracker.dataLayer.push({'event': 'page_data'});
  }
}
