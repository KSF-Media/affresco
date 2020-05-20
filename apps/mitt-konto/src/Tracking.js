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

exports.reclamationEvent_ = function(tracker, subsno, date, action) {
  return function() {
      tracker.dataLayer.push({'event': 'reclamationEvent', 'subsno': subsno, 'date': date, 'action': action});
  }
}

exports.tempAdressChange_ = function(tracker, subsno) {
  return function() {
      tracker.dataLayer.push({'event': 'tempAdressChangeEvent', 'subsno': subsno});
  }
}

exports.pauseSubscription_ = function(tracker, subsno) {
  return function() {
      tracker.dataLayer.push({'event': 'pauseSubscriptionEvent', 'subsno': subsno});
  }
}
