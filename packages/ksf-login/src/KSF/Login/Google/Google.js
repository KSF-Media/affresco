var googleClientId = process.env.GOOGLE_CLIENT_ID;
exports.load_ = function() {
  return gapi.auth2.init({
    client_id: googleClientId + ".apps.googleusercontent.com",
    cookiepolicy: 'single_host_origin'
  })
};

exports.attachClickHandler_ = function(args) {
  gapi.load('auth2', function() {
    var auth2 = gapi.auth2.init({
      client_id: googleClientId + ".apps.googleusercontent.com",
      cookiepolicy: 'single_host_origin'
    }).then(initSuccess(args), initError(args))
  });
  return {};
};

function initSuccess(args) {
  return function(auth2) {
    auth2.attachClickHandler(args.node, args.options, args.onSuccess, args.onFailure);
  }
}

function initError(args) {
  return function(err) {
    args.onFailure(err);
  }
}

exports.isSignedIn_ = function() {
  if (gapi['auth2']) {
    var auth2 = gapi.auth2.getAuthInstance();
    return auth2.isSignedIn.get();
  }
  return false;
};

exports.signOut_ = function() {
  var auth2 = gapi.auth2.getAuthInstance();
  return auth2.signOut();
}
