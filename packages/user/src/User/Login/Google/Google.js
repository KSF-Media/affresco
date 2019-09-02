var googleClientId = process.env.GOOGLE_CLIENT_ID;

exports.loadGapi_ = function(args) {
  gapi.load('auth2', function() {
    var auth2 = gapi.auth2.init({
      client_id: googleClientId + ".apps.googleusercontent.com",
      cookiepolicy: 'single_host_origin'
    }).then(initSuccess(args), initError(args))
  });
  return {};
};

function initSuccess(args) {
  return function() {
    var GoogleAuth = gapi.auth2.getAuthInstance();
    GoogleAuth.isSignedIn.listen(setUser(args.onSuccess));
    if (GoogleAuth.isSignedIn.get()) {
      var user = GoogleAuth.currentUser.get();
      args.onSuccess(user);
    }
    else {
      GoogleAuth.signIn();
    }
  }
}

function initError(args) {
  return function(err) {
    args.onFailure(err);
  }
}

function setUser(onSuccess) {
  return function(isSignedIn) {
    if (isSignedIn) {
      var GoogleAuth = gapi.auth2.getAuthInstance();
      var user = GoogleAuth.currentUser.get();
      onSuccess(user);
    }
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
