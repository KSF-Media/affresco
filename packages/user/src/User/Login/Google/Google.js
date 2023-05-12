var googleClientId = process.env.GOOGLE_CLIENT_ID;

export function loadGapi_(args) {
  if (window.gapi === undefined) {
    var script = document.createElement("script");
    script.onload = function () {
      loadGapi(args);
    };
    script.src = "https://apis.google.com/js/platform.js";
    document.head.appendChild(script);
  } else {
    loadGapi(args);
  }
}

function loadGapi(args) {
  gapi.load("auth2", function () {
    var auth2 = gapi.auth2
      .init({
        client_id: googleClientId + ".apps.googleusercontent.com",
        cookiepolicy: "single_host_origin",
      })
      .then(initSuccess(args), initError(args));
  });
  return {};
}

function initSuccess(args) {
  return function () {
    var GoogleAuth = gapi.auth2.getAuthInstance();
    GoogleAuth.isSignedIn.listen(setUser(args.onSuccess));
    if (GoogleAuth.isSignedIn.get()) {
      var user = GoogleAuth.currentUser.get();
      args.onSuccess(mkAuthResponse(user));
    } else {
      GoogleAuth.signIn();
    }
  };
}

function initError(args) {
  return function (err) {
    args.onFailure(err);
  };
}

function setUser(onSuccess) {
  return function (isSignedIn) {
    if (isSignedIn) {
      var GoogleAuth = gapi.auth2.getAuthInstance();
      var user = GoogleAuth.currentUser.get();
      onSuccess(mkAuthResponse(user));
    }
  };
}

function mkAuthResponse(user) {
  var userProfile = user.getBasicProfile(),
    userAuth = user.getAuthResponse(true), // When `true`, access_token is included
    userEmail = userProfile.getEmail(),
    userAccessToken = userAuth.access_token;
  return { accessToken: userAccessToken, email: userEmail };
}

export function isSignedIn_() {
  if (typeof gapi !== "undefined" && gapi["auth2"]) {
    var auth2 = gapi.auth2.getAuthInstance();
    return auth2.isSignedIn.get();
  }
  return false;
}

export function signOut_() {
  var auth2 = gapi.auth2.getAuthInstance();
  return auth2.signOut();
}
