"use strict";

exports._appId = process.env.FACEBOOK_APP_ID;

exports._loginStatus = function(fb) {
  return function(callback) {
    return function() { // returns an effect
      fb.getLoginStatus(function(response) {
        callback(response)(); // callback itself returns an effect
      });
    }
  }
}

exports._login = function(opts) {
  return function(fb) {
    return function(callback) {
      return function() { // returns an effect
        fb.login(function (response) {
          callback(response)();  // callback itself returns an effect
        }, opts);
      }
    }
  }
}

exports._logout = function(fb) {
  return function(callback) {
    return function() { // returns an effect
      fb.logout(function (response) {
        callback(response)();  // callback itself returns an effect
      });
    }
  }
}

exports._api = function(fb, path, method, params, callback) {
  return function() { // returns an effect
    fb.api(path, method, params, function (response) {
      callback(response)();  // callback itself returns an effect
    });
  }
}

exports._init = function(callback) {
  return function(fbConfig) {
    return function() { // returns an effect
      if (window.isFBLoaded || typeof FB === 'object') {
        callback(FB)();
      } else {
        window.fbAsyncInit = function() {
          FB.init(fbConfig);
          FB.AppEvents.logPageView();
          window.isFBLoaded = true;
          callback(FB)(); // callback itself returns an effect
        };
        (function(d, s, id){
          var js, fjs = d.getElementsByTagName(s)[0];
          if (d.getElementById(id)) {return;}
          js = d.createElement(s);
          js.id = id;
          var debug = fbConfig['debug'] ? "/debug" : "";
          js.src = "//connect.facebook.net/" + fbConfig['locale'] + "/sdk" + debug + ".js";
          fjs.parentNode.insertBefore(js, fjs);
        }(window.document, 'script', 'facebook-jssdk'));
      };
    }
  }
}
