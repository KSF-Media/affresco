"use strict";

// Eww, global state
var saveTokenInCookies = false;

exports.saveTokenInCookies = function() { return saveTokenInCookies; }

exports.enableCookieLogin = function() { saveTokenInCookies = true; }
