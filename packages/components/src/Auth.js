"use strict";

// Eww, global state
var saveTokenInCookies_ = false;

export function saveTokenInCookies() {
    return saveTokenInCookies_;
}

export function enableCookieLogin() {
    saveTokenInCookies_ = true;
}
