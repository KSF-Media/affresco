"use strict";

/*
It would be very tempting to write these as:
  exports.setItem_ =  window.localStorage.setItem;

However, it creates weird errors and does not work as expected.

JavaScript, not even once.
*/

export function setItem_(k, v) {
  window.localStorage.setItem(k, v);
  return {};
};

export function getItem_(k) {
  return window.localStorage.getItem(k);
};

export function removeItem_(k) {
  window.localStorage.removeItem(k);
  return {};
};
