"use strict";

/* This simplifies cookie handling and doesn't do things like allow
 * for cookies with the same names but different paths.
 */

// For development environments
const insecure = !!process.env.INSECURE_COOKIE;

export function setCookie_(k, v) {
  document.cookie = k + "=" + v + ";SameSite=Strict;Path=/" + (insecure ? "" : ";Secure");
  return {};
}

export function getValue_(k) {
  const cookie = document.cookie.split('; ').find(function(row) {return row.startsWith(k+'=')});
  return cookie ? cookie.split('=')[1] : null;
}

export function deleteCookie_(k) {
  document.cookie = k + "=;Max-age=0;Path=/";
  return {};
}
