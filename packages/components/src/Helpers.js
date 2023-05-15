// Get the current TZ offset used in Europe/Helsinki.
export function getCurrentTZOffset_() {
  // According to MDN, most browsers got support for this API by
  // 2015. Let's just assume the feature.
  const options = { timeStyle: "long", timeZone: "Europe/Helsinki" };
  const tz = new Intl.DateTimeFormat("en-GB", options).format(new Date()).substr(9);
  if (tz == "EEST") {
    return -3;
  } else if (tz == "EST") {
    return -2;
  } else if (tz == "CST") {
    // Just in case DST is removed and that's what we switch to (eww).
    return -1;
  } else {
    // Shouldn't happen
    return new Date().getTimezoneOffset() / 60;
  }
}
