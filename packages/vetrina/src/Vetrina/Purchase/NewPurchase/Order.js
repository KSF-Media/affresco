export function windowClose(w) {
  return function () {
    try {
      w.close();
    } catch (e) {}
  };
}

export function getLocation() {
  return function () {
    console.log("in getLocation")
    console.log(document.location.href)
    return document.location.href;
  }
}