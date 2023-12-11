export function windowClose(w) {
  return function () {
    try {
      w.close();
    } catch (e) {}
  };
}

export function getLocation() {
  return function () {
    return document.location.href;
  }
}