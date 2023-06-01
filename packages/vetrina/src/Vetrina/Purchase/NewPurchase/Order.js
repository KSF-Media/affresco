export function windowClose(w) {
  return function () {
    try {
      w.close();
    } catch (e) {}
  };
}
