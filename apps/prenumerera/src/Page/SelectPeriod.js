export function clearOpener(w) {
  return function () {
    w.opener = null;
  };
}
