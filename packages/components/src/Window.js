export function close(w) {
  return function () {
    try {
      w.close();
    } catch (e) {}
  };
}

export function clearOpener(w) {
  return function () {
    try {
      w.opener = null;
    } catch (e) {}
  };
}
