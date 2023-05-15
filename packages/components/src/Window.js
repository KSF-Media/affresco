export function close(w) {
  return function () {
    try {
      w.close();
    } catch (e) {
      /* ignore errors from w being null */
    }
  };
}

export function clearOpener(w) {
  return function () {
    try {
      w.opener = null;
    } catch (e) {
      /* ignore errors from w being null */
    }
  };
}
