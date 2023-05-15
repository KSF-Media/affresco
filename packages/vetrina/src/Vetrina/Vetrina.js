export function sentryDsn_() {
  return process.env.SENTRY_DSN;
}

export function scrollToVetrina() {
  const vetrina = document.querySelector(".vetrina--container");
  if (vetrina)
    // Missing from purescript-web-html
    vetrina.scrollIntoView({ block: "start", inline: "nearest", behavior: "smooth" });
}

export function windowClose(w) {
  return function () {
    try {
      w.close();
    } catch (e) {
      /* ignore errors from w being null */
    }
  };
}
