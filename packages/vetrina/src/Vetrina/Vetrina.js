export function sentryDsn_() {
  return process.env.SENTRY_DSN;
}

export function scrollToVetrina() {
  const header = document.querySelector("header.header-container-container");
  const vetrina = document.querySelector(".vetrina--container");
  if (vetrina) {
    const headerOffset = header ? header.clientHeight : 0;
    const vetrinaPosition = vetrina.getBoundingClientRect().top - document.body.getBoundingClientRect().top;
    window.scrollTo({
         top: vetrinaPosition - headerOffset,
         behavior: "smooth"
    });
  }
}
