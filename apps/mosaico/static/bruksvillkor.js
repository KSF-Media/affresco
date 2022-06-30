import { addHandler } from "./common";
const showOnPx = 100;
const backToTopButton = document.querySelector(".static-page__back-to-top");

const scrollContainer = () => {
  return document.documentElement || document.body;
};

addHandler(
  document,
  () => {
    if (scrollContainer().scrollTop > showOnPx) {
      backToTopButton.style.opacity = "100%";
    } else {
      backToTopButton.style.opacity = "0%";
    }
  },
  "scroll"
);

const goToTop = () => {
  document.body.scrollIntoView({
    behavior: "smooth",
  });
};

addHandler(backToTopButton, goToTop);

const links = document.querySelectorAll(".static-page__list-link");

for (const link of links) {
  addHandler(link, clickHandler);
}

function clickHandler(e) {
  const href = this.getAttribute("href");
  if (!href.startsWith("#")) {
    /* bubble event up, ie. open external links */
    return;
  }
  e.preventDefault();
  history.pushState(undefined, "", href);
  const offsetTop = document.querySelector(href).offsetTop;

  scroll({
    top: offsetTop,
    behavior: "smooth",
  });
}
