function init() {
  window.fragorHandlersInitialized = true;
  const questions = document.getElementsByClassName("faq__question");

  for (const q of questions) {
    q.addEventListener("click", function (e) {
      e.stopPropagation();
      e.preventDefault();

      q.classList.toggle("faq__question--active");

      let answer = q.nextElementSibling;
      if (answer.style.display === "block") {
        answer.style.display = "none";
      } else {
        answer.style.display = "block";
      }
    });
  }

  const showOnPx = 100;
  const backToTopButton = document.querySelector(".static-page__back-to-top");

  const scrollContainer = () => {
    return document.documentElement || document.body;
  };

  document.addEventListener("scroll", () => {
    if (scrollContainer().scrollTop > showOnPx) {
      backToTopButton.style.opacity = "100%";
    } else {
      backToTopButton.style.opacity = "0%";
    }
  });

  const goToTop = () => {
    document.body.scrollIntoView({
      behavior: "smooth",
    });
  };

  backToTopButton.addEventListener("click", goToTop);

  const links = document.querySelectorAll(".static-page__list-link");

  for (const link of links) {
    link.addEventListener("click", clickHandler);
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
}
if (!window.fragorHandlersInitialized) {
  init();
}
