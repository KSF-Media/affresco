const questions = document.getElementsByClassName("faq__question");

for (const q of questions) {
  q.addEventListener("click", function () {
    for (const q of questions) {
      if (q === this) {
        if (q.classList.contains("faq__question--active")) {
          q.classList.remove("faq__question--active");
          q.nextElementSibling.style.display = "none";
        } else {
          q.classList.add("faq__question--active");
          q.nextElementSibling.style.display = "block";
        }
      } else {
        q.classList.remove("faq__question--active");
        q.nextElementSibling.style.display = "none";
      }
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
