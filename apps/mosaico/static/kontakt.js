import { createWidget } from '@typeform/embed'
import '@typeform/embed/build/css/widget.css'

createWidget('ayblJO4k', { container: document.querySelector('#kontakt'), height: 500, iframeProps: { style:"width: 100%; height: 100%; outline: none; border: none;"} })

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
