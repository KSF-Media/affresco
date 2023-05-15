import apple from "../../../../images/hbl365/apple.png";
import android from "../../../../images/hbl365/android.png";
import logo from "../../../../images/hbl365/app-icon-1024x1024-pixels.png";

export const appStore = { apple, android };

export { logo };

export function addOnScroll() {
  const header = document.getElementsByTagName("header")[0];
  window.onscroll = function () {
    if (window.scrollY > 38) {
      header.classList.add("tight");
    } else {
      header.classList.remove("tight");
    }
  };
}
