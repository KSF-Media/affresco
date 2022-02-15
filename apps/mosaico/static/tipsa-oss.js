import { createWidget } from "@typeform/embed";
import "@typeform/embed/build/css/widget.css";

createWidget("Ys7qBL3s", {
  container: document.querySelector("#kontakt"),
  height: 500,
  iframeProps: { style: "width: 100%; height: 100%; outline: none; border: none;" },
});
