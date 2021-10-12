exports.setInnerHTMLImpl = function (elemId, html) {
  const e = document.getElementById(elemId);
  if (e) {
    e.innerHTML = html;
  }
  console.log("SET INNER HTML");
};
