exports.evalExternalScriptsImpl = function (scripts) {
  if (typeof document !== "undefined") {
    scripts.forEach((script) => {
      var dummy = document.createElement("div");
      dummy.innerHTML = script.trim();
      evalScript(dummy.firstChild.innerHTML);
      const scriptSrc = dummy.firstChild.getAttribute("src");
      if (scriptSrc) {
	fetch(scriptSrc)
	  .then((r) => r.text())
	  .then(evalScript);
      }
    });
  }
};

function evalScript(s) {
  try {
    eval(s);
  } catch (err) {
    console.warn("Failed to eval script:", err);
  }
}
