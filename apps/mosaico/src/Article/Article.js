exports.evalExternalScriptsImpl = function (scripts) {
  if (typeof document !== "undefined") {
    scripts.forEach((script) => {
      var dummy = document.createElement("div");
      dummy.innerHTML = script.trim();
      try {
	eval(dummy.firstChild.innerHTML);
      } catch (err) {
	console.warn("Failed to eval script:", err);
      }
    });
  }
};
