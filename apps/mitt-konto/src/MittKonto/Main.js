exports.sentryDsn_ = function () {
  return process.env.SENTRY_DSN;
};

exports.getElem = function(sel) {
    return function() {
	const el = document.querySelector(sel);
	console.log("getElem", sel, el);
	return el;
    }
};

exports.getElem_ = exports.getElem;

exports.click = function(el) {
    return function() {
	console.log("click", el);
	el.click();
    }
};
