const Router = require("react-router-dom");

exports.images = {
  subscribe: require('../../../../images/offer-cta.png')
};
exports.sentryDsn_ = function() {
  return process.env.SENTRY_DSN;
};
exports.router_ = function() {
  return Router.BrowserRouter.default;
};
