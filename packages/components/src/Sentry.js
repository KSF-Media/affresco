exports.initSentry_ = function(sentryDsn) {
    var Sentry = require('@sentry/browser');
    Sentry.init({ dsn: sentryDsn });
    window.sentry = Sentry;
    return Sentry;
};
exports.captureMessage_ = function(sentry, message, level) {
    return sentry.captureMessage(message, level);
};
exports.captureException_ = function(sentry, err) {
    return sentry.captureException(err);
};
exports.setTag_ = function(sentry, key, value) {
    return sentry.setTag(key, value);
};
exports.setUser_ = function(sentry, cusno) {
    return sentry.setUser({id: cusno});
};
