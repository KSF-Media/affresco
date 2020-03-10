exports.initSentry_ = function(sentryDsn) {
    var Sentry = require('@sentry/browser')
    if (sentryDsn && sentryDsn.length > 1) {
        Sentry.init({ dsn: sentryDsn });
        return Sentry;
    }
    else {
        console.warn("Could not setup Sentry, dsn is faulty. Look into your env variables.");
        return null;
    }
};

exports.captureMessage_ = function(sentry, message, level) {
    return sendSentryEvent(sentry, 'captureMessage', message, level);
};
exports.captureException_ = function(sentry, err) {
    return sendSentryEvent(sentry, 'captureException', err);
};
exports.setTag_ = function(sentry, key, value) {
    return sendSentryEvent(sentry, 'setTag', key, value);
};
exports.setUser_ = function(sentry, cusno) {
    return sendSentryEvent(sentry, 'setUser', {id: cusno});
};

function sendSentryEvent(sentry, fnName, ...args) {
    if (sentry && typeof sentry[fnName] === 'function') {
        return sentry[fnName](...args);
    }
    else {
      console.log('Tried to send something to Sentry, but not initialized.', fnName, args);
    }
}
