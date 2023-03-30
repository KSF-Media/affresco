/*
   Note for debugging/testing! I spent a lot of time wondering why events were not sent.
   From Sentry docs:

   "Errors triggered from within Browser DevTools are sandboxed, so will not trigger an error handler.
   Place the snippet directly in your code instead."

   https://docs.sentry.io/platforms/javascript/
 */
export function initSentry_(sentryDsn) {
  var Sentry = require("@sentry/browser");
  var Tracing = require("@sentry/tracing");
  if (sentryDsn && sentryDsn.length > 1) {
    Sentry.init({
      dsn: sentryDsn,
      integrations: [new Tracing.BrowserTracing()],
      tracesSampleRate: 0.8,
      beforeSend(event, hint) {
        const error = hint.originalException;
        if (error && error.message && error.message.match(/_AutofillCallbackHandler/i)) {
          return null;
        }
        return event;
      },
    });
    return Sentry;
  } else {
    console.warn("Could not setup Sentry, dsn is faulty. Look into your env variables.");
    return null;
  }
}

export function captureMessage_(sentry, appName, message, level) {
  return sendSentryEvent(sentry, appName, "captureMessage", message, level);
}
export function captureException_(sentry, appName, err) {
  return sendSentryEvent(sentry, appName, "captureException", err);
}
export function setTag_(sentry, key, value) {
  return sendSentryEvent(sentry, null, "setTag", key, value);
}
export function setUser_(sentry, cusno) {
  if (sentry) {
    // Empty object is used for istance when user logs out from the system
    sentry.setUser(cusno === null ? {} : { id: cusno });
  }
}

function sendSentryEvent(sentry, appName, fnName, ...args) {
  if (sentry && typeof sentry[fnName] === "function") {
    return sentry.withScope(function (scope) {
      // Let's add app name tag for this call only, if `appName` is defined
      if (typeof appName === "string") {
        scope.setTag("appName", appName);
      }
      sentry[fnName](...args);
    });
  } else {
    console.log("Tried to send something to Sentry, but not initialized.", fnName, args);
  }
}
