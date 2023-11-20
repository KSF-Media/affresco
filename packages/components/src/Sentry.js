/*
   Note for debugging/testing! I spent a lot of time wondering why events were not sent.
   From Sentry docs:

   "Errors triggered from within Browser DevTools are sandboxed, so will not trigger an error handler.
   Place the snippet directly in your code instead."

   https://docs.sentry.io/platforms/javascript/
 */
export function initSentry_(sentryDsn, tSampleRate) {
  var Sentry = require("@sentry/browser");
  var Tracing = require("@sentry/tracing");
  if (sentryDsn && sentryDsn.length > 1) {
    Sentry.init({
      dsn: sentryDsn,
      integrations: [new Tracing.BrowserTracing()],
      tracesSampleRate: tSampleRate,
      // Only allow errors from code hosted on our own domains
      allowUrls:
        [
          /https?:\/\/([a-z\-]+\.)?([a-z\-]+\.)?(hbl|ksfmedia|ostnyland|vastranyland)\.fi/i,
        ],
      beforeSend(event, hint) {
        return filterBeforeSend(event, hint);
      },
    });
    return Sentry;
  } else {
    console.warn("Could not setup Sentry, dsn is faulty. Look into your env variables.");
    return null;
  }
}

// function for filtering out "unfixable" errors
function filterBeforeSend(event, hint) {
  const error = hint.originalException;
  // for some reason iOS devices want to access the nonexistent telephone field of our journalists
  if (
    error &&
    error.message &&
    error.message.match(/null is not an object (evaluating 'Object.prototype.hasOwnProperty.call(o,"telephone")'))/i)
  ) {
    return null;
  }
  return event;
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
