import * as Sentry from "@sentry/browser";
import { Integrations } from "@sentry/tracing";

Sentry.init({
    dsn: process.env.SENTRY_DSN || '',
    integrations: [new Integrations.BrowserTracing()],
});

Sentry.setTag("appName", "app-article");

exports.Sentry = Sentry;