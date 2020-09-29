# Vetrina-test

This is a test deployment for Vetrina (the fast buying path), for:
- convenience of development (since the "production" one is embedded in the main sites for now)
- running end-to-end tests

## End-to-end tests

Run:

```bash
$ yarn install
$ export PERSONA_URL=https://persona.staging.ksfmedia.fi/v1
$ spago test
```

Puppeteer will be launched in `headless` mode by default (because CI doesn't have a
display), but for easier debugging you might want to see the Chrome instance doing things.
To achieve that, you'll want to set `headless: false` in `Puppeteer.launch`.