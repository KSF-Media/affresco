# Mitt Konto

https://konto.ksfmedia.fi/

## End-to-end tests

Run:

Running tests doesn't start the app so do a `npm run start` as well.

```bash
$ npm install
$ export PERSONA_URL=https://persona.staging.ksfmedia.fi/v1
$ export BOTTEGA_URL=https://bottega.staging.ksfmedia.fi/v1
$ spago -x test.dhall test
```
