# Mitt Konto

https://konto.ksfmedia.fi/

## End-to-end tests

Run:

Running tests doesn't start the app so do a `yarn start` as well.

```bash
$ yarn install
$ export PERSONA_URL=https://persona.staging.ksfmedia.fi/v1
$ spago -x test.dhall test
```
