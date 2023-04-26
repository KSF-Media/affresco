# Mitt Konto

https://konto.ksfmedia.fi/

## Dev setup

```bash
yarn install
yarn build
yarn start
```

devsite now available @ http://localhost:8001

## End-to-end tests

```bash
yarn install
export PERSONA_URL=https://persona.staging.ksfmedia.fi/v1
export BOTTEGA_URL=https://bottega.staging.ksfmedia.fi/v1
spago build
spago -x test.dhall test
```

## Deploying 

Deployment is handled by github actions, more details in the CI [README](../../ci/README.md)