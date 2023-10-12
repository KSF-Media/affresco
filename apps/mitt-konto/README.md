# Mitt Konto

https://konto.hbl.fi/
https://konto.ksfmedia.fi/

## Dev setup

```bash
yarn install
yarn build
yarn start
```

Dev site now available @ http://localhost:8001

## End-to-end tests

```bash
yarn install
export PERSONA_URL=https://persona.staging.ksfmedia.fi/v1
export BOTTEGA_URL=https://bottega.staging.ksfmedia.fi/v1
spago build
yarn run local-tests
```

## Deploying 

When a PR is merged to __master__ it gets __deployed__ to __production__

Deployment is handled by github actions, more details in the CI [README](../../ci/README.md)