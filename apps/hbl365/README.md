# HBL365 purchase path

This site contains the purchase path for the HBL365 app (https://365.ksfmedia.fi/)

## Running locally

```bash
yarn install
yarn build
yarn start
```

A local dev server can now be found @ http://localhost:8004/

## Running tests

```bash
yarn test
```

## Deployment

When a PR is merged to **master** it gets **deployed** to **production**

Deployment is handled by github actions, more details in the CI [README](../../ci/README.md)
