# App article - the server

This is a server-side rendered version of `app-article`.

It basically has only one endpoint, the `/article/<uuid>`.

Authentication is done via `AuthUser` and `Authorization` headers.

## Running the thing locally

```
export PUBLIC_URL=/dist
yarn install
yarn build
yarn start
```

As an alternative to exporting PUBLIC_URL for every new shell you can also use [direnv](https://direnv.net/docs/installation.html) to have it done automagically.

A server in port `8003` should now be running. Here's an example article for you to test it `http://localhost:8003/article/e63fee78-2630-4195-bdee-2dcb0cd8c3b2`.

To best simulate behaviour in app use query parameters `paper`, `fontSize` and `mode`. `paper` takes values `hbl, on and vn`. `fontSize` takes `1.06, 1.5, 2.0, 2.5 or 3.0` (default being 1.06). To test dark mode set `mode=dark`.

## Debugging

It might be helpful to use React development build instead of the production
build to avoid minified error messages (see <https://reactjs.org/docs/error-decoder.html?invariant=152&args[]=f>).
To use development React, simply run

```
export NODE_ENV=development
```

before building the app.

## Tests
TODO

## Deploying

When a PR is merged to __master__ it gets __deployed__ to __production__

Deployment is handled by our gitlab mirror, see [.gitlab-ci.yml](../../.gitlab-ci.yml) and [infra](../../infra/README.md) for details
