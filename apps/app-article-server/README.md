# App article - the server

This is a server-side rendered version of `app-article`.

It basically has only one endpoint, the `/article/<uuid>`.

Authentication is done via `AuthUser` and `Authorization` headers.

## Running the thing locally

Simply execute

```
export PUBLIC_URL=/dist
yarn install
yarn build
yarn start
```

As an alternative to exporting PUBLIC_URL for every new shell you can also use [direnv](https://direnv.net/docs/installation.html) to have it done automgically.

A server in port `8080` should be now up. Here's an example article for you to test it `http://localhost:8080/article/44f345b5-eb92-4e60-a884-26396cd3d831`.
