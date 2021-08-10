# App article - the server

This is a server-side rendered version of `app-article`.

It basically has only one endpoint, the `/article/<uuid>`.

Authentication is done via `AuthUser` and `Authorization` headers.

## Running the thing locally

Simply execute

```
yarn install
yarn build
yarn start
```

A server in port `8080` should be now up. Here's an example article for you to test it `http://localhost:8080/article/44f345b5-eb92-4e60-a884-26396cd3d831`.
