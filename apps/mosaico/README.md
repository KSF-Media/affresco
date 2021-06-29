# Mosaico

The new and shiny sites!

It's a server side rendered react thing. The idea is that the first pageload will be returned from the server and after that it acts like a single page app.

To run things:
```
yarn install
yarn build
yarn start
```

## Development

For now, the server will not in fact render any react components. Instead, it returns the same HTML the former SPA version of Mosaico did.
For this to work, we need to pre-build the client HTML/JS before running the server and also every time a `purs` file is changed (this is the `yarn build` part).
So you will be restarting the server and building a lot. There might be smoother ways of doing this.
