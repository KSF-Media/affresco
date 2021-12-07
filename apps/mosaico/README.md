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

We can mentally divide Mosaico into two parts: the server code and the browser code. In production, both parts are required in order to run it as designed. However, as building the server dependent parts and restarting the thing continuously takes time and requires patience, it might be desired to run only the browser part when developing Mosaico. That is, if the development work does not concern the server itself. You can start the development server using Parcel with `yarn start-dev`.

When developing the server side bits, you need to restart the server after any changes with `yarn start` or `spago run`. Note that if your server side code requires also changes to the browser side of things, you need to build the static files with Parcel before running your server. This is what `yarn build` does. This is a bit clumsy and again, a bit time consuming. The smoothest way of doing this currently is to run these commands (related [Spago issue](https://github.com/purescript/spago/issues/506)):
```
$ spago run
$ spago build --watch
$ parcel watch web/index.html -d dist/client --no-cache --public-url /assets
```


Under `web/` we have `index.html` and `index.js` which can be thought of as templates that we load in the browser. This is the entry point for Mosaico react component. In fact, with these files alone, one could run a single page app version of Mosaico (`yarn start-dev`). When the node server of Mosaico is involved, however, the server might want to write something to the files. Or more specifically, it will write to the parcel built version of these files, located under `dist/client/`. This is what `yarn build` will do. Each yarn command is defined in `package.json` under `scripts` object. Let's look at what `yarn build` does:

```
"scripts": {
  "yarn build": "spago build && parcel build web/index.html -d dist/client --no-cache --public-url /assets",
  ...
}
```

First we run `spago build` which compiles our PureScript code into `output/`. This is important, as we use the compiled PureScript in `web/index.js`

```
...
var Mosaico = require("../output/Mosaico/index.js").jsApp();
...

```

After that, we run `parcel build web/index.html -d dist/client --no-cache --public-url /assets`. Here, we build the file `index.html` we have under `web/` into a destination directory `dist/client/`. Parcel does its thing: it finds every dependency it needs (js, css, images, whatever) and places them into `dist/client/`. We don't want Parcel to use any caching and we set the public url of our assets to be `/assets`. This where we look for static files in our server, and parcel will just prefix the file url's with `/assets` in this case.

## Tests

Launch site as described in the Development sections.

The test expects to get account data for testing from environment
variables.

```
export TEST_USER=
export TEST_PASSWORD=
export ENTITLED_USER=
export ENTITLED_PASSWORD=
```

```
spago -x test.dhall test
```
