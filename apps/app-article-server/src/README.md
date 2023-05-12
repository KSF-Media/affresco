# App article server

Run `yarn build-server && yarn start-server` to get the server up.

Then call `http://localhost:3000/article/2b7a160f-6d38-46e9-8851-9ca44146ce7f` for some content.

To best simulate behaviour in app use query parameters `paper`, `fontSize` and `mode`. `paper` takes values `hbl, on and vn`. `fontSize` takes `1.06, 1.5, 2.0, 2.5 or 3.0` (default being 1.06). To test dark mode set `mode=dark`.
