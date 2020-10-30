# Vetrina-test

This is a test deployment for Vetrina (the fast buying path), for:
- convenience of development (since the "production" one is embedded in the main sites for now)
- running end-to-end tests

## End-to-end tests

Run:

```bash
$ yarn install
$ export PERSONA_URL=https://persona.staging.ksfmedia.fi/v1
$ spago test
```

### Fixing the tests

Vetrina will change, and the tests will need to be patched.

Some tips on the possible workflow to do that:
- figure out where the hiccup is, and add a `waitFor` just before that, so you have some time to look at the page
- logs go a long way to help in figuring out where things break
- to get selectors out of Chrome you can right-click on an element, Inspect,
  then right-click on the element in the Inspector, and "Copy Selector".
  Then you can just copy-paste that in the code