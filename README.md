# Affresco

![Affresco](http://www.hotelfororomano.com/wp-content/gallery/gallery-arte-braocca-roma/vita-di-mose-affresco-cappella-sistina-michelangelo.jpg)

Affresco (IPA: */af'fresko/*) is KSF Media's frontend monorepo: as many beautiful scenes are contained in a fresco, many beautiful apps are contained in this repo.

| App | Deploy status |
| --- | --- |
| Mitt Konto | [![Netlify Status](https://api.netlify.com/api/v1/badges/078bfa8b-14c4-45f2-8866-5176e7e25d08/deploy-status)](https://app.netlify.com/sites/mitt-konto/deploys) |
| Prenumerera | [![Netlify Status](https://api.netlify.com/api/v1/badges/4b9be1bf-389a-457f-8cbf-a39662efca3f/deploy-status)](https://app.netlify.com/sites/prenumerera/deploys) |
| KSF National Elections | [![Netlify Status](https://api.netlify.com/api/v1/badges/5e80eb03-8a93-4e4e-94f4-966b63e761e5/deploy-status)](https://app.netlify.com/sites/ksf-elections/deploys) |
| KSF EU Elections | [![Netlify Status](https://api.netlify.com/api/v1/badges/4c33a8b4-0409-44b0-91e5-7d20c36a3095/deploy-status)](https://app.netlify.com/sites/ksf-elections-eu/deploys) |
| Duellen | [![Netlify Status](https://api.netlify.com/api/v1/badges/7984db0a-22d1-49f1-badf-660d9ac3ad3b/deploy-status)](https://app.netlify.com/sites/duellen/deploys) |
| App Article View | [![Netlify Status](https://api.netlify.com/api/v1/badges/1754f1af-5065-4ebb-b498-97ace58a8817/deploy-status)](https://app.netlify.com/sites/app-article/deploys) |

## Initializing

For initializing the repo, run:
```bash
yarn install-all
```
This installs `npm` packages, `spago` packages and bootstraps the monorepo dependencies with `lerna`.

It may be sometimes useful to only build purescript packages, do it by running:
```bash
yarn build-purs
```
This also creates an `index.js` entry point file for every purescript module, which can then be required by any javascript application.

## Structure

It's a monorepo.

There is a separation of **packages** and **apps**.

**packages** is for npm packages, or individual components that are used in applications throughout the repo.
Each package should include an entry point file. This is typically `index.js` in the root of the package directory.
Note that for Purescript packages, these entry point files should be created with the `build-purs.rb` script.

**apps** is where all the applications are. These are not supposed to be required in any other application, however a `package.json` should still exist for internal and external dependencies.

### Sharing assets

The shared assets are, for reasons of development/build comfort, located in `images`, `less` and `fonts` directories. These asset directories can (and should) be then symlinked into any package or app.

The styles of each package should be symlinked into the `less` directory.

## Developing

Each package should be initialized with `yarn init`. Prefix the name of the package with `@affresco/`, so that it's obvious from looking at any `package.json` which of the dependencies point to this repo and which do not.

When developing a package with Purescript, `yarn build-purs` should be run after making changes. This compiles and bundles the Purescript package to a single `index.js` file.

### Requiring monorepo packages

Just include them in the `package.json` as a normal dependency. By running `lerna bootstrap`, the dependencies are linked.
