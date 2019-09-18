# Affresco

![Affresco](http://www.hotelfororomano.com/wp-content/gallery/gallery-arte-braocca-roma/vita-di-mose-affresco-cappella-sistina-michelangelo.jpg)

Affresco (IPA: */af'fresko/*) is KSF Media's frontend monorepo: as many beautiful scenes are contained in a fresco, many beautiful frontends are contained in this repo.

| App | Deploy status | Langs |
| --- | --- | --- |
| [Mitt Konto](https://konto.ksfmedia.fi/) | [![Netlify Status](https://api.netlify.com/api/v1/badges/078bfa8b-14c4-45f2-8866-5176e7e25d08/deploy-status)](https://app.netlify.com/sites/mitt-konto/deploys) | PureScript |
| [Prenumerera](https://prenumerera.netlify.com/) | [![Netlify Status](https://api.netlify.com/api/v1/badges/4b9be1bf-389a-457f-8cbf-a39662efca3f/deploy-status)](https://app.netlify.com/sites/prenumerera/deploys) | PureScript |
| [KSF National Elections](https://ksf-elections.netlify.com/) | [![Netlify Status](https://api.netlify.com/api/v1/badges/5e80eb03-8a93-4e4e-94f4-966b63e761e5/deploy-status)](https://app.netlify.com/sites/ksf-elections/deploys) | JavaScript |
| [KSF EU Elections](https://ksf-elections-eu.netlify.com/) | [![Netlify Status](https://api.netlify.com/api/v1/badges/4c33a8b4-0409-44b0-91e5-7d20c36a3095/deploy-status)](https://app.netlify.com/sites/ksf-elections-eu/deploys) | JavaScript |
| [Duellen](https://duellen.netlify.com/) | [![Netlify Status](https://api.netlify.com/api/v1/badges/7984db0a-22d1-49f1-badf-660d9ac3ad3b/deploy-status)](https://app.netlify.com/sites/duellen/deploys) | JavaScript |
| [App Article View](https://app-article.ksfmedia.fi/) | [![Netlify Status](https://api.netlify.com/api/v1/badges/1754f1af-5065-4ebb-b498-97ace58a8817/deploy-status)](https://app.netlify.com/sites/app-article/deploys) | PS/JS |


## Developing

To install all packages run `yarn install` in the root of the repo. 

You might also want to install the tools globally for more convenience: `npm install -g purescript spago parcel`

We have different kinds of apps, and they require different initial setup. Move inside of some app's folder (e.g. `apps/mitt-konto`), and then:
- JavaScript only: no setup needed
- PureScript only: run `spago build --watch` to get a file-watching build, or use some editor integration to do this watch/rebuild for you (e.g. see [here](https://github.com/purescript/documentation/blob/master/ecosystem/Editor-and-tool-support.md) for the things mentioning "ide")
- Mixed JavaScript/PureScript: run `yarn build-purs` to get the compiled PureScript bundle in the right place

After this initial setup, running `yarn start` should give you a hot reloading local dev server.


### Structure

It's a monorepo, and there's a separation between **packages** and **apps**:
- **packages** are actual npm packages (should be prefixed with `@affresco/`), and represent units of compilation to be included in apps.
  
  Each package should include an entry point file. This is typically `index.js` in the root of the package directory.
  
  The granularity here is important: JS packages might be as small as we wish, PureScript packages should be as big as possible - this is because their size is not really important for PureScript apps, but it should be so that we can easily include them into JS apps by just compiling a bundle (e.g. the `login` is a separate package. See the `package.json` in `apps/app-article` for how to compile it.
  
  When making a new PureScript package, add it to the global `packages.dhall` - after that you'll be able to import it in your apps or in other packages (note that the compiler will prevent you from having circular dependencies anyways.
- **apps** is where all the applications are. These are not supposed to be required in any other application, however a `package.json` should still exist for internal and external dependencies, and build scripts.


### Sharing assets

All assets are shared by all packages and apps:
- all styles are in `less`
- all fonts in `fonts`
- all images in `images`

### Production build

#### Apps

For building and deploying single page applications from `apps/`, the `deploy.rb` script is used.
```
$ ruby deploy.rb $APP_NAME
```

#### Scripts

For static scripts, the build command is defined in Netlify. In this case, we just want to minify all content under `scripts/`.
```
# A simple one-liner to minify all js files
ruby -e 'Dir.glob("scripts/**/*.js").each { |f| `uglifyjs #{f} -o #{f.gsub(/js\z/, "min.js")}` }'
```
Here, for example, a file `scripts/apps/appScript.js` is minified to `scripts/apps/appScript.min.js`.
