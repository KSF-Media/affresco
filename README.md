# Affresco

![Affresco](http://www.hotelfororomano.com/wp-content/gallery/gallery-arte-braocca-roma/vita-di-mose-affresco-cappella-sistina-michelangelo.jpg)

Affresco (IPA: */af'fresko/*) is KSF Media's frontend monorepo: as many beautiful scenes are contained in a fresco, many beautiful frontends can be found in this repo.

![production](https://github.com/KSF-Media/affresco/workflows/production/badge.svg)

| Deploy | Lang(s) |
| --- | --- |
| [Mitt Konto](https://konto.ksfmedia.fi/) | PureScript |
| [KSF National Elections](https://frontends.ksfmedia.fi/elections/index.html) | JavaScript |
| [KSF EU Elections](https://frontends.ksfmedia.fi/elections-eu/index.html) | JavaScript |
| [App Article View](https://app-article.ksfmedia.fi/) | PS/JS |
| [Scripts](https://frontends.ksfmedia.fi/scripts) | JS |
| [Vetrina (test)](https://frontends.ksfmedia.fi/vetrina/index.html) | PS |
| [Podcasts](https://frontends.ksfmedia.fi/podcasts/index.html) | JS |
| [Podcasts (VN)](https://frontends.ksfmedia.fi/podcasts-vn/index.html) | JS |

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

### Styles

All styles should be placed under `/less`. The file should be named after the component it belongs to. Also note that the file `Components.less` conveniently imports every `less` file associated with a component. Newly created styles should be added to this file.

### Production build

#### Apps

For building and deploying single page applications from `apps/`, the `deploy.rb` script is used.
```
$ ruby deploy.rb $APP_NAME
```

#### Scripts

For static scripts, the build command is defined in CI. In this case, we just want to minify all content under `scripts/`.
```
# A simple one-liner to minify all js files
ruby -e 'Dir.glob("scripts/**/*.js").each { |f| `uglifyjs #{f} -o #{f.gsub(/js\z/, "min.js")}` }'
```
Here, for example, a file `scripts/apps/appScript.js` is minified to `scripts/apps/appScript.min.js`.

### Adding a new deployment

See the [CI README](./ci/README.md) for info about the CI setup, and how to add a new app
