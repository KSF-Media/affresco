import esbuild from "esbuild";
import { buildSettings, transpileIfMinify } from "../../esbuild.defaults.mjs";
import * as util from "util";
import glob from "tiny-glob";
import * as fs from "fs";
import child_process from "child_process";
import * as cheerio from "cheerio";
import { create } from "browser-sync";

const exec = util.promisify(child_process.exec);
const writeFile = util.promisify(fs.writeFile);
const rmFile = util.promisify(fs.rm);

const mainSettings = {
  ...buildSettings,
  assetNames: "[name]-[hash]",
  chunkNames: "[name]-[hash]",
  outdir: "dist/assets",
  entryPoints: ["./web/index.html"],
};

function rewriteIndexHtmlPathsToAbsolute(result) {
  // The HTML builder doesn't use publicPath for some reason...
  // so we have to fix the src=index.hash.js link to src=/index.hash.js
  let outfiles = Object.keys(result.metafile.outputs);

  const templateFile = "./dist/assets/index.html";
  const template = cheerio.load(fs.readFileSync(templateFile, "utf8"));

  template("script, link[rel=stylesheet]").each((ix, elem) => {
    const src = template(elem).attr("src");
    const href = template(elem).attr("href");

    const assetName = src || href;
    if (!assetName || assetName.indexOf("//") >= 0) {
      return;
    }

    const publicUrl = process.env.PUBLIC_URL || "";
    if (src) {
      template(elem).attr("src", publicUrl + "/assets/" + assetName);
      //.attr("class", undefined);
    } else if (href) {
      template(elem).attr("href", publicUrl + "/assets/" + assetName);
      //.attr("class", undefined);
    }
  });

  return writeFile("./dist/index.html", template.html()).then(() =>
    rmFile("./dist/assets/index.html", { force: true })
  );
}

export function buildMain(watch) {
  return esbuild
    .build({
      ...mainSettings,
      watch: watch
        ? {
            onRebuild: (error, result) => {
              if (error) console.error("watch build failed:", error);
              else return rewriteIndexHtmlPathsToAbsolute(result);
            },
          }
        : watch,
    })
    .then(transpileIfMinify(mainSettings))
    .then(rewriteIndexHtmlPathsToAbsolute);
}

export async function buildStatic(watch) {
  /* The static page building is a three-step process:
       1) Copy everything from ./static to ./dist/static
          Essentially this copies the .html files as-is.
       2) Compile javascript from ./static to ./dist/static
          This allows using modern JS features such as modules.
          Server-side rendering serves static pages from ./dist/static.
       3) Copy everything from ./dist/static to ./dist/assets
          The browser static pages are loaded from ./dist/assets.
     */

  await exec("mkdir -p dist/static && mkdir -p dist/assets && cp -R ./static/* ./dist/static/");

  const staticEntryPoints = await glob("./static/**/*.js");
  const staticBuildOpts = {
    ...buildSettings,
    entryPoints: staticEntryPoints,
    outbase: "static",
    outdir: "dist/static",
    watch,
  };
  await esbuild.build(staticBuildOpts).then(transpileIfMinify(staticBuildOpts));;
  await exec("mkdir -p dist/assets && cp -R ./dist/static/* ./dist/assets/");
}

function runBuild() {
  return buildMain(false).then(() => buildStatic(false));
}

async function buildOrServe() {
  if (process.argv.some((arg) => arg == "serve")) {
    const statics = buildStatic(true);
    const main = buildMain(true);

    return Promise.all([statics, main]).then(() => {
      const bs = create();
      bs.watch("dist/**/*").on("change", (file) => bs.reload(file));

      function redirToRoot(req, res, next) {
        res.writeHead(302, {
          Location: "/",
        });
        req.url = "/";
        res.end();
        return next();
      }

      return bs.init({
        server: {
          baseDir: "dist",
          index: "index.html",
        },
        serveStatic: [
          {
            route: "/assets",
            dir: "./dist/assets",
          },
        ],
        middleware: [
          {
            route: "/artikel",
            handle: redirToRoot,
          },
          { route: "/sida", handle: redirToRoot },
        ],
        single: true,
      });
    });
  }
  return runBuild();
}

await buildOrServe();
