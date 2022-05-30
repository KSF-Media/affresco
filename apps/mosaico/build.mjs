import esbuild from "esbuild";
import { buildSettings, transpileIfMinify } from "../../esbuild.defaults.mjs";
import htmlPlugin from "@chialab/esbuild-plugin-html";
import { lessLoader } from "esbuild-plugin-less";
import * as util from "util";
import glob from "tiny-glob";
import child_process from "child_process";
const exec = util.promisify(child_process.exec);

const mainSettings = {
  ...buildSettings,
  outdir: "dist/assets",
  entryPoints: ["./web/index.html"],
};

export function buildMain() {
  return esbuild.build(mainSettings).then(transpileIfMinify(mainSettings));
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
  return buildMain().then(() => buildStatic(false));
}

async function buildOrServe() {
  if (process.argv.some((arg) => arg == "serve")) {
    const port = parseInt(process.env.PORT, 10) || 3000;
    await buildStatic(false);
    buildStatic(true);
    console.log("Running in http://localhost:" + port);
    return esbuild.serve(
      {
        servedir: mainSettings.outdir,
        port,
      },
      mainSettings
    );
  }
  return runBuild();
}

await buildOrServe();
