import * as esbuild from "esbuild";
import babel from "@babel/core";
import { lessLoader } from "esbuild-plugin-less";
import { config } from "dotenv";
config();
import glob from "tiny-glob";

import path from "path";
import { sassPlugin } from "esbuild-sass-plugin";
import postcss from "postcss";
import autoprefixer from "autoprefixer";
import tailwindcss from "tailwindcss";

import * as fs from "fs";
import cheerio from "cheerio";
import * as util from "util";
import * as cp from "child_process";
const exec = util.promisify(cp.exec);
const writeFile = util.promisify(fs.writeFile);

const minify = process.env.NODE_ENV === "production";

const plugins = [
  lessLoader(),
  sassPlugin({
    async transform(source, resolveDir) {
      const { css } = await postcss()
        .use(autoprefixer)
        .use(tailwindcss(path.resolve("./tailwind.config.js")))
        .process(source, {
          from: "./src/_site.scss",
        });
      return css;
    },
  }),
];

export async function runBuild() {
  try {
    console.log("Bundling javascript...");

    const templateFile = "./web/index.html";
    const template = cheerio.load(fs.readFileSync(templateFile, "utf8"));

    const buildOpts = {
      entryPoints: ["./web/index.js"],
      entryNames: "[name]-[hash]",
      bundle: true,
      outdir: "./dist/assets",
      minify,
      metafile: true,
      plugins,
      loader: {
        ".js": "jsx",
        ".png": "file",
        ".woff": "file",
        ".ttf": "file",
        ".svg": "file",
        ".otf": "file",
        ".eot": "file",
        ".woff2": "file",
        ".gif": "file",
        ".html": "file",
      },
      define: {
        "process.env.BOTTEGA_URL": '"' + process.env.BOTTEGA_URL + '"',
        "process.env.LETTERA_URL": '"' + process.env.LETTERA_URL + '"',
        "process.env.PERSONA_URL": '"' + process.env.PERSONA_URL + '"',
        "process.env.PUBLIC_URL": '"' + process.env.PUBLIC_URL + '"',
        "process.env.INSECURE_COOKIE": '"' + process.env.INSECURE_COOKIE + '"',
        "process.env.FACEBOOK_APP_ID": '"' + process.env.FACEBOOK_APP_ID + '"',
        "process.env.GOOGLE_CLIENT_ID": '"' + process.env.GOOGLE_CLIENT_ID + '"',
        "process.env.PAPER": '"' + process.env.PAPER + '"',
        "process.env.HIDE_LOGIN_LINKS": '"' + process.env.HIDE_LOGIN_LINKS + '"',
        "process.env.SENTRY_DSN": '"' + process.env.SENTRY_DSN + '"',
        "process.env.JANRAIN_FLOW_VERSION": '"' + process.env.JANRAIN_FLOW_VERSION + '"',
        "process.env.JANRAIN_LOGIN_CLIENT_ID": '"' + process.env.JANRAIN_LOGIN_CLIENT_ID + '"',
        "process.env.JANRAIN_SSO_SERVER": '"' + process.env.JANRAIN_SSO_SERVER + '"',
        "process.env.JANRAIN_XD_RECEIVER_PATH": '"' + process.env.JANRAIN_XD_RECEIVER + '"',
      },
      treeShaking: true,
    };

    const result = await esbuild.build(buildOpts);
    // Refer to assets according to PUBLIC_URL env var
    // This will replace src or href of script and link tags
    let outfiles = Object.keys(result.metafile.outputs);

    template(".mosaico-asset").each((ix, elem) => {
      const src = template(elem).attr("src");
      const href = template(elem).attr("href");

      const assetName = src || href;
      const asset = assetName.split(".");

      // Makes a regex e.g. "/index-\w+\.js/"
      // The esbuild generated files are named like "index-EDGJWUC6.js"
      const assetRegex = new RegExp(asset[0] + "-\\w+\\." + asset[1]);
      const assetPath = outfiles.reduce((acc, fileName) => {
        const matchingPath = fileName.match(assetRegex);
        return matchingPath ? matchingPath[0] : acc;
      }, null);

      const publicUrl = process.env.PUBLIC_URL || "";
      if (src) {
        template(elem).attr("src", publicUrl + "/assets/" + assetPath);
      } else if (href) {
        template(elem).attr("href", publicUrl + "/assets/" + assetPath);
      }
    });
    const staticEntryPoints = await glob("./static/**/*.js");
    const staticBuildOpts = {
      entryPoints: staticEntryPoints,
      bundle: true,
      outdir: "./dist/static",
      minify,
      plugins,
      treeShaking: true,
      metafile: true,
    };

    /* The static page building is a three-step process:
       1) Copy everything from ./static to ./dist/static
          Essentially this copies the .html files as-is.
       2) Compile javascript from ./static to ./dist/static
          This allows using modern JS features such as modules.
          Server-side rendering serves static pages from ./dist/static.
       3) Copy everything from ./dist/static to ./dist/assets
          The browser static pages are loaded from ./dist/assets.
     */

    await exec("mkdir -p dist/static && cp -R ./static/* ./dist/static/");
    const staticResult = await esbuild.build(staticBuildOpts);
    await exec("mkdir -p dist/assets && cp -R ./dist/static/* ./dist/assets/");

    const staticFiles = Object.keys(staticResult.metafile.outputs);

    if (minify) {
      console.log("Transpiling results with Babel");
      await Promise.all(
        [...outfiles, ...staticFiles].map((file) =>
          file.endsWith(".js")
            ? babel
                .transformFileAsync(file, {
                  presets: [
                    [
                      "@babel/preset-env",
                      {
                        modules: false,
                      },
                    ],
                    "@babel/preset-react",
                  ],
                })
                .then(({ code }) => writeFile(file, code))
            : Promise.resolve(undefined)
        )
      );
    }

    await exec("mkdir -p dist/ && cp ./redir/redir.json ./dist/");

    await writeFile("./dist/index.html", template.html());
    console.log("Wrote index.html");
  } catch (e) {
    console.warn("Build error", e);
  }
}
