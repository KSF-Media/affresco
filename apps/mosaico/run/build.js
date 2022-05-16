var esbuild = require("esbuild");
var lessLoader = require("esbuild-plugin-less").lessLoader;
require("dotenv").config();
const glob = require("tiny-glob");

const fs = require("fs");
const cheerio = require("cheerio");
const util = require("util");
const exec = util.promisify(require("child_process").exec);

module.exports.runBuild = async function () {
  try {
    console.log("Bundling javascript...");

    const templateFile = "./web/index.html";
    const template = cheerio.load(fs.readFileSync(templateFile, "utf8"));

    const buildOpts = {
      entryPoints: ["./web/index.js"],
      entryNames: "[name]-[hash]",
      bundle: true,
      outdir: "./dist/assets",
      minify: process.env.NODE_ENV === "production",
      metafile: true,
      plugins: [lessLoader()],
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
      minify: process.env.NODE_ENV === "production",
      treeShaking: true,
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
    await esbuild.build(staticBuildOpts);
    await exec("mkdir -p dist/assets && cp -R ./dist/static/* ./dist/assets/");

    fs.writeFile("./dist/index.html", template.html(), () => {
      console.log("Wrote index.html");
    });
  } catch (e) {
    console.warn("Build error", e);
  }
};
