var esbuild = require("esbuild");
var lessLoader = require("esbuild-plugin-less").lessLoader;
const { exec } = require("child_process");
require("dotenv").config();

const fs = require("fs");
const cheerio = require("cheerio");

module.exports.runBuild = function () {
  console.log("Bundling javascript...");

  const templateFile = "./web/index.html";
  const template = cheerio.load(fs.readFileSync(templateFile, "utf8"));

  const buildOpts = {
    entryPoints: ["./web/index.js"],
    entryNames: "[name]-[hash]",
    bundle: true,
    outdir: "./dist/assets",
    minify: true,
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

  esbuild.build(buildOpts).then((result) => {
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
    exec("mkdir -p dist/assets && cp -R ./static/* ./dist/assets/", (err, stdout, stderr) => {
      if (err) {
        console.log("Error when creating dirs and stuff: ", err);
      }
      fs.writeFile("./dist/index.html", template.html(), () => {
        console.log("Wrote index.html");
      });
    });
  });
};
