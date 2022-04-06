var esbuild = require("esbuild");
var lessLoader = require("esbuild-plugin-less").lessLoader;
const { exec } = require("child_process");
require("dotenv").config();

const fs = require("fs");
const cheerio = require("cheerio");

const templateFile = "./web/index.html";
const template = cheerio.load(fs.readFileSync(templateFile, "utf8"));

console.log("Bundling javascript...");

// When developing, run build on file change
const watch = process.argv.includes("dev") ? true : false;

// Refer to assets according to PUBLIC_URL env var
// This will replace src or href of script and link tags
template(".mosaico-asset").each((ix, elem) => {
  const src = template(elem).attr("src");
  const href = template(elem).attr("href");
  const publicUrl = process.env.PUBLIC_URL || "";
  if (src) {
    template(elem).attr("src", publicUrl + "/assets/" + src);
  } else if (href) {
    template(elem).attr("href", publicUrl + "/assets/" + href);
  }
});

const buildOpts = {
  entryPoints: ["./web/index.js", "./web/ads.js"],
  bundle: true,
  outdir: "./dist/assets",
  minify: true,
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
  watch: watch,
};

esbuild.build(buildOpts);

exec("mkdir -p dist/assets && cp ./static/* ./dist/assets/", (err, stdout, stderr) => {
  if (err) {
    console.log("Error when creating dirs and stuff: ", err);
  }
  fs.writeFile("./dist/index.html", template.html(), () => {
    console.log("Wrote index.html");
  });
});
