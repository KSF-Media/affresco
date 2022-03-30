var esbuild = require("esbuild");
var lessLoader = require("esbuild-plugin-less").lessLoader;
const { exec } = require("child_process");
require("dotenv").config();

console.log("Bundling javascript...");

const watch = process.argv.includes("dev") ? true : false;
const buildOpts = {
  entryPoints: ["./web/index.js", "./web/ads.js"],
  bundle: true,
  outdir: "./dist/assets",
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
    ".html": "text",
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
    "process.env.JANRAIN_XD_RECEIVER_PATH": '"' + process.env.JANRAIN_XD_RECEIVER_PATH + '"',
  },
  treeShaking: true,
  watch: watch,
};

esbuild.build(buildOpts);
exec("mkdir -p dist && cp ./web/index.html ./dist/index.html");
