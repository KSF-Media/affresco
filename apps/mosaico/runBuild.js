var esbuild = require("esbuild");
var lessLoader = require("esbuild-plugin-less").lessLoader;

require("dotenv").config();

/* esbuild.build({
 *   entryPoints: ["./web/index.js"],
 *   bundle: true,
 *   outdir: "./build",
 *   plugins: [lessLoader()],
 *   loader: {
 *     ".js": "jsx",
 *     ".png": "dataurl",
 *     ".woff": "dataurl",
 *     ".ttf": "dataurl",
 *     ".svg": "dataurl",
 *     ".otf": "dataurl",
 *     ".eot": "dataurl",
 *     ".woff2": "dataurl",
 *     ".gif": "dataurl",
 *   },
 *   define: {
 *     "process.env.BOTTEGA_URL": '"' + process.env.BOTTEGA_URL + '"',
 *     "process.env.LETTERA_URL": '"' + process.env.LETTERA_URL + '"',
 *     "process.env.PERSONA_URL": '"' + process.env.PERSONA_URL + '"',
 *     "process.env.PUBLIC_URL": '"' + process.env.PUBLIC_URL + '"',
 *     "process.env.INSECURE_COOKIE": '"' + process.env.INSECURE_COOKIE + '"',
 *     "process.env.FACEBOOK_APP_ID": '"' + process.env.FACEBOOK_APP_ID + '"',
 *     "process.env.GOOGLE_CLIENT_ID": '"' + process.env.GOOGLE_CLIENT_ID + '"',
 *     "process.env.PAPER": '"' + process.env.PAPER + '"',
 *     "process.env.HIDE_LOGIN_LINKS": '"' + process.env.HIDE_LOGIN_LINKS + '"',
 *   },
 * }); */

const buildOpts = {
  entryPoints: ["./web/index.js"],
  bundle: true,
  outdir: "./build",
  plugins: [lessLoader()],
  loader: {
    ".js": "jsx",
    ".png": "dataurl",
    ".woff": "dataurl",
    ".ttf": "dataurl",
    ".svg": "dataurl",
    ".otf": "dataurl",
    ".eot": "dataurl",
    ".woff2": "dataurl",
    ".gif": "dataurl",
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
  },
  treeShaking: true,
};

esbuild.serve(
  {
    servedir: "./build",
  },
  buildOpts
);
