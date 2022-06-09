import esbuild from "esbuild";
import babel from "@babel/core";
import htmlPlugin from "@chialab/esbuild-plugin-html";
import { lessLoader } from "esbuild-plugin-less";
import * as util from "util";
import * as fs from "fs";

const writeFile = util.promisify(fs.writeFile);

export function buildOrServe(buildOptions) {
  if (process.argv.some((arg) => arg == "serve")) {
    const port = parseInt(process.env.PORT, 10) || 3000;
    console.log("Running in http://localhost:" + port);
    return esbuild.serve(
      {
        servedir: buildOptions.outdir,
        port,
      },
      buildOptions
    );
  }
  return esbuild.build(buildOptions).then(transpileIfMinify(buildOptions));
}

export function transpileIfMinify(buildOptions) {
  return buildOptions.minify ? transpile : result => Promise.resolve(result);
}

export function transpile(buildResult) {
  console.log("Transpiling results with Babel");
  return Promise.all(
    buildResult.metafile.outputs.map((file) =>
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
  ).then(() => buildResult);
}

export const buildSettings = {
  bundle: true,
  minify: process.env.NODE_ENV === "production",
  treeShaking: true,
  sourcemap: "external",
  outdir: "dist",
  assetNames: "[name]-[hash]",
  chunkNames: "[name]-[hash]",
  metafile: true,
  loader: {
    ".js": "jsx",
    ".jpg": "file",
    ".png": "file",
    ".woff": "file",
    ".ttf": "file",
    ".svg": "file",
    ".otf": "file",
    ".eot": "file",
    ".woff2": "file",
    ".gif": "file",
  },
  define: {
    "process.env.BOTTEGA_URL": '"' + process.env.BOTTEGA_URL + '"',
    "process.env.LETTERA_URL": '"' + process.env.LETTERA_URL + '"',
    "process.env.PERSONA_URL": '"' + process.env.PERSONA_URL + '"',
    "process.env.DUELLEN_URL": '"' + process.env.DUELLEN_URL + '"',
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
  plugins: [
    htmlPlugin(),
    lessLoader(),
  ],
};
