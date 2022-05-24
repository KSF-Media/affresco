import esbuild from "esbuild";

export function buildOrServe(buildOptions) {
  if (process.argv[2] == "serve") {
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
  return esbuild.build(buildOptions);
}

export const buildSettings = {
  bundle: true,
  minify: process.env.NODE_ENV === "production",
  treeShaking: true,
  sourcemap: 'external',
  outdir: "dist",
  assetNames: "[name]-[hash]",
  chunkNames: "[name]-[hash]",
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
};
