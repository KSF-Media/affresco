import { exec } from "child_process";
import { create } from "browser-sync";
const bs = create();
import * as build from "./build.mjs";

bs.watch("dist/assets/*").on("change", file => bs.reload(file));

bs.watch("static/*").on("change", file => {
  exec("cp ./static/* ./dist/assets/");
});

bs.watch([
  "./web/*",
  "./output/Mosaico/index.js",
  "../../less/**/*",
  "./src/_site.scss",
  "./src/css/**/*",
]).on("change", file => {
  build.runBuild();
});

function redirToRoot(req, res, next) {
  res.writeHead(302, {
    Location: "/",
  });
  req.url = "/";
  res.end();
  return next();
}

bs.init({
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
