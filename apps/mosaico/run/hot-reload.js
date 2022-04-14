const { exec } = require("child_process");
var bs = require("browser-sync").create();
var build = require("./build");
bs.watch("dist/assets/*").on("change", bs.reload);

bs.watch("static/*").on("change", () => {
  exec("cp ./static/* ./dist/assets/");
  bs.reload();
});

bs.watch("./web/*").on("change", () => {
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
