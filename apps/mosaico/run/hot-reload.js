const { exec } = require("child_process");
var bs = require("browser-sync").create();
var build = require("./build");

bs.watch("dist/assets/*").on("change", file => bs.reload(file));

bs.watch("static/*").on("change", file => {
  exec("cp ./static/* ./dist/assets/");
  bs.reload(file);
});

bs.watch("./web/*").on("change", file => {
  build.runBuild();
  bs.reload(file);
});

bs.watch("../../less/**/*").on("change", file => {
  build.runBuild();
  bs.reload(file);
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
