const { exec } = require("child_process");
var bs = require("browser-sync").create();
bs.watch("build/assets/*").on("change", bs.reload);

bs.watch("static/*").on("change", () => {
  console.log("ayoo");
  exec("cp ./static/* ./build/assets/");
  bs.reload();
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
    baseDir: "web",
    index: "index.html",
  },
  serveStatic: [
    {
      route: "/assets",
      dir: "./build/assets",
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
