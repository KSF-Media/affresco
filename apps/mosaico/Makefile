run-dev: run-spago-watch run-esbuild-watch run-browser-sync

run-spago-watch:
	spago build --watch
run-browser-sync:
	node runBrowsersync.js
run-esbuild-watch:
	node runBuild.js

function redirToRoot(req, res, next) {
  res.writeHead(302, {
    Location: "/",
  });
  req.url = "/";
  res.end();
  return next();
}
