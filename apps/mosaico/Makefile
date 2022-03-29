all: run-dev

run-dev: run-spago-watch run-esbuild-watch run-browser-sync

run-spago-watch:
	spago build --watch
run-browser-sync:
	node ./run/hot-reload.js
run-esbuild-watch:
	node ./run/build.js dev
