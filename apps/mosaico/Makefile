all: run-dev

run-dev: run-spago-watch run-browser-sync
run-spago-watch:
	spago build --watch
run-browser-sync:
	node ./run/hot-reload.mjs
start-dev-server:
	node --input-type=module -r dotenv/config -e 'import index from "./output/Main/index.js"; index.main();' dotenv_config_path=.env.development
