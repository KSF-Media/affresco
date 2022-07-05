#!/usr/bin/env bash

error_exit(){
    echo "Tests failed! Killing server."
    kill -9 $1
    exit 1
}
cp .env.local .env.test
echo "DISABLE_ADS=1" >> .env.test
node --input-type=module -r dotenv/config -e 'import {main} from "./output/Main/index.js"; main();' dotenv_config_path=.env.test &
PIDTOKILL=$! 
echo "Started server with pid $PIDTOKILL"
sleep 5
echo "Running tests"
spago -x test.dhall test || error_exit $PIDTOKILL
echo "Killing server $PIDTOKILL"
kill -9 $PIDTOKILL
