#!/usr/bin/env bash

node -r dotenv/config -e 'require("./output/Main/index").main()' dotenv_config_path=.env.local &
PIDTOKILL=$! 
echo "Started server with pid $PIDTOKILL"
sleep 5
echo "Running tests"
spago -x test.dhall test
echo "Killing server $PIDTOKILL"
kill -9 $PIDTOKILL