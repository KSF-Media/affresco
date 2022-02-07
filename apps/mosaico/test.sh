#!/usr/bin/env bash

error_exit(){
    echo "Tests failed, killing server."
    kill -9 $1
    exit 1
}

node -r dotenv/config -e 'require("./output/Main/index").main()' dotenv_config_path=.env.local &
PIDTOKILL=$! 
echo "Started server with pid $PIDTOKILL"
sleep 5
echo "Running tests"
spago -x test.dhall test || error_exit $PIDTOKILL
echo "Killing server $PIDTOKILL"
kill -9 $PIDTOKILL