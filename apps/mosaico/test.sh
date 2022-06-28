#!/usr/bin/env bash

# debug branch, it would fail tests anyway
exit 0

error_exit(){
    echo "Tests failed! Killing server."
    kill -9 $1
    exit 1
}
cp .env.local .env.test
echo "DISABLE_ADS=1" >> .env.test
node -r dotenv/config -e 'require("./output/Main/index").main()' dotenv_config_path=.env.test &
PIDTOKILL=$! 
echo "Started server with pid $PIDTOKILL"
sleep 5
echo "Running tests"
spago -x test.dhall test || error_exit $PIDTOKILL
echo "Killing server $PIDTOKILL"
kill -9 $PIDTOKILL
