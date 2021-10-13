#!/usr/bin/env bash

if [[ $# -ne 1 ]]; then
    echo "Illegal number of parameters" >&2
    exit 2
fi
APP_ID=$1

versions=$(gcloud app versions list \
                  --service $APP_ID \
                  --sort-by '~VERSION.ID' \
                  --filter 'traffic_split = 0.0' \
                  --format 'value(VERSION.ID)' | sed 1,10d)

echo "Deleting $APP_ID versions: $versions"
gcloud app versions delete $versions --service $APP_ID --quiet
