#!/usr/bin/env bash

versions=$(gcloud app versions list \
                  --service ${app.id} \
                  --sort-by '~VERSION.ID' \
                  --filter 'traffic_split = 0.0' \
                  --format 'value(VERSION.ID)' | sed 1,5d)
for version in $versions; do
    gcloud app versions delete '$version' \
           --service ${app.id} \
           done
