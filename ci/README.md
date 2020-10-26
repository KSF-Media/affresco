# Affresco CI setup

All the CI for all the apps in this repo is run on [GitHub Actions](https://github.com/features/actions).

We have two CI workflows:
- one that runs on [pull requests](../.github/previews.yml)
- one for [deploying production](../.github/production.yml)

## Editing the workflows

Since the workflows include a bunch of boilerplate, we do not edit the YAML files
by hand, but instead we generate those files from some [Dhall](https://dhall-lang.org) definitions.

The template for the pull-requests CI is [here](./ci-pull-request.dhall), while
the one for production is [here](./ci-master.dhall).

TODO: list of apps
TODO: go through what every workflow does

