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

The list of the deployments is [here](./apps.dhall) - and this is the file that
should be edited when adding a new app. For documentation on the machinery that we
use to generate the CI workflows, see [this file](./workflows.dhall)

The two workflow are slightly different. They both:
- setup the CI environment
- check that the generated workflows are up to date with the Dhall source
- build all the apps
- upload them to a bucket

Then the "preview" workflows posts a comment to the PR with the link to the newly
deployed previews, while the "production" workflows clears the CDN cache so that
the new version of the apps goes live.

## Environment variables

The global environment variables (i.e. the ones shared by more than one app)
should go in every workflow's `env` key, while the ones belonging to a single app
should go in the `apps.dhall` file.

## Adding a new app

1. Edit the [`apps.dhall`](./apps.dhall) file to add the new app details
2. You'll find the production app deployed at `https://frontends.ksfmedia.fi/$deployDir/index.html`

If you need to redirect `index.html` to the `/`, you can add a rewriting rule to the `ksf-frontends-lb` load balancer
(in the `ksf-production` project)

### Getting a "nice url" for the app

Sometimes you need a nicer URL for the deployed frontend, e.g. as it's the case for Mitt Konto.

In this situation you'll need to add a new CDN setup in Google Cloud. Steps:
1. Create a new "Cloud CDN" resource backed by the `ksf-frontends` bucket
2. For this you'll need to create new load balancer. You'll need to add a new "host and path rule", where:
  - the host is the one you need for the app
  - the path rules are:
    - path `/` with a URL rewrite to `$deployDir/index.html` (note that `deployDir` is one of the configurations of an app, and ultimately the location of it in the bucket)
    - path `/*` with a URL rewrite to `/$deployDir/` (note the slashes, they seem to be important)
3. Create a new SSL certificate for the new host on this load balancer, and point a DNS A record to the IP of the load balancer
4. Edit the `refreshCDNSteps` [source](./workflows.dhall) to include a `gcloud` command to clear the CDN cache on new deployments

### Maintenance Mode

Change the `MAINTENANCE_MODE` env var to `true` for the desired app in `apps.dhall`
