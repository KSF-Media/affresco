SHELL := bash

all: generate-branch-ci generate-master-ci

.PHONY: generate-branch-ci generate-master-ci

generate-branch-ci:
	@nix-shell ci/dhall.nix --run 'dhall-to-yaml --omit-empty <<< "./ci/ci-pull-request.dhall"' > .github/workflows/previews.yml

generate-master-ci:
	@nix-shell ci/dhall.nix --run 'dhall-to-yaml --omit-empty <<< "./ci/ci-master.dhall"' > .github/workflows/production.yml

generate-ci-local:
	npx dhall-to-yaml --omit-empty --file "./ci/ci-master.dhall" --output .github/workflows/production.yml; npx dhall-to-yaml --omit-empty --file "./ci/ci-pull-request.dhall" --output .github/workflows/previews.yml;
