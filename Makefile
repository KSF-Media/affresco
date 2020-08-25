SHELL := bash

all: generate-branch-ci generate-master-ci

.PHONY: generate-branch-ci generate-master-ci

generate-branch-ci:
	@nix-shell ci/dhall.nix --run 'dhall-to-yaml --omit-empty <<< "./ci/ci-pull-request.dhall"' > .github/workflows/previews.yml

generate-master-ci:
	@nix-shell ci/dhall.nix --run 'dhall-to-yaml --omit-empty <<< "./ci/ci-master.dhall"' > .github/workflows/production.yml