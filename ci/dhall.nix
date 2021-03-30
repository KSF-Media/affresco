{ pkgs ? import <nixpkgs> {} }:

let
  easy-dhall = import (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-dhall-nix";
      rev = "eae7f64c4d6c70681e5a56c84198236930ba425e";
      sha256 = "pzcwv9qLuk4qkxw5YOD95krF3YpXftUv3ekYhXYJXfg=";
    }
  ) {
    inherit pkgs;
  };

in
pkgs.runCommand "dummy" {
  buildInputs =
    builtins.attrValues {
      inherit (easy-dhall) dhall-simple dhall-json-simple;
    };
} ""
