{ pkgs ? import <nixpkgs> {} }:

let
  easy-dhall = import (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-dhall-nix";
      rev = "90957969850a44481c6e150350c56e8b53b29e1e";
      sha256 = "1hsmp3cb0k554kh0jlfzpdzx2b8ndyh2gdykmw9hw41haaw16mmi";
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
