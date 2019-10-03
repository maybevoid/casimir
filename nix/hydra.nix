let
  pkgs = import <nixpkgs> {};

  jobs = rec {
    build = { system ? builtins.currentSystem }:
      let
        nixpkgs = import <nixpkgs> { inherit system; };
      in

      import ./release.nix {
        inherit nixpkgs;
      };
  };
in
  jobs
