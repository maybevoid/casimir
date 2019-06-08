{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.pkgs.haskellPackages.callCabal2nix
  "implicit-effects"
  ../code
  {}