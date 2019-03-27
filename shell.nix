{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  LANG = "en_US.UTF-8";
  buildInputs = project.env.nativeBuildInputs ++ [
    pkgs.glibcLocales
    haskellPackages.hpack
    haskellPackages.cabal-install
  ];
}