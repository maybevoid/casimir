{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  project = haskellPackages.callPackage ./project.nix { };
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  LANG = "en_US.UTF-8";
  buildInputs = project.env.nativeBuildInputs ++ [
    pkgs.cabal2nix
    pkgs.glibcLocales
    haskellPackages.cabal-install
  ];
}