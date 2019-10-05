{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  project = haskellPackages.callPackage ./release.nix {
    inherit nixpkgs;
  };
in
pkgs.mkShell {
  name = "shell";
  LANG = "en_US.UTF-8";
  inputsFrom = [ project.env ];
  buildInputs = [
    pkgs.glibcLocales
    haskellPackages.cabal-install
  ];
}