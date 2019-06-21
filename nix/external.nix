{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      currentProject = self.callPackage ./project.nix {};
    };
  };

  haskellDeps = ps: with ps; [
    base
    currentProject
  ];

  ghc = haskellPackages.ghcWithHoogle haskellDeps;

  nixPackages = [
    ghc
    haskellPackages.currentProject
    haskellPackages.cabal-install
  ];
in
pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
}