{ nixpkgs ? import ./nixpkgs.nix {} }:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = pkgs.haskell.packages.ghc881;

  project = haskellPackages.callPackage ./release.nix {
    inherit nixpkgs haskellPackages;
  };
in
pkgs.mkShell {
  name = "shell";
  LANG = "en_US.UTF-8";
  inputsFrom = [ project.env ];
  buildInputs = [
    pkgs.glibcLocales
    haskellPackages.cabal-install
    haskellPackages.ghcid
  ];
}
