{
  nixpkgs ? import ./nixpkgs.nix {},
  haskellPackages ? nixpkgs.pkgs.haskellPackages
}:
let
  inherit (nixpkgs) pkgs;
in
pkgs.haskell.lib.doBenchmark
  ( haskellPackages.callCabal2nix
      "implicit-effects"
      ../code
      {}
  )
