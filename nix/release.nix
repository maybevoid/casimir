{
  nixpkgs ? import ./nixpkgs.nix {}
}:
let
  inherit (nixpkgs) pkgs;
in
pkgs.haskell.lib.doBenchmark
  ( pkgs.haskellPackages.callCabal2nix
      "implicit-effects"
      ../code
      {}
  )
