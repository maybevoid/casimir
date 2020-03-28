{ nixpkgs ? import ./nixpkgs.nix {} }:
let
  ghc865 = (import ./release/8.6.5.nix { inherit nixpkgs; }).release;
  ghc883 = (import ./release/8.8.3.nix { inherit nixpkgs; }).release;
in
{
  inherit ghc865 ghc883;
}
