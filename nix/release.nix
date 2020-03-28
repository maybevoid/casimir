{ nixpkgs ? import ./nixpkgs.nix {} }:
let
  ghc86 = (import ./release/8.6.nix { inherit nixpkgs; }).release;
  ghc88 = (import ./release/8.8.nix { inherit nixpkgs; }).release;
in
{
  inherit ghc86 ghc88;
}
