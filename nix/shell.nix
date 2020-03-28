{ nixpkgs ? import ./nixpkgs.nix {} }:
let
  ghc86 = (import ./release/8.6.nix { inherit nixpkgs; }).shell;
  ghc88 = (import ./release/8.8.nix { inherit nixpkgs; }).shell;
  ghc810 = (import ./release/8.10.nix { inherit nixpkgs; }).shell;
in
{
  inherit ghc86 ghc88 ghc810;

  default = ghc88;
}
