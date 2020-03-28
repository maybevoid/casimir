{  nixpkgs ? import ../nixpkgs.nix {} }:
let
  haskellPackages = nixpkgs.pkgs.haskell.packages.ghc883;

  release = import ./release.nix {
    inherit nixpkgs haskellPackages;
  };

  shell = import ./shell.nix {
    inherit nixpkgs release haskellPackages;
  };
in
{
  inherit shell release haskellPackages;
}
