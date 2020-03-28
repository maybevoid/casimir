{ nixpkgs ? import ../nixpkgs.nix {} }:
let
  inherit (nixpkgs) pkgs;

  haskellPackages-1 = pkgs.haskell.packages.ghc8101;

  haskellPackages = haskellPackages-1.override (old: {
    overrides = pkgs.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: {
      });
    });

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
