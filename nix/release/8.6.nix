{  nixpkgs ? import ../nixpkgs.nix {} }:
let
  inherit (nixpkgs) pkgs;
  haskellLib = pkgs.haskell.lib;

  callPackage = self: name: src:
    self.callCabal2nix
      name
      src
      {}
  ;

  callGitPackage = self: name: url: rev:
    callPackage self
        name
        (builtins.fetchGit {
          inherit url rev;
        })
      ;

  haskellPackages-1 = nixpkgs.pkgs.haskell.packages.ghc865;

  haskellPackages = haskellPackages-1.override (old: {
    overrides = pkgs.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: {
        time-compat = haskellLib.appendPatch
          ( callGitPackage super
              "time-compat"
              "https://github.com/phadej/time-compat.git"
              "89ef24ecf2b9a7f30bf91ec7cc82edc71c7b29d0"
          )
          ../patches/time-compat.patch
        ;
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
