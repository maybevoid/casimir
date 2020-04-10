{ useLocal ? false
, nixpkgs ? import ./nixpkgs.nix {inherit useLocal; }
}:
let
  inherit (nixpkgs) pkgs;

  nix-src = import ./source/maybevoid.nix { inherit useLocal; };

  sourceOverride = {
    casimir = import ./source/casimir.nix;
  };

  allRelease = import (nix-src + /release/all.nix)
    { inherit useLocal nixpkgs sourceOverride; };

  mapRelease = name: release:
    let
      build = release.builds.casimir;
      shell = release.shells.casimir;
      manual-shell = release.shells.manual;
    in
    { inherit build shell manual-shell; }
  ;

  release = pkgs.lib.mapAttrs mapRelease allRelease;
in
release
