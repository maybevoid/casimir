let
  nixpkgs-src = import ../source/nixpkgs.nix;
in
import nixpkgs-src {}
