{ useLocal }:
let
  nix-src = import ./source/maybevoid.nix { inherit useLocal; };
in
import (nix-src + /release/nixpkgs.nix)
