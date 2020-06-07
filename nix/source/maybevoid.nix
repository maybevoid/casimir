{ useLocal }:
let
  local-src = ../../../nix;

  remote-src = builtins.fetchGit {
    url = "https://github.com/maybevoid/maybevoid-nix.git";
    rev = "aaed468bfeaf26e5160c333999e44a8810245625";
  };
in
if useLocal then local-src else remote-src
