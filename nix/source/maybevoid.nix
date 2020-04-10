{ useLocal }:
let
  local-src = ../../../nix;

  remote-src = builtins.fetchGit {
    url = "https://github.com/maybevoid/maybevoid-nix.git";
    rev = "645bed7bd8fff7112dbadd2e84bd732d2d190c27";
  };
in
if useLocal then local-src else remote-src
