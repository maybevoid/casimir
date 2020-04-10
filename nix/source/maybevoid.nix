{ useLocal }:
let
  local-src = ../../../nix;

  remote-src = builtins.fetchGit {
    url = "https://github.com/maybevoid/maybevoid-nix.git";
    rev = "8570adce981515ecbee6c684c5bca8e18fc1d97e";
  };
in
if useLocal then local-src else remote-src
