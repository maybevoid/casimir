{ useLocal }:
let
  local-src = ../../../maybevoid-nix;

  remote-src = builtins.fetchGit {
    url = "https://github.com/maybevoid/maybevoid-nix.git";
    rev = "20d01e3f789cbe77120920f19a18cd9323fca660";
  };
in
if useLocal then local-src else remote-src
