{ useLocal ? false
, nixpkgs ? import ./nixpkgs.nix {inherit useLocal; }
}:
let
  release = import ./default.nix
    { inherit useLocal nixpkgs; };
in
{ ghc86 = release.ghc86.shell;
  ghc88 = release.ghc88.shell;
  ghc810= release.ghc810.shell;
}
