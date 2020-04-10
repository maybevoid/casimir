{ useLocal ? false
, nixpkgs ? import ./nixpkgs.nix {inherit useLocal; }
}:
let
  release = import ./default.nix
    { inherit useLocal nixpkgs; };
in
{ ghc86 = release.ghc86.build;
  ghc88 = release.ghc88.build;
}
