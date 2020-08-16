{ nixpkgs ? import ./modules/nixpkgs.nix
, checkMaterialization ? false
}:
let
  callRelease = ghc-version:
    import ./modules/release.nix
      { inherit ghc-version checkMaterialization;
      };

  ghc86 = callRelease "ghc865";
  ghc88 = callRelease "ghc884";
  ghc810 = callRelease "ghc8102";
in
{ inherit ghc86 ghc88 ghc810;
}
