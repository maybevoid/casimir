{ haskell-nix
, project-src
, index-state
, ghc-version
, checkMaterialization ? false
, plan-hash ? null
, materialized ? null
}:
haskell-nix.pkgs.haskell-nix.cabalProject
  { inherit
      index-state
      checkMaterialization
      materialized
    ;

    src = project-src;
    plan-sha256 = plan-hash;
    compiler-nix-name = ghc-version;
  }
