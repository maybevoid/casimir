{ nixpkgs
, haskell-nix
, project-src
, index-state
, ghc-version
}:
let
  project = nixpkgs.callPackage ./project.nix
    { inherit
        haskell-nix
        project-src
        ghc-version
        index-state
      ;
    };

  plan = project.plan-nix;
in
nixpkgs.stdenv.mkDerivation {
  name = "project-plan";

  unpackPhase = "true";

  buildInputs = [ nixpkgs.nix ];

  installPhase = ''
    mkdir -p $out

    cp -r ${plan} $out/plan

    nix-hash --base32 --type sha256 $out/plan/ > $out/plan-hash.txt
  '';
}
