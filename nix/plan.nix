args @
{ nixpkgs ? import ./modules/nixpkgs.nix
, ...
}:
let
  release = nixpkgs.callPackage ./release.nix args;

  versions = [ "ghc86" "ghc88" "ghc810" ];

  copyDirs = nixpkgs.lib.strings.concatMapStrings
    (version:
      let
        inherit (release.${version}) plan ghc-version;
      in
      "cp -r \"${plan}\" \"$out/${ghc-version}\" \n"
    )
    versions
  ;
in
nixpkgs.stdenv.mkDerivation {
  name = "project-plans";

  unpackPhase = "true";

  installPhase = ''
    mkdir -p $out
    ${copyDirs}
  '';
}
