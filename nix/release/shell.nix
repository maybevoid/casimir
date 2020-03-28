{ nixpkgs,
  release,
  haskellPackages
}:
let
  inherit (nixpkgs) pkgs;
in
pkgs.mkShell {
  name = "shell";
  LANG = "en_US.UTF-8";
  inputsFrom = [ release.env ];
  buildInputs = [
    pkgs.glibcLocales
    haskellPackages.ghcid
    haskellPackages.cabal-install
  ];
}
