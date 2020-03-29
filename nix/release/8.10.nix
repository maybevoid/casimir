{ nixpkgs ? import ../nixpkgs.nix {} }:
let
  inherit (nixpkgs) pkgs stdenv;

  haskellLib = pkgs.haskell.lib;

  callPackage = self: name: src:
    self.callCabal2nix
      name
      src
      {}
  ;

  callGitPackage = self: name: url: rev:
    callPackage self
        name
        (builtins.fetchGit {
          inherit url rev;
        })
      ;

  haskellPackages-1 = pkgs.haskell.packages.ghc8101;

  haskellPackages = haskellPackages-1.override (old: {
    overrides = pkgs.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: {

        doctest = haskellLib.dontCheck
          ( super.callPackage
            ({ mkDerivation, base, base-compat, code-page, deepseq, directory
            , filepath, ghc, ghc-paths, hspec, HUnit, mockery, process
            , QuickCheck, setenv, silently, stringbuilder, syb, transformers
            }:
            mkDerivation {
              pname = "doctest";
              version = "0.16.2";
              src = builtins.fetchGit {
                url = "https://github.com/sol/doctest.git";
                rev = "39eae983aa5f2e19cd797a559b04f044715d63e8";
              };
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [
                base base-compat code-page deepseq directory filepath ghc ghc-paths
                process syb transformers
              ];
              executableHaskellDepends = [
                base base-compat code-page deepseq directory filepath ghc ghc-paths
                process syb transformers
              ];
              testHaskellDepends = [
                base base-compat code-page deepseq directory filepath ghc ghc-paths
                hspec HUnit mockery process QuickCheck setenv silently
                stringbuilder syb transformers
              ];
              description = "Test interactive Haskell examples";
              license = stdenv.lib.licenses.mit;
            }) {}
          )
        ;

        lifted-async = haskellLib.appendPatch
          ( super.callHackage
              "lifted-async"
              "0.10.0.5"
              {}
          )
          ../patches/lifted-async.patch
        ;

        # ed25519 = haskellLib.appendPatch
        #   ( super.callHackage
        #       "ed25519"
        #       "0.0.5.0"
        #       {}
        #   )
        #   ../patches/ed25519.patch
        # ;

        # resolv = haskellLib.appendPatch
        #   ( super.callHackage
        #       "resolv"
        #       "0.1.1.3"
        #       {}
        #   )
        #   ../patches/resolv.patch
        # ;

        language-haskell-extract = haskellLib.appendPatch
          ( callGitPackage super
              "language-haskell-extract"
              "https://github.com/finnsson/template-helper.git"
              "9047b3793aac904052dcb3f1704b53d6d41d2374"
          )
          ../patches/language-haskell-extract.patch
        ;
      });
    });

  release = import ./release.nix {
    inherit nixpkgs haskellPackages;
  };

  shell = import ./shell.nix {
    inherit nixpkgs release haskellPackages;
  };
in
{
  inherit shell release haskellPackages;
}
