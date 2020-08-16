{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "3.0";
      identifier = { name = "casimir"; version = "0.1.0"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) Soares Ruofei Chen";
      maintainer = "Soares Chen <soares.chen@maybevoid.com>";
      author = "Soares Chen";
      homepage = "https://github.com/maybevoid/casimir";
      url = "";
      synopsis = "Algebraic Effects in Haskell using Implicit Parameters";
      description = "casimir is a experimental effect library to support\nalgebraic effects in Haskell.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."casimir-params" or (errorHandler.buildDepError "casimir-params"))
          (hsPkgs."casimir-argkind" or (errorHandler.buildDepError "casimir-argkind"))
          ];
        buildable = true;
        modules = [
          "Casimir/Base/Lift"
          "Casimir/Base/Effect"
          "Casimir/Base/EffFunctor"
          "Casimir/Base/ContraLift"
          "Casimir/Higher/Lift"
          "Casimir/Higher/Effect"
          "Casimir/Higher/EffFunctor"
          "Casimir"
          "Casimir/Base"
          "Casimir/Higher"
          "Casimir/Ops/State"
          "Casimir/Ops/State/Effect"
          "Casimir/Ops/State/StateT"
          "Casimir/Ops/State/MonadState"
          "Casimir/Ops/Env"
          "Casimir/Ops/Env/Effect"
          "Casimir/Ops/Env/ReaderT"
          "Casimir/Ops/Env/MonadReader"
          "Casimir/Ops/Resource/Higher/Effect"
          ];
        hsSourceDirs = [ "src/lib" ];
        };
      sublibs = {
        "casimir-argkind" = {
          depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
          buildable = true;
          modules = [ "Casimir/Base/ArgKind" "Casimir/Higher/ArgKind" ];
          hsSourceDirs = [ "src/argkind" ];
          };
        "casimir-params" = {
          depends = [
            (hsPkgs."casimir-argkind" or (errorHandler.buildDepError "casimir-argkind"))
            (hsPkgs."quasi-params" or (errorHandler.buildDepError "quasi-params"))
            ];
          buildable = true;
          };
        };
      tests = {
        "casimir-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
            (hsPkgs."free" or (errorHandler.buildDepError "free"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."casimir" or (errorHandler.buildDepError "casimir"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          modules = [
            "Casimir/Test/Main"
            "Casimir/Test/Basic"
            "Casimir/Test/Basic/Test1"
            ];
          hsSourceDirs = [ "src/test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././casimir; }