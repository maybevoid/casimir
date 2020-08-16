{
  pkgs = hackage:
    {
      packages = {
        "semigroupoids".revision = (((hackage."semigroupoids")."5.3.4").revisions).default;
        "semigroupoids".flags.comonad = true;
        "semigroupoids".flags.doctests = true;
        "semigroupoids".flags.unordered-containers = true;
        "semigroupoids".flags.distributive = true;
        "semigroupoids".flags.tagged = true;
        "semigroupoids".flags.containers = true;
        "semigroupoids".flags.contravariant = true;
        "free".revision = (((hackage."free")."5.1.3").revisions).default;
        "exceptions".revision = (((hackage."exceptions")."0.10.4").revisions).default;
        "exceptions".flags.transformers-0-4 = true;
        "binary".revision = (((hackage."binary")."0.8.7.0").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.3").revisions).default;
        "bifunctors".revision = (((hackage."bifunctors")."5.5.7").revisions).default;
        "bifunctors".flags.semigroups = true;
        "bifunctors".flags.tagged = true;
        "stm".revision = (((hackage."stm")."2.5.0.0").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "tasty-quickcheck".revision = (((hackage."tasty-quickcheck")."0.10.1.1").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "clock".revision = (((hackage."clock")."0.8").revisions).default;
        "clock".flags.llvm = false;
        "distributive".revision = (((hackage."distributive")."0.6.2").revisions).default;
        "distributive".flags.semigroups = true;
        "distributive".flags.tagged = true;
        "QuickCheck".revision = (((hackage."QuickCheck")."2.14.1").revisions).default;
        "QuickCheck".flags.templatehaskell = true;
        "QuickCheck".flags.old-random = false;
        "tasty".revision = (((hackage."tasty")."1.3.1").revisions).default;
        "tasty".flags.clock = true;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "random".revision = (((hackage."random")."1.2.0").revisions).default;
        "optparse-applicative".revision = (((hackage."optparse-applicative")."0.15.1.0").revisions).default;
        "splitmix".revision = (((hackage."splitmix")."0.1.0.1").revisions).default;
        "splitmix".flags.optimised-mixer = false;
        "async".revision = (((hackage."async")."2.2.2").revisions).default;
        "async".flags.bench = false;
        "constraints".revision = (((hackage."constraints")."0.12").revisions).default;
        "semigroups".revision = (((hackage."semigroups")."0.19.1").revisions).default;
        "semigroups".flags.bytestring = true;
        "semigroups".flags.unordered-containers = true;
        "semigroups".flags.text = true;
        "semigroups".flags.tagged = true;
        "semigroups".flags.containers = true;
        "semigroups".flags.binary = true;
        "semigroups".flags.hashable = true;
        "semigroups".flags.transformers = true;
        "semigroups".flags.deepseq = true;
        "semigroups".flags.bytestring-builder = false;
        "semigroups".flags.template-haskell = true;
        "parsec".revision = (((hackage."parsec")."3.1.14.0").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.6.0").revisions).default;
        "transformers-compat".revision = (((hackage."transformers-compat")."0.6.5").revisions).default;
        "transformers-compat".flags.five = false;
        "transformers-compat".flags.generic-deriving = true;
        "transformers-compat".flags.two = false;
        "transformers-compat".flags.five-three = true;
        "transformers-compat".flags.mtl = true;
        "transformers-compat".flags.four = false;
        "transformers-compat".flags.three = false;
        "template-haskell".revision = (((hackage."template-haskell")."2.15.0.0").revisions).default;
        "call-stack".revision = (((hackage."call-stack")."0.2.0").revisions).default;
        "profunctors".revision = (((hackage."profunctors")."5.5.2").revisions).default;
        "ansi-terminal".revision = (((hackage."ansi-terminal")."0.10.3").revisions).default;
        "ansi-terminal".flags.example = false;
        "tagged".revision = (((hackage."tagged")."0.8.6").revisions).default;
        "tagged".flags.transformers = true;
        "tagged".flags.deepseq = true;
        "containers".revision = (((hackage."containers")."0.6.2.1").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.10.10.1").revisions).default;
        "ansi-wl-pprint".revision = (((hackage."ansi-wl-pprint")."0.6.9").revisions).default;
        "ansi-wl-pprint".flags.example = false;
        "wcwidth".revision = (((hackage."wcwidth")."0.0.2").revisions).default;
        "wcwidth".flags.split-base = true;
        "wcwidth".flags.cli = false;
        "StateVar".revision = (((hackage."StateVar")."1.2").revisions).default;
        "contravariant".revision = (((hackage."contravariant")."1.5.2").revisions).default;
        "contravariant".flags.semigroups = true;
        "contravariant".flags.tagged = true;
        "contravariant".flags.statevar = true;
        "type-equality".revision = (((hackage."type-equality")."1").revisions).default;
        "text".revision = (((hackage."text")."1.2.4.0").revisions).default;
        "Cabal".revision = (((hackage."Cabal")."3.0.1.0").revisions).default;
        "unordered-containers".revision = (((hackage."unordered-containers")."0.2.12.0").revisions).default;
        "unordered-containers".flags.debug = false;
        "base".revision = (((hackage."base")."4.13.0.0").revisions).default;
        "comonad".revision = (((hackage."comonad")."5.0.6").revisions).default;
        "comonad".flags.distributive = true;
        "comonad".flags.test-doctests = true;
        "comonad".flags.containers = true;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "tasty-hunit".revision = (((hackage."tasty-hunit")."0.10.0.2").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "hashable".revision = (((hackage."hashable")."1.3.0.0").revisions).default;
        "hashable".flags.sse2 = true;
        "hashable".flags.integer-gmp = true;
        "hashable".flags.sse41 = false;
        "hashable".flags.examples = false;
        "colour".revision = (((hackage."colour")."2.3.5").revisions).default;
        "transformers-base".revision = (((hackage."transformers-base")."0.4.5.2").revisions).default;
        "transformers-base".flags.orphaninstances = true;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "unbounded-delays".revision = (((hackage."unbounded-delays")."0.1.1.0").revisions).default;
        "monad-control".revision = (((hackage."monad-control")."1.0.2.3").revisions).default;
        "process".revision = (((hackage."process")."1.6.9.0").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "cabal-doctest".revision = (((hackage."cabal-doctest")."1.0.8").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.8.4").revisions).default;
        "base-orphans".revision = (((hackage."base-orphans")."0.8.2").revisions).default;
        "th-abstraction".revision = (((hackage."th-abstraction")."0.3.2.0").revisions).default;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        };
      compiler = {
        version = "8.8.4";
        nix-name = "ghc884";
        packages = {
          "binary" = "0.8.7.0";
          "ghc-prim" = "0.5.3";
          "stm" = "2.5.0.0";
          "unix" = "2.7.2.2";
          "mtl" = "2.2.2";
          "rts" = "1.0";
          "deepseq" = "1.4.4.0";
          "parsec" = "3.1.14.0";
          "directory" = "1.3.6.0";
          "template-haskell" = "2.15.0.0";
          "containers" = "0.6.2.1";
          "bytestring" = "0.10.10.1";
          "text" = "1.2.4.0";
          "Cabal" = "3.0.1.0";
          "base" = "4.13.0.0";
          "time" = "1.9.3";
          "transformers" = "0.5.6.2";
          "filepath" = "1.4.2.1";
          "process" = "1.6.9.0";
          "pretty" = "1.1.3.6";
          "ghc-boot-th" = "8.8.4";
          "array" = "0.5.4.0";
          "integer-gmp" = "1.0.2.0";
          };
        };
      };
  extras = hackage:
    {
      packages = {
        quasi-params = ./.plan.nix/quasi-params.nix;
        casimir = ./.plan.nix/casimir.nix;
        };
      };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "quasi-params" = { flags = {}; };
          "casimir" = { flags = {}; };
          };
        })
    ];
  }