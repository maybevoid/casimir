{ mkDerivation, async, base, criterion, free, mtl, QuickCheck
, stdenv, tasty, tasty-hunit, tasty-quickcheck, transformers
}:
mkDerivation {
  pname = "implicit-effects";
  version = "0.1.0";
  src = ../code;
  isLibrary = true;
  doBenchmark = true;
  isExecutable = true;
  libraryHaskellDepends = [ async base free mtl transformers ];
  executableHaskellDepends = [ async base free mtl transformers ];
  testHaskellDepends = [
    async base free mtl QuickCheck tasty tasty-hunit tasty-quickcheck
    transformers
  ];
  benchmarkHaskellDepends = [
    async base criterion free mtl transformers
  ];
  homepage = "https://github.com/maybevoid/implicit-effects";
  description = "Algebraic Effects in Haskell using Implicit Parameters";
  license = stdenv.lib.licenses.bsd3;
}
