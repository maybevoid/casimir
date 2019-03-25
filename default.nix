{ mkDerivation, async, base, comonad, criterion, free, hpack, mtl
, natural-transformation, QuickCheck, stdenv, stm, tasty
, tasty-hunit, tasty-quickcheck, transformers
}:
mkDerivation {
  pname = "implicit-effects";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base comonad free mtl natural-transformation stm transformers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    async base comonad criterion free mtl natural-transformation stm
    transformers
  ];
  testHaskellDepends = [
    async base comonad free mtl natural-transformation QuickCheck stm
    tasty tasty-hunit tasty-quickcheck transformers
  ];
  preConfigure = "hpack";
  description = "Demo effects";
  license = stdenv.lib.licenses.bsd3;
}
