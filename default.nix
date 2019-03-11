{ mkDerivation, base, comonad, free, hpack, mtl
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
    base comonad free mtl natural-transformation stm transformers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base comonad free mtl natural-transformation stm transformers
  ];
  testHaskellDepends = [
    base comonad free mtl natural-transformation QuickCheck stm tasty
    tasty-hunit tasty-quickcheck transformers
  ];
  preConfigure = "hpack";
  description = "Demo effects";
  license = stdenv.lib.licenses.bsd3;
}
