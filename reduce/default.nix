{ mkDerivation, base, containers, directory, filepath
, generic-random, hpack, hspec-expectations-pretty-diff, mtl
, QuickCheck, stdenv, tasty, tasty-discover, tasty-hspec
, tasty-hunit, tasty-quickcheck, text, transformers, vector
}:
mkDerivation {
  pname = "reduce";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers mtl transformers vector
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base containers directory filepath generic-random
    hspec-expectations-pretty-diff mtl QuickCheck tasty tasty-discover
    tasty-hspec tasty-hunit tasty-quickcheck text transformers vector
  ];
  testToolDepends = [ tasty-discover ];
  preConfigure = "hpack";
  homepage = "https://github.com/ucla-pls/reduce#readme";
  license = stdenv.lib.licenses.bsd3;
}
