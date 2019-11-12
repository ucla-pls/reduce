{ mkDerivation, aeson, async, base, binary, bytestring, cassava
, containers, contravariant, cryptohash-sha256, data-fix, deepseq
, directory, dirtree, filepath, free, hashable, hashtables, hpack
, hspec, lens, megaparsec, mtl, optparse-applicative, process
, profunctors, reduce, stdenv, stm, temporary, text, time
, transformers, typed-process, unliftio, unordered-containers
, vector
}:
mkDerivation {
  pname = "reduce-util";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base binary bytestring cassava containers contravariant
    cryptohash-sha256 data-fix deepseq directory dirtree filepath free
    hashable hashtables lens megaparsec mtl optparse-applicative
    process profunctors reduce stm temporary text time transformers
    typed-process unliftio unordered-containers vector
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson async base binary bytestring cassava containers contravariant
    cryptohash-sha256 data-fix deepseq directory dirtree filepath free
    hashable hashtables hspec lens megaparsec mtl optparse-applicative
    process profunctors reduce stm temporary text time transformers
    typed-process unliftio unordered-containers vector
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/kalhauge/reduce#readme";
  license = stdenv.lib.licenses.bsd3;
}
