{ mkDerivation, ansi-wl-pprint, base, bytestring, process
, QuickCheck, stdenv, transformers, transformers-compat
}:
mkDerivation {
  pname = "optparse-applicative";
  version = "0.16.0.0";
  sha256 = "09c53c339c41167701343afaeccd5993c6bd827290a2b30053f2a7375555cb2b";
  libraryHaskellDepends = [
    ansi-wl-pprint base process transformers transformers-compat
  ];
  testHaskellDepends = [ base bytestring QuickCheck ];
  homepage = "https://github.com/pcapriotti/optparse-applicative";
  description = "Utilities and combinators for parsing command line options";
  license = stdenv.lib.licenses.bsd3;
}
