{ mkDerivation, base, Cabal, cabal-doctest, cabal-toolkit
, containers, criterion, deepseq, directory, doctest, filepath, ghc
, ghc-paths, lattices, pomaps, primitive, QuickCheck, stdenv, tasty
, tasty-hunit, tasty-smallcheck, text, transformers, turtle, vector
}:
mkDerivation {
  pname = "datafix";
  version = "0.0.1.0";
  sha256 = "1rp3lwrqd8ghmjbqk22sb4mfhl13swm3vij28l5ygj2f3sb8x2zi";
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal cabal-doctest cabal-toolkit ];
  libraryHaskellDepends = [
    base containers lattices pomaps primitive transformers vector
  ];
  executableHaskellDepends = [
    base Cabal cabal-toolkit containers criterion deepseq directory
    filepath ghc ghc-paths lattices primitive text transformers turtle
  ];
  testHaskellDepends = [
    base Cabal cabal-toolkit containers directory doctest filepath ghc
    ghc-paths lattices primitive QuickCheck tasty tasty-hunit
    tasty-smallcheck text transformers turtle
  ];
  benchmarkHaskellDepends = [
    base Cabal cabal-toolkit containers criterion deepseq directory
    filepath ghc ghc-paths lattices primitive text transformers turtle
  ];
  homepage = "https://github.com/sgraf812/datafix";
  description = "Fixing data-flow problems";
  license = stdenv.lib.licenses.isc;
}
