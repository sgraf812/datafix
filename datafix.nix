{ mkDerivation, base, Cabal, cabal-doctest, cabal-toolkit
, containers, criterion, deepseq, directory, doctest, filepath, ghc
, ghc-paths, lattices, pomaps, primitive, QuickCheck, stdenv, tasty
, tasty-hunit, tasty-smallcheck, text, transformers, turtle, vector
}:
mkDerivation {
  pname = "datafix";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal cabal-doctest cabal-toolkit ];
  libraryHaskellDepends = [
    base containers lattices pomaps primitive transformers vector
  ];
  executableHaskellDepends = [ base containers ];
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
