#!/bin/bash

set -ex

case "$BUILD" in
  style)
    curl -sL https://raw.github.com/ndmitchell/hlint/master/misc/travis.sh | sh -s src/ tests/ bench/ examples/
    stack --system-ghc --no-terminal build --test --bench --no-run-tests --no-run-benchmarks --pedantic
    cabal check
    ;;
  stack)
    stack --no-terminal test --bench --no-run-benchmarks --haddock --no-haddock-deps --haddock-hyperlink-source $ARGS
    ;;
  cabal)
    cabal install --enable-tests --enable-benchmarks --force-reinstalls --ghc-options=-O0 --reorder-goals --max-backjumps=-1 $CABALARGS $PACKAGES

    ORIGDIR=$(pwd)
    for dir in $PACKAGES
    do
      cd $dir
      cabal check || [ "$CABALVER" == "1.16" ]
      cabal sdist
      PKGVER=$(cabal info . | awk '{print $2;exit}')
      SRC_TGZ=$PKGVER.tar.gz
      cd dist
      tar zxfv "$SRC_TGZ"
      cd "$PKGVER"
      cabal configure --enable-tests --ghc-options=-O0
      cabal build --ghc-options=-O0
      cabal test --ghc-options=-O0
      cd $ORIGDIR
    done
    ;;
esac
