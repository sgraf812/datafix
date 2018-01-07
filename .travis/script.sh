#!/bin/bash

echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"

if [ -f configure.ac ]; then 
  autoreconf -i
fi

set -ex

case "$BUILD" in
  style)
    ;;
  stack)
    # Add in extra-deps for older snapshots, as necessary
    stack --no-terminal --install-ghc $ARGS test --bench --dry-run || ( \
      stack --no-terminal $ARGS build cabal-install && \
      stack --no-terminal $ARGS solver --update-config)

    # Build the dependencies
    stack --no-terminal --install-ghc -j1 build $ARGS Cabal
    stack --no-terminal --install-ghc test --bench --only-dependencies $ARGS
    ;;
  cabal)
    cabal --version
    travis_retry cabal update

    # Get the list of packages from the stack.yaml file
    # But ignore packages in .stack-work (such as extra-deps)
    PACKAGES=$(stack --install-ghc query locals | grep '^ *path' | sed 's@^ *path:@@' | xargs -d "\n" -I{} bash -c "! [[ '{}' =~ \\.stack-work ]] && echo '{}'" | tr -d '[:space:]')

    cabal install --only-dependencies --enable-tests --enable-benchmarks --force-reinstalls --ghc-options=-O0 --reorder-goals --max-backjumps=-1 $CABALARGS $PACKAGES
    ;;
esac
