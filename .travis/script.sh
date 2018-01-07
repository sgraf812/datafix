set -ex

echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"

if [ -f configure.ac ]; then 
  autoreconf -i
fi

case "$BUILD" in
  style)
    # This should be resolved (and removed) by Stack 1.6
    # Follows from https://github.com/commercialhaskell/stack/issues/3178
    stack --system-ghc --no-terminal install happy
    
    stack --system-ghc --no-terminal install hlint
    ;;
  stack)
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
