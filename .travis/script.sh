#!/bin/bash

echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"

if [ -f configure.ac ]; then 
  autoreconf -i
fi

set -ex

# From https://github.com/travis-ci/travis-build/blob/af4b84614590336a137310a1b82688f3d32a03e4/lib/travis/build/templates/header.sh
# Couldn't figure out how to make this visible in the sub script
travis_retry() {
  local result=0
  local count=1
  while [ $count -le 3 ]; do
    [ $result -ne 0 ] && {
      echo -e "\n${ANSI_RED}The command \"$@\" failed. Retrying, $count of 3.${ANSI_RESET}\n" >&2
    }
    "$@" && { result=0 && break; } || result=$?
    count=$(($count + 1))
    sleep 1
  done
  [ $count -gt 3 ] && {
    echo -e "\n${ANSI_RED}The command \"$@\" failed 3 times.${ANSI_RESET}\n" >&2
  }
  return $result
}

case "$BUILD" in
  style)
    ;;
  stack)
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
