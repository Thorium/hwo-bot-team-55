#!/bin/sh
set -e

which cabal > /dev/null
if [ "$?" -ne "0" ]; then
echo "cabal missing. Please install Haskell environment."
  exit 1
fi

cabal update && cabal install --only-dependencies
cabal configure && cabal build
