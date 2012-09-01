#!/bin/sh
which cabal > /dev/null
if [ "$?" -ne "0" ]; then
echo "cabal missing. Please install Haskell environment."
  exit 1
fi

cabal configure && cabal build
