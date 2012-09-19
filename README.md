# Pingpong Haskell client

This project contains a client for the [Reaktor Hello World Open event](http://helloworldopen.fi/).

NOTICE: The test server enforces a threshold of 10 messages per client in a second. At the moment the bot answers each message from the server with up direction message. This exceeds the threshold defined by the server and kicks the bot out of the game.

## Requires

[cabal](http://www.haskell.org/cabal/)

You must run `cabal update && cabal install --only-dependencies` at least once before building the bot.

## Usage

to build:
`./build.sh`

to run:
`./start.sh <bot-name> <host> <port>`

to stop
`./stop.sh`

## Credits

Bot Copyright (C) 2012 Tuomas Hietanen

Template from https://github.com/helloworldopen ,
Template Copyright (C) 2012 Tuomas Järvensivu, Juha Paananen, Timo Rantalaiho, Sami Rosendahl, Jussi Vesala

Distributed under the Apache-2.0 license http://www.apache.org/licenses/LICENSE-2.0.html

## Comments

Some specifications pictures in the specs-folder.

This is my first Haskell program. Nice language. I prefer OCaml pattern matching syntax so far... ;-)
