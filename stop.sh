#!/bin/sh
kill `ps -ef | awk '/[h]wohaskellbot/{ print $2 }'`
