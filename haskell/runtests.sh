#! /bin/sh

#runghc -isrc:testsuite -fbreak-on-error Tests

cabal test
cat dist/test/*.log
