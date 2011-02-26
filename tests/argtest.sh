#!/bin/sh

# Test arg. passing.

set -e

../udeck.pl --dump argtest.dk 
echo "----------"

../udeck.pl argtest.dk --dump
echo "----------"

../udeck.pl argtest.dk foo.dk asdf blort --dumpxxx --yyy -x
echo "----------"


