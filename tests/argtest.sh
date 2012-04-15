#!/bin/sh

# Test arg. passing.

set -e

../udeck.pl --xxxxx argtest.dk 
echo "----------"

../udeck.pl argtest.dk --xxxxx
echo "----------"

../udeck.pl argtest.dk foo.dk asdf blort --xxxxxyyy --yyy -x
echo "----------"


