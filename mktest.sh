#!/bin/bash

# Given a test, run it and store its output in a matching .tst file.

set -e

test=$1
result=${test%.dk}.txt

if [ ! -f "$test" ]; then
	echo "Unable to open '$test'"
	exit 1
fi

function failed() {
	echo -n "Test '$test' failed"

	bn=`basename $test`
	if [ ${bn:0:4} = "fail" ]; then
		echo -n " (as expected)"
	fi
	echo ".  Files created."
}

./udeck.pl $test > $result 2>&1 || failed

