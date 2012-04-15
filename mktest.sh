#!/bin/bash

# Given a test, run it and store its output in a matching .txt file.

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

./udeck.pl --flush $test > $result 2>&1 || failed

bn=`basename $test`
if [[ ${bn:0:4} = "fail" ]]; then
	echo "Emitting backtrace version..."
	result2=${result%.txt}.bt.txt
	./udeck.pl --flush --backtrace $test > $result2 2>&1 || failed

	if diff $result $result2 > /dev/null 2>&1 ; then
		echo "Results identical.  Deleting $result2"
		rm $result2
	fi
fi



