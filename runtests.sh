#!/bin/sh

set -e

tmp=/tmp/runtests-$$.txt

cd tests

function fail() {
#	echo "Test failed.  Quitting."
	rm $tmp
	exit 1
}


for i in test*.dk fail*.dk; do
	echo "$i"

	if [[ ! -f ${i%.dk}.txt ]]; then
		echo "Skipping $i: No results file."
		continue
	fi

	failed=succeeded
	../udeck.pl $i > $tmp 2>&1 || failed=failed

	expectedFail=succeeded
	if [[ ${i:0:4} = "fail" ]]; then
		expectedFail=failed
	fi

	if [[ $failed != $expectedFail ]]; then
		echo "$i $failed unexpectedly."
		fail
	fi

	if diff $tmp ${i%.dk}.txt; then
		true
	else
		fail
	fi
done







