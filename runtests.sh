#!/bin/sh

set -e

tmp=/tmp/runtests-$$.txt

cd tests

function fail() {
#	echo "Test failed.  Quitting."
	rm $tmp
	exit 1
}


for i in  argtest.sh test*.dk fail*.dk; do
	echo "$i"

	if [ $i = ${i%.sh}.sh ]; then
		cmp=${i%.sh}.txt
		cmd=bash
	else
		cmp=${i%.dk}.txt
		cmd=../udeck.pl
	fi

	if [[ ! -f $cmp ]]; then
		echo "Skipping $i: No results file."
		continue
	fi

	failed=succeeded
	$cmd $i > $tmp 2>&1 || failed=failed

	expectedFail=succeeded
	if [[ ${i:0:4} = "fail" ]]; then
		expectedFail=failed
	fi

	if [[ $failed != $expectedFail ]]; then
		echo "$i $failed unexpectedly."
		fail
	fi

	if diff $tmp $cmp; then
		true
	else
		fail
	fi
done







