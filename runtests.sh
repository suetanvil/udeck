#!/bin/sh

# Run all regression tests in udeck/tests/.  If a test name is given
# as an argument, all the preceding tests are skipped.

set -e

tmp=/tmp/runtests-$$.txt

cd tests

function fail() {
	rm $tmp
	exit 1
}


upto=$1
if [ -n "$upto" ]; then
	upto=`basename "$upto"`
	if [ -f "$upto" ]; then
		true
	else
		echo "Invalid argument: '$upto'"
		exit 1
	fi
fi


for i in test*.dk argtest.sh fail*.dk; do
	
	if [ -n "$upto" -a "$upto" != "$i" ]; then
		echo "Skipping $i"
		continue
	fi

	if [ -n "$upto" -a "$upto" = "$i" ]; then
		upto=
	fi

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







