#!/bin/sh

set -e

tmp=/tmp/runtests-$$.txt

cd tests

function fail() {
#	echo "Test failed.  Quitting."
	rm $tmp
	exit 1
}


for i in test*.dk; do
	echo "$i"
	../udeck.pl $i > $tmp
	if diff $tmp ${i%.dk}.txt; then
		true
	else
		fail
	fi
done


for i in fail*.dk; do
	echo $i
	if ../udeck.pl $i > $tmp 2>&1 ; then
		echo "$i succeeded.  Should have died."
		echo "Output:"
		cat $tmp
		fail
	fi

	if diff $tmp ${i%.dk}.txt; then
		true
	else
		fail
	fi
done





