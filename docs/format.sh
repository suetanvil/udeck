#!/bin/bash

set -e

if ../udeck.pl ../progs/utils/print_docstrings.dk > ds.tmp; then
	mv ds.tmp libref-body.pod
else
	echo "Error creating libref-body.pod.  Using old version."
fi

cat libref-head.pod libref-body.pod | \
    pod2html --css format.css --no-index \
    --title "Deck Library Reference" > libref.html

pod2html --css format.css --noindex intro.pod > intro.html
