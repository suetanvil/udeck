#!/bin/bash

# This script generates all final .pod documents and then produces
# html from them.

set -e

# Recreate the libref from docstrings *unless* print_docstrings.dk
# fails.
if ../udeck.pl ../progs/utils/print_docstrings.dk > libref-body.pod; then
	cat libref-head.pod libref-body.pod > libref.pod
	rm libref-body.pod
else
	echo "Error creating libref-body.pod.  Using old version."
fi

pod2html --css format.css --no-index \
--title "Deck Library Reference" libref.pod > libref.html

pod2html --css format.css --noindex intro.pod > intro.html

pod2html --css format.css --noindex manual.pod > manual.html


