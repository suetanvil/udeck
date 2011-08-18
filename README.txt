
							  Deck 0.01
							  =========

Deck is a Lispish programming language.  The current implementation is
written in Perl.  It is a work in progress.


Requirements
============

Deck requires a fairly recent version of Perl and a Unixish operating
environment.  I used perl 5.10.0 on Fedora Linux.  Cygwin users should
be careful to ensure that input files do not have DOS-style line
endings.

Note that I have not tested Deck on any platform other than my own.


Running
=======

To run Deck, just go to the udeck directory and type './udeck.pl' (or
'perl udeck.pl').  To run a Deck program, add it to the command line:
'./udeck.pl foo.dk'

Installation
============

Deck isn't really installable at this point, but...

	1) Copy udeck.pl and lib-deck to your installation directory.

	2) Create a shell script to launch udeck.pl with an absolute path
       (udeck uses its location to find the lib-deck directory).

	3) Put the script somewhere in your path.

You can also point udeck to the location of lib-deck by setting the
environment variable DECKLIB to the path.  DECKLIB allows multiple
paths separated by colons.


Command-line Options
====================

udeck.pl can take two arguments.  Both need to appear before the
program being run (if any).  They are:

	--dump-expr		-- Print the expansion(s) of all expressions being
                       evaluated.

	--no-lib        -- Do not load the default system library (Lang)
                       on startup.

Both of these are only useful for debugging udeck.


Testing
=======

The directory 'tests' contains a set of regression tests.  The shell
script 'runtests.sh' executes them and compares the results with the
expected results.  This is done with a simple 'diff' against a file
containing the results of a previous run.


Documentation
=============

The directory 'docs' contains the manuals, notably:

	intro.pod	-- An introduction/tutorial.

	manual.pod	-- The Deck language reference.

	libref.pod	-- The Deck library reference.

The script docs/make-docs.sh recreates those parts of the manual that
were machine-generated and produces HTML manuals from the PODs.

The directory 'progs' contains a number of Deck programs of varying
quality.  They may be useful as examples.











