
                              Deck 0.03
                              =========

Deck is a simple programming language.  It is dynamically typed,
object-oriented, functional and provides macros.  The current
implementation is written in Perl.  It is a work in progress.

License and Disclaimer
======================

Deck is licensed under the GNU GPL with an exception and is provided
without any warranty.  See the file 'docs/Copyright.txt' for details.


Requirements
============

Deck requires a fairly recent version of Perl and a Unixish operating
environment.  I used perl 5.10.0 on Fedora and Ubuntu Linux.  Cygwin
users should be careful to ensure that input files do not have
DOS-style line endings.

Note that I have not tested Deck on any platform other than my own.


Running
=======

To run Deck, just go to the udeck directory and type './udeck.pl' (or
'perl udeck.pl').  To run a Deck program, add it to the command line:
'./udeck.pl foo.dk'


Installation
============

Deck isn't really useful at this point, but...

    1) Copy udeck.pl and lib-deck to your installation directory.

    2) Create a shell script to launch udeck.pl with an absolute path
       (udeck uses its location to find the lib-deck directory).

    3) Put the script somewhere in your path.

You can also point udeck to the location of lib-deck by setting the
environment variable DECKLIB to the path.  DECKLIB allows multiple
paths separated by colons.


Command-line Options
====================

udeck.pl can take a number of arguments.  All need to appear before
the program being run (if any).  They are:

    --backtrace     -- If given, errors will (often) print out a
                       backtrace, possibly with some performance
                       overhead.

    --dump-expr     -- Print the expansion(s) of all expressions being
                       evaluated.

    --show-lib-perl -- Print out the Perl code each expression is
                       compiled to.

    --show-perl     -- Print out the Perl code each expression is
                       compiled to EXCLUDING library modules.

In addition, the following arguments are available, at least for now.
They are mostly intended to aid the development of udeck.pl and are
likely to change.  Nonetheless, for completeness, the are:

    --no-lib        -- Do not load the default system library (Lang)
                       on startup.  Deck will probably not work with
                       this set anymore.

    --flush         -- Disable output buffering.  (Sets $| to 1).

    --xxxxx         -- Is silently ignored.  This is only ever used in
                       a test case.


Testing
=======

The directory 'tests' contains a set of regression tests.  The Perl
script alltests.pl executes them and compares the results with the
expected results.  This is done with a simple comparison against a
file containing the results of a previous run.


Documentation
=============

The directory 'docs' contains the manuals, notably:

    intro.pod   -- An introduction/tutorial.

    manual.pod  -- The Deck language reference.

    libref.pod  -- The Deck library reference.

The script docs/make-docs.sh recreates those parts of the manual that
were machine-generated and produces HTML manuals from the PODs.

The directory 'progs' contains a number of Deck programs of varying
quality.  They may be useful as examples.











