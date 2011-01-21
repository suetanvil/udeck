#!/usr/bin/perl

# Interpreter for a minimal implementation of Deck


use strict;
use warnings;

# ---------------------------------------------------------------------------

package LL::Object;

sub new {
  my ($class, $ref, $xxx) = @_;

  # Test to catch the case where you pass an array instead of its
  # reference
  die "Extra argument to 'new'\n" if $xxx;

  die "Attempted to instantiate abstract class 'LL::Stringlike'\n"
	if ($class eq 'LL::Stringlike');

  # Turn $ref into a reference if it isn't one
  if (!ref($ref)) {
	my $v = $ref;
	$ref = \$v;
  }

  return bless $ref, $class;
}

sub checkType {
  my ($self, $type, $name) = @_;

  my $fname = "";
  $fname = " in '$name'" if $name;

  $self->isa("LL::$type")
	or die "Expected 'LL::$type'; got @{[ref($self)]}$fname.\n";
}

sub checkNumber {die "Expected number, got @{[ref(shift)]}@_\n"}
sub checkString {die "Expected string, got @{[ref(shift)]}@_\n"}
sub checkList   {die "Expected list, got @{[ref(shift)]}@_\n"}
sub checkSymbol {die "Expected symbol, got @{[ref(shift)]}@_\n"}
sub checkQuote  {die "Expected quoted expr, got @{[ref(shift)]}@_\n"}
sub checkLoL    {die "Expected quoted LoL, got @{[ref(shift)]}@_\n"}
sub isAtom {return 0}
sub isSymbol {return 0}
sub isStringlike {0}
sub isOperator {return 0}
sub isUnescapedOperator {return 0}
sub isEscapedOperator {return 0}
sub isEol {return 0}
sub isExplicitEol {return 0}
sub isParen {return 0}
sub isRoundParen {return 0}
sub isLiteral {return 0}
sub isEmptyList {return 0}
sub isList {return 0}
sub isInfixList {return 0}
sub isQuote {return 0}
sub isNil {return 0}
sub isMacro {return 0}
sub isFunction {return 0}
sub isCallable {return 0}
sub isTrue {return 1}
sub isNumber {return 0}
sub isLoL {return 0}	# Is a quoted list containing only lists
sub matchesOpen {return 0}
sub storeStr {my ($self) = @_; return "${$self}"}
sub printStr {my ($self) = @_; return $self->storeStr()};

sub equals {
  my ($self, $other) = @_;
  return LL::Main::boolObj(ref($self) eq ref($other) &&
						   $self->inTypeEq($other));
}
sub inTypeEq {my ($self, $other) = @_; return $self == $other }

sub at {die "Expecting indexed object, got @{[shift()->printStr()]}\n"}
sub atPut {die "Expecting indexed object, got @{[shift()->printStr()]}\n"}
sub size {die "Expecting indexed object, got @{[shift()->printStr()]}\n"}

package LL::Number;
use base 'LL::Object';
sub checkNumber {}
sub isTrue {my ($self) = @_; !! ${$self} }
sub isLiteral {return 1}
sub isNumber {return 1}
sub inTypeEq {my ($self, $other) = @_; return ${$self} == ${$other} }



package LL::Stringlike;
use base 'LL::Object';
sub equals {my ($self, $other) = @_; 
			return LL::Main::boolObj(${$self} eq ${$other})}
sub printStr {my ($self) = @_; return $ {$self} }
sub isStringlike {1}

package LL::String;
use base 'LL::Stringlike';
sub checkString {}
sub isAtom {return 1}
sub isLiteral {return 1}		# ???
sub isTrue {my ($self) = @_; return ${$self} ne ''}
sub storeStr {my ($self) = @_; return "\"${$self}\""};


package LL::Symbol;
use base 'LL::Stringlike';

{
  my $symbols = {};

  sub new {
	my ($class, $str) = @_;

	die "Unexpected ref '@{[ref($str)]}' in LL::Symbol->new.\n"
	  if ref($str);

	if (!exists($symbols->{$str})) {
	  $symbols->{$str} = bless \$str, $class;
	}
	
	return $symbols->{$str};
  }
}

sub checkSymbol {}
sub isAtom {return 1}
sub isSymbol {return 1};
sub isOperator {my ($self) = @_; return ${$self} =~ m{^ \W+ $}x }
sub isEscapedOperator {
  my ($self) = @_;
  return ($self->isOperator() && ${$self} =~ m{^\\});
}
sub isUnescapedOperator {
  my ($self) = @_;
  return ${$self} !~ m{^\\} && $self->isOperator();
}
sub asUnescapedOperator {
  my ($self) = @_;

  die "Called 'asUnescapedOperator' on a non-operator.\n"
	unless $self->isOperator();

  my $op = ${$self};
  $op =~ s/^\\//;
  return LL::Symbol->new($op);
}


package LL::List;
use base 'LL::Object';
sub checkList {}
sub isEmptyList {my ($self) = @_; return scalar @{$self} == 0}
sub isTrue {my ($self) = @_; return ! $self->isEmptyList()}
sub isList {return 1}
sub asPrefixList {return shift}
sub storeStr {
  my ($self) = @_;
  return "[".join (" ", map { $_->storeStr() } @{$self})."]";
}
sub printStr {
  my ($self) = @_;
  return "[".join (" ", map { $_->printStr() } @{$self})."]";
}
sub inTypeEq {
  my ($self, $other) = @_;

  return 0 unless scalar @{$self} == scalar @{$other};

  for my $n (0 .. $#{$self}) {
	return 0 unless ( $self->[$n] -> equals($other->[$n]) )->isTrue();
  }

  return 1;
}

# Recursively replace all escaped operators in $self or its sublists
# with their unescaped equivalents.
sub unescapeAllOperators {
  my ($self) = @_;

  for my $entry (@{$self}) {
	$entry = $entry->asUnescapedOperator()
	  if $entry->isEscapedOperator();

	$entry->unescapeAllOperators()
	  if $entry->isList();
  }

  return undef;
}

# Assignment statements are turned infix
sub couldBeImpliedInfix {
  my ($self) = @_;

  # True if it takes the form <x> = <z> ...
  return 0 if scalar @{$self} < 3;	
  return 1 if ($self->[1]->isUnescapedOperator() && ${$self->[1]} eq '=');

  # True if it takes the form <x> @ <y> = <z> ...
  return 0 if scalar @{$self} < 5;
  return 1 if ($self->[1]->isUnescapedOperator() && ${$self->[1]} eq '@' &&
			   $self->[3]->isUnescapedOperator() && ${$self->[3]} eq '=');

  # False otherwise
  return 0;
}


sub _sanitizeIndex {
  my ($self, $index) = @_;

  $index->checkNumber();
  die "Index out of range: ${$index}\n"
	if (int(${$index}) > $#{$self} || int(${$index}) < -scalar(@{$self}));

  return int(${$index});
}

sub size {
  my ($self) = @_;
  return scalar @{$self};
}

sub at {
  my ($self, $index) = @_;
  return $self->[$self->_sanitizeIndex($index)];
}

sub atPut {
  my ($self, $index, $value) = @_;
  $self->[$self->_sanitizeIndex($index)] = $value;

  return $value;
}



package LL::InfixList;
use base 'LL::List';
sub isInfixList {return 1}
{
  my @precedence = ([qw{-> . @}],	# method lookup, field lookup, seq. access
					[qw{**}],		# power
					[qw{* / // %}], # mult, div, div rounded toward zero, modulo
					[qw{+ -}],				# add, subtract
					[qw{<< >> >>> <<<}],	# shifts	
					[qw{== === != !== < > <= >=}],	# Equality and magnitude
					[qw{&}],		# Bitwise AND.
					[qw{| ^}],		# Bitwise OR, bitwise XOR
					[qw{&&}],		# Logical AND, short-circuited
					[qw{||}],		# Logical OR, short-circuited
					[qw{=}],		# Assignment
				   );
  my %precPerOp = ();
  {
	my $prec = 1;
	for my $lev (reverse @precedence) {
	  map { $precPerOp{$_} = $prec } @{$lev};
	  ++$prec;
	}
  }

  sub _findOuterOp {
	my ($self) = @_;

	# Find the lowest-precedence operator in $self
	my ($prec, $index, $op) =  (999999, -1, '');
	for my $ndx (0 .. $#{$self} ) {
	  my $entry = $self->[$ndx];

	  next unless $entry->isUnescapedOperator();

	  my $p = $precPerOp{${$entry}} || 0;
	  if ($p < $prec) {
		$index = $ndx;
		$prec = $p;
		$op = ${$entry};
	  }
	}
	
	# Fail if we don't find an unescaped operator
	return -1 unless $index > -1;

	# '=' is right-associative, so if that's the op, we're done.
	return $index if $op eq '=';

	# Find the left-most use of the operator
	for my $ndx (0 .. $#{$self}) {
	  return $ndx if ${ $self->[$ndx] } eq $op;
	}

	die "_findOuterOp: ('$op', $prec, $index) WTF???";
  }

  sub asPrefixList {
	my ($self) = @_;

	return LL::List->new([]) if scalar @{$self} == 0;

	if (scalar @{$self} == 1) {
	  my $retval = $self->[0];
	  $retval = $retval->asUnescapedOperator()
		if $retval->isEscapedOperator();
	  return $retval;
	}

	my $mfi = "Malformed infix expression: @{[$self->printStr()]}\n";
	die $mfi
	  unless scalar @{$self} % 2;

	my $middle = $self->_findOuterOp();
	die $mfi unless ($middle > 0 && $middle < $#{$self});

	my $left = LL::InfixList->new([ @{$self}[0 .. ($middle - 1)] ]);
	my $right = LL::InfixList->new([ @{$self}[($middle + 1) .. $#{$self}] ]);

	my $result = LL::List->new([$self->[$middle],
								$left->asPrefixList(),
								$right->asPrefixList()]);

	$result->unescapeAllOperators();

	return $result;
  }
}

package LL::Nil;
use base 'LL::Object';
sub new {my ($class) = @_; my $x = ''; return bless \$x, $class}
sub isAtom {return 1}
sub isNil {return 1}
sub isTrue {return 0}
sub storeStr {"nil"}
sub inTypeEq {my ($self, $other) = @_; $other->isNil}

use constant NIL => LL::Nil->new();	# The only instance you should use

package LL::Eol;
use base 'LL::Object';
sub isEol {return 1}
sub isTrue {return 0}	# Maybe not necessary
sub storeStr {"<EOL>"}
sub isExplicitEol {my ($self) = @_; return ${$self} eq ';'}

package LL::Paren;
use base 'LL::Object';
sub isParen {return 1}
sub isOpen {local $_ = ${ shift() }; return /^( \[ | \( |\{)$/x }
sub isClose {local $_ = ${ shift() }; return /^( \) | \) |\})$/x }
sub isBrace {local $_ = ${ shift() }; return /^[{}]$/x }
sub isRoundParen {local $_ = ${ shift() }; return /^[()]$/x }
sub isSquareParen {local $_ = ${ shift() }; return /^[\[\]]$/x }
sub storeStr {my ($self) = @_; return "paren:'${$self}'"};

sub matchesOpen {
  my ($self, $open) = @_;
  my $tok  = ${ $self };
  $open = ref($open) ? ${ $open } : $open;

  return ($open eq '{' && $tok eq '}') ||
	($open eq '(' && $tok eq ')')      ||
	  ($open eq '[' && $tok eq ']');
}



package LL::Quote;
use base 'LL::Object';
sub new {
  my ($class, $ref) = @_;

  die "Trying to quote a non-Object.\n"
	unless $ref->isa("LL::Object");

  return bless [$ref], $class;
}
sub isQuote {return 1}
sub checkQuote {}
sub value {my ($self) = @_; return $self->[0]}
sub storeStr {my ($self) = @_; return ':' . $self->value()->storeStr()}
sub isLoL {	# Is this a quoted list of lists?
  my ($self) = @_;

  return 0 unless $self->value()->isList();
  for my $elem ( @{$self->value()} ) {
	return 0 unless $elem->isList();
  }
	
  return 1;
}
sub checkLoL {
  my ($self, @args) = @_;
  die "Expecting a quoted LoL, got @{[$self->storeStr()]}@_\n"
	unless $self->isLoL();
}
sub equals {
  my ($self, $other) = @_;

  return 0 unless $other->isQuote();
  return $self->value() -> equals($other->value());
}



package LL::Macro;
use base 'LL::Object';
sub isMacro {return 1}
sub storeStr {return "<macro>"}
# isCallable?

package LL::Function;
use base 'LL::Object';
sub isAtom {return 1}
sub isFunction {return 1}
sub isCallable {return 1}
sub storeStr {return "<function>"}




# ---------------------------------------------------------------------------

package LL::Context;

sub new {
  my ($class, $parent) = @_;
  return bless {
				# Reserved fields:
				' parent'		=> $parent,
				' consts'		=> {},	# <- list of const names
			   }, $class;
}

sub def {
  my ($self, $symbol) = @_;

  die "Expecting string, not reference!\n" unless ref($symbol) eq '';
  die "Name contains whitespace.\n" if $symbol =~ /\s/;

  $self->{$symbol} = LL::Nil::NIL;
}

sub set {
  my ($self, $symbol, $value) = @_;

  die "Expecting string, not reference!\n" unless ref($symbol) eq '';
  die "Attempted to modify a const: $symbol.\n"
	if defined($self->{' consts'}->{$symbol});

  exists($self->{$symbol}) and do {
	$self->{$symbol} = $value;
	return;
  };

  defined($self->{' parent'}) and return $self->{' parent'}->set($symbol, $value);

  die "Unknown variable: '$symbol'\n";
}

sub defset {
  my ($self, $symbol, $value) = @_;

  $self->def($symbol);
  $self->set($symbol, $value);
}

sub defsetconst {
  my ($self, $symbol, $value) = @_;

  $self->defset($symbol, $value);
  $self->{' consts'}->{$symbol} = 1;
}


sub lookup {
  my ($self, $symbol) = @_;

  exists($self->{$symbol})	and return $self->{$symbol};
  defined($self->{' parent'})		and return $self->{' parent'}->lookup($symbol);

  die "Unknown variable: '$symbol'\n";
}

sub present {
  my ($self, $symbol) = @_;

  exists($self->{$symbol}) and return 1;
  defined($self->{' parent'}) and return $self->{' parent'}->present($symbol);

  return 0;
}



# ---------------------------------------------------------------------------

package LL::Main;

use Term::ReadLine;
use Scalar::Util qw(looks_like_number);

use constant NIL => LL::Nil::NIL;

my $Input = undef;
my $NeedPrompt = 0;		# If true, reader is inside a LoL Line
my $Globals = LL::Context->new();

# Flags:
my $dumpExpr = 0;

# ---------------------------------------------------------------------------


use Getopt::Long;
GetOptions ('dump-expr'			=> \$dumpExpr)
  or die "Invalid argument.\n";

initGlobals();

if (@ARGV) {
  for my $ifile (@ARGV) {
	open $Input, "< $ifile"
	  or die "Unable to open '$ifile'.";

	interp(0);
  }
} else {

  while (1) {
	eval {interp(1)};
	last unless $@;
	print "Error: $@\n";
  }

  print "\n";
}

exit(0);



# ---------------------------------------------------------------------------


sub interp {
  my ($print) = @_;

  while (1) {
	$NeedPrompt = 1;

	my $expr = readLoLLine(0);
	next if ($expr->isEmptyList());

	$expr = LL::List->new([$expr]);
	my $args = LL::List->new([]);

	my $fn = compile(undef, $args, $expr, 'toplevel', "*top*");

	my $result;
	eval {
	  $result = $fn->();
	};
	if ($@) {
	  die "Called return continuation on a returned function.\n"
		if $@ =~ /^LL::Context=HASH/;
	  die $@;
	}

	print $result->storeStr(), "\n"
	  if $print;
  }
}



# ---------------------------------------------------------------------------

sub readLoLLine {
  my ($insideBraces) = @_;
  my @result = ();

  while (1) {
	my $item = readExpr($insideBraces ? '{' : undef);

	if ($insideBraces && $item->matchesOpen('{')) {
	  if (!@result) {
		return undef;
	  }

	  unread($item);
	  last;
	}

	last if $item->isEol();
	push @result, $item;
  }

  # Create the result object
  my $rlist = LL::List->new(\@result);

  # See if this line could be treated as infix
  if ($rlist->couldBeImpliedInfix()) {
	$rlist = LL::InfixList->new(\@result)->asPrefixList();
  }

  # Warn of the case where an explicit list is the only element of a
  # line because the programmer may have accidentally bracketted the
  # line.
  if (scalar @{$rlist} == 1 && $rlist->[0]->isList()) {
	dkwarn ("Entire LoL line is bracketed.  This may not be what",
			"you want.");
  }

  return $rlist
}


sub readExpr {
  my ($open) = @_;

  my $tok = readNext();

  $tok->isQuote() and do {
	my $quoted = readExpr();
	$tok = LL::Quote->new($quoted);
	return $tok;
  };

  ($tok->isLiteral() || $tok->isSymbol() || $tok->isEol()) and do {
	return $tok
  };

  ($open && $tok->matchesOpen($open)) and do {
	return $tok;
  };

  $tok->isParen() and do {
	(($tok->isSquareParen() || $tok->isRoundParen()) && $tok->isOpen()) and do{
	  return readSexp(${$tok})->asPrefixList();
	};
	
	($tok->isBrace() && $tok->isOpen()) and do {
	  return readLoL();
	};

#	$tok->isRoundParen()
#	  and die "Round parens currently unsupported.\n";
  };

  die "Unexpected token type: @{[ref($tok)]} (@{[$tok->storeStr()]}).\n";
}


# Read a sexp and return a ref. to it.
sub readSexp {
  my ($openChar) = @_;
  my @result;

  while (1) {
	my $tok = readExpr($openChar);

	if ($tok->isEol()) {
	  dkwarn("Found a ';' inside a list.  Probably not what you want.")
		if $tok->isExplicitEol();
	  next;
	}

	last if $tok->matchesOpen($openChar);

	push @result, $tok;
  }

  return $openChar eq '('			?
	LL::InfixList->new(\@result)	:
	LL::List->new(\@result);
}


# Read a LoL
sub readLoL {
  my @result;

  while (1) {
	my $line = readLoLLine(1);

	# Undef means we've found a '}'
	if (!defined($line)) {
	  last;
	}

	# We skip empty lines
	next if $line->isEmptyList();

	if ($line->isList()) {
	  push @result, $line;
	  next;
	}
  }

  return LL::Quote->new(LL::List->new(\@result));
}


{
  my @tokens = ();

  sub readNext {
	fillTokList() unless @tokens;
	return shift @tokens;
  }

  sub unread {
	my ($tok) = @_;
	unshift @tokens, $tok;
  }

  sub fillTokList {

	# Regexp to match operators: may begin with '\'; may not end with
	# '-' (to keep from interfering with trailing negative int) except
	# if it's the minus operator.
	my $OPER_REGEX = qr{\\? [-!@\$\%^&*+=?<>\/]+}x;

	while (!@tokens) {
	  my $line = getLine();
	  if (!defined($line)) {
		exit (0);
	  }
	  chomp $line;

	  while (1) {
		my $tok;

		$line =~ s/^\s*//;
		last if $line eq '';

		# Comments are removed
		$line =~ s{^#.*$}{} and do {
		  next;
		};

		# A single '\' is the line continuation
		$line =~ s{^\\ \s* (\#.*)? $}{}x and do {
		  $line = getLine();
		  next;
		};

		# ';' is the alternate EOL.
		$line =~ s/^;// and do {
		  push @tokens, LL::Eol->new(';');
		  next;
		};

		# Quote characters return an empty Quote object.  It's up to
		# the caller to put them together with the following
		# expression.
		$line =~ s/^\:// and do {
		  push @tokens, LL::Quote->new(NIL);
		  next;
		};

		$line =~ s/^(\-?\d+)\.(\d+)(\W?)/$3/ and do {
		  $tok = "$1.$2" + 0;
		  push @tokens, LL::Number->new($tok);
		  next;
		};

		$line =~ s/^(\-?\d+)(\W?)/$2/ and do {
		  $tok = $1 + 0;
		  push @tokens, LL::Number->new($tok);
		  next;
		};
		
		$line =~ s/^\"([^\"]+)\"// and do {
		  push @tokens, LL::String->new($1);
		  next;
		};

		$line =~ s/^( [a-zA-Z_]\w* | ${OPER_REGEX} )//x and do {
		  push @tokens, LL::Symbol->new($1);
		  next;
		};

		$line =~ s/^( \[ | \] | \{ | \} |\( | \) )//x and do {
		  push @tokens, LL::Paren->new($1);
		  next;
		};

		die "Syntax error: >> '$line'\n";
	  }

	  push @tokens, LL::Eol->new("\n");
	}
  }
}


{
  my $term;
  sub getLine {
	return scalar <$Input> if $Input;

	my $prompt = $NeedPrompt ? "udeck> " : "";
	$NeedPrompt = 0;	# Reset the next time we start reading an expression

	$term = Term::ReadLine->new('udeck')
	  unless $term;

	return $term->readline($prompt);
  }
}


# ---------------------------------------------------------------------------

# Return the canonical Deck true or false value given a Perl true or
# false.
sub boolObj {
  my ($trueOrFalse) = @_;

  return $trueOrFalse ? LL::Number->new(1) : NIL;
}

# Print a warning.
sub dkwarn {
  print STDERR "WARNING: ", join(" ", @_), "\n";
}


# ---------------------------------------------------------------------------


sub evalExpr {
  my ($expr, $context) = @_;

  $expr->isSymbol() and do {
	return $context->lookup(${$expr});
  };

  ($expr->isLiteral() || $expr->isNil()) and do {
	return $expr;
  };

  $expr->isQuote() and do {
	return $expr->value();
  };

  $expr->isList() and do {
	return evalFuncCall($expr, $context);
  };

  die "evalExpr: Don't know what to do with @{[$expr->storeStr()]}.\n";
}


# If $expr is a macro invocation, evaluate it and return the result.
# Note that macros are allowed to return macro calls so this function
# evaluates $expr itteratively until no macros are present.
sub applyMacros {
  my ($expr, $context) = @_;

  # Skip empty lists
  return unless scalar @{$expr};

  my @backtrace = ([@{$expr}]);
  while (1) {
	my $name = $expr->[0];
	last unless ($name->isSymbol() && $context->present($ {$name})) ;
	
	my $val = $context->lookup(${$name});
	last unless $val->isMacro();

	$expr = $val->(@{$expr});

	push @backtrace, $expr;
	if (scalar @backtrace > 20) {
	  die "Probable recursive macro:\n" .
		join("\n", map { $_->storeString()."\n" } @backtrace) . "\n";
	}
  }

  return $expr;
}


# Call applyMacros on every sublist of $expr
sub applyMacrosRecursively {
  my ($expr, $context) = @_;

  return $expr unless $expr->isList();

  $expr = applyMacros($expr, $context);

  my @result;
  for my $elem (@{$expr}) {
	push @result, applyMacrosRecursively($elem, $context);
  }

  return LL::List->new(\@result);
}


# Evaluate $expr, which is a function call.
sub evalFuncCall {
  my ($expr, $context) = @_;

  die "Attempted to evaluate empty list.\n"
	unless scalar @{$expr} > 0;

  my @args = map { evalExpr($_, $context) } @{$expr};
  my $fn = shift @args;

  # _::set, _::sub and _::var are special cases and get access to the
  # context.
  my $fname = $expr->[0]->isSymbol() ? ${$expr->[0]} : '';
  if ($fname =~ /^_::(set|var|sub|const)$/) {
	unshift @args, $context;
  }

  die "Attempted to call non-function '@{[$fn->storeStr()]}' as a function.\n"
	unless $fn->isCallable();

  return $fn->(@args);
}

# ---------------------------------------------------------------------------

# Return a blessed func. ref which executes a sub with $args and $body
# in the given context.  If $context is undef, the $Global context is
# used, allowing the function to define and set global variables.
# $name is used for error messages and may be omitted.
sub compile {
  my ($outerContext, $args, $body, $mode, $name) = @_;

  $name ||= 'unnamed function';

  my $nargs = scalar @{$args};
  my $isVararg = $nargs > 0 && ${$args->[-1]} eq 'args';
  if ($isVararg) {
	pop @{$args};
	--$nargs;
  }

  my @fixedBody;
  {
	my $macroContext = $outerContext ? $outerContext : $Globals;
	for my $expr (@{$body}) {
	  my $newExpr =  applyMacrosRecursively ($expr, $macroContext);
	  push @fixedBody, $newExpr;
	}
  }

  print "$name: ", LL::List->new(\@fixedBody)->storeStr(), "\n"
	if $dumpExpr;

  my $fn = sub {
	my $context = $outerContext ? LL::Context->new($outerContext) : $Globals;

	#  Check for argument mismatch
	(scalar @_ == $nargs || $isVararg && scalar @_ > $nargs)
	  or die "Arg. count mismatch in call to $name.  Expecting $nargs, got " .
		scalar @_ . ".\n";

	# Bind arguments
	for my $arg (@{$args}) {
	  $context->defset ($ {$arg}, shift @_ );
	}

	# Bind varargs
	if ($isVararg) {
	  my @args = @_;

	  $context->defset(LL::Symbol->new('args'), \@args);
	}
	
	my $lastexpr;
	my $retname =
	  ($mode eq 'proc') ? 'return' :
	  ($mode eq 'sub') ? '_lreturn' : '';
	my $ret = sub {
	  my ($retval) = @_;
	  $retval ||= NIL;

	  die "Too many arguments to $retname\n" unless scalar @_ <= 1;

	  $lastexpr = $retval;
	  die "$context\n";
	};
	$context->defsetconst($retname, LL::Function->new($ret))
	  if $retname;

	eval {
	  for my $expr (@fixedBody) {
		$lastexpr = evalExpr($expr, $context);
	  }
	};
	if ($@ && $@ ne "$context\n") {
	  die $@;
	}

	return $lastexpr;
  };

  return LL::Function->new($fn);
}



# ---------------------------------------------------------------------------


sub prim ( $$$$ ) {
  my ($retType, $name, $argsAndTypes, $function) = @_;

  my $prim = sub {
	validateArgs($name, $argsAndTypes, \@_);
	my $retval = $function->(@_);
	return $retType ? "LL::$retType"->new($retval) : $retval;
  };

  $Globals->defset($name, LL::Function->new($prim));
}


sub validateArgs {
  my ($name, $types, $args) = @_;

  my @typeList = split(/\s+/, $types);

  die "Argument count mismatch for '$name'\n"
	if (scalar @typeList != scalar @{$args});

  my $count = 0;
  for my $type (@typeList) {
	$args->[$count++]->checkType($type, $name);	
  }
}


sub prim2 ( $$ ) {
  my ($name, $function) = @_;

  $Globals->defset($name, LL::Function->new($function));
}

# Make $dest reference the same thing as $src
sub alias {
  my ($src, $dest) = @_;
  $Globals->defset($dest, $Globals->lookup($src));
}


# ---------------------------------------------------------------------------

sub macro ( $$ ) {
  my ($name, $transformation) = @_;

  $Globals->defset($name, LL::Macro->new($transformation));
}


# Normalize the arg. list into a quoted list of words
sub fixFormalArgs {
  my ($args) = @_;

  $args = $args->value()
	if $args->isQuote();

  die "Expecting argument list, got @{[ref($args)]}\n"
	unless $args->isList();

  # It should now be either a list of words or a list of lists of
  # words, depending on whether the source used "[]" or "{}".
  my $type = ref($args->[0]);
  for my $arg (@{$args}) {
	die "Malformed argument in list: '@{[$arg->storeStr()]}'\n"
	  unless ref($arg) eq $type;
  }

  if ($type ne 'LL::Symbol') {
	my @flatArgs = ();

	for my $arg (@{$args}) {
	  push @flatArgs, @{$arg};
	}

	$args = LL::List->new(\@flatArgs);
  }

  return LL::Quote->new($args);
}



sub macro_proc {
  my @result = @_;

  $result[0] = LL::Symbol->new('_::proc');

  my $sym = $result[1];
  $sym->checkSymbol(" in 'proc' arg 1");
  $result[1] = LL::Quote->new($sym);

  $result[2] = fixFormalArgs($result[2]);

  $result[3]->checkLoL(" in function body of 'proc'.");

  return LL::List->new(\@result);
}

sub macro_var {
  my @result = (LL::Symbol->new('_::var'));

  shift;
  for my $sym (@_) {
	die "Argument for 'var' '@{[$sym->storeStr()]}' is not a symbol.\n"
	  unless $sym->isSymbol();

	push @result, LL::Quote->new($sym);
  }
  return LL::List->new(\@result);
}

sub macro_const {
  my @result = (LL::Symbol->new('_::const'));

  shift @_;	# Drop the word 'const'

  my $name = shift @_;
  $name && $name->isSymbol()
	or die "Missing or invalid name for 'const'.";

  my $val = shift @_;

  # If the second word is '=', drop it and fetch the third.
  if ($val and $val->isSymbol() and ${$val} eq '=') {
	$val = shift @_;
  }

  die "Value missing in 'const' declaration.\n" unless $val;

  die "Too many arguments to 'const'.\n"
	if scalar @_;

  push @result, LL::Quote->new($name);
  push @result, $val;

  return LL::List->new(\@result);
}

# Handle the 'set' and '=' functions.
sub macro_assign {
  my @result = @_;
  $result[0] = LL::Symbol->new('_::set');

  my $target = $result[1];

  if ($target->isSymbol()) {
	# Case 1: simple assignment to variable
	$result[1] = LL::Quote->new($target);
  } elsif ($target->isList() && scalar @{$target} == 3 &&
		   $target->[0]->isUnescapedOperator() &&
		   ${$target->[0]} eq '@') {
	# Case 2: List element assignment (eg: 'l@5 = 42')
	@result = (LL::Symbol->new('_::atput'), @{$target}[1..2],
			   @result[2..$#result]);
  } else {
	my $err = LL::List->new(\@_)->printStr();
	die "Malformed assignment: $err\n";
	# To do: handle list and object-field assignments as well
  }

  return LL::List->new(\@result);
}


sub macro_subfn {
  my @args = @_;

  die "Too many arguments for 'sub'.\n"
	if (scalar @args > 3);

  $args[0] = LL::Symbol->new('_::sub');

  # Insert the arg. list if it was omitted.
  if (scalar @args == 2) {
	$args[2] = $args[1];
	$args[1] = LL::Quote->new(LL::List->new([]));
  }

  # Normalize the arg list.
  $args[1] = fixFormalArgs($args[1]);

  return LL::List->new(\@args)
}


sub macro_iffn {
  my @args = @_;

  $args[0] = LL::Symbol->new('_::if');

  # Remove the 'else' word if present
  if (defined($args[3]) && $args[3]->isSymbol()) {
	die "Expecting 'else', got '@{[$args[3]->storeStr()]}'\n"
	  unless ${$args[3]} eq 'else';

	my $elseClause = pop @args;
	pop @args;
	push @args, $elseClause;
  }

  die "Too many arguments to 'if'\n"
	unless scalar @args <= 5;

  my $sub = LL::Symbol->new('sub');

  for my $i (1 .. $#args) {
	$args[$i]->checkLoL();
	$args[$i] = LL::List->new([$sub, $args[$i]]);
  }

  # Add the empty else clause
  push @args, NIL
	if scalar @args == 3;

  return LL::List->new(\@args);
}


sub macro_whilefn {
  my @args = @_;

  die "Expecting 2 arguments to 'while'; got @{[scalar @args - 1]}\n"
	unless scalar @args == 3;

  $args[0] = LL::Symbol->new('_::while');

  my $sub = LL::Symbol->new('sub');
  for my $i (1 .. $#args) {
	$args[$i]->checkLoL();
	$args[$i] = LL::List->new([$sub, $args[$i]]);
  }

  return LL::List->new(\@args);
}


# ---------------------------------------------------------------------------
sub initGlobals {

  $Globals->def('nil');

  # Externally-defined primitive functions
  for my $special (
				   ['println',	\&builtin_println],
				   ['puts',		\&builtin_println],
				   ['_::proc',	\&builtin_proc],
				   ['_::sub',	\&builtin_subfn],
				   ['_::if',	\&builtin_iffn],
				   ['_::while',	\&builtin_whilefn],
				   ['_::set',	\&builtin_set],
				   ['_::var',	\&builtin_var],
				   ['_::const', \&builtin_const],
				   ['_::atput', \&builtin_atput],
				   ['atput',    \&builtin_atput],
				  ) {
	$Globals->defset($special->[0], LL::Function->new($special->[1]));
  }

  # Simple numeric primitive functions
  prim 'Number', '+',  "Number Number", sub { return $ {$_[0]} +  ${$_[1]} };
  prim 'Number', '-',  "Number Number", sub { return $ {$_[0]} -  ${$_[1]} };
  prim 'Number', '*',  "Number Number", sub { return $ {$_[0]} *  ${$_[1]} };
  prim 'Number', '/',  "Number Number", sub { return $ {$_[0]} /  ${$_[1]} };
  prim 'Number', '<',  "Number Number", sub { return $ {$_[0]} <  ${$_[1]} };
  prim 'Number', '<=', "Number Number", sub { return $ {$_[0]} <= ${$_[1]} };
  prim 'Number', '>',  "Number Number", sub { return $ {$_[0]} >  ${$_[1]} };
  prim 'Number', '>=', "Number Number", sub { return $ {$_[0]} >= ${$_[1]} };

  # Other simple primitives
  prim 'Symbol', 'typeof', "Object", sub { local $_=ref($_[0]); s/^LL:://; $_};

  # More complex primitive functions
  prim2 '===',      sub { return boolObj($_[0] == $_[1])};
  prim2 '==',       sub { return $_[0]->equals($_[1]) };
  prim2 'list',     sub { return LL::List->new(\@_) };
  prim2 'val',      sub { return NIL unless scalar @_; return $_[-1] };
  prim2 '@',        sub { my ($l, $ndx) = @_; return $l->at($ndx) };

  prim2 'size',     sub { my ($l) = @_; return LL::Number->new($l->size()) };

  # Macros
  macro 'var',	\&macro_var;
  macro 'const',\&macro_const;
  macro 'proc', \&macro_proc;
  macro 'set',  \&macro_assign;
  macro '=',    \&macro_assign;
  macro 'sub',  \&macro_subfn;
  macro 'if',   \&macro_iffn;
  macro 'while',\&macro_whilefn;
}

sub builtin_println {
  for my $obj (@_) {
	print $obj->printStr();
  }
  print "\n";

  return NIL;
}

sub builtin_set {
  my ($context, $name, $value) = @_;

  die "'set' expects 2 arguments, got @{[scalar @_ - 1]}\n"
	unless scalar @_ == 3;

  $name->checkSymbol();
  $context->set(${$name}, $value);

  return $value;
}


sub builtin_atput {
  my ($l, $ndx, $v) = @_;

  die "'atput' expects 3 arguments, got @{[scalar @_ - 1]}\n"
	unless scalar @_ == 3;

  return $l->atPut($ndx, $v);
};


sub builtin_var {
  my ($context, @names) = @_;

  for my $name (@names) {
	$name->checkSymbol(" in var argument.");
	$context->defset(${$name}, NIL);
  }

  return NIL;
}

sub builtin_const {
  my ($context, $name, $value) = @_;

  $name->checkSymbol(" in const argument.");
  $context->defsetconst(${$name}, $value);

  return NIL;
}


sub builtin_proc {
  my ($name, $args, $body) = @_;

  # Argument checking.
  die "'proc' expects 3 arguments: got @{[scalar @_]}\n"
	unless scalar @_ == 3;

  $name->checkSymbol();
  $args->checkList();
  $body->checkList();

  my $func = compile ($Globals, $args, $body, 'proc', ${$name});

  $Globals->defset(${$name}, $func);

  return $func;
}


sub builtin_subfn {
  my ($context, $args, $body) = @_;

  $args->checkList();
  $body->checkList();

  return compile ($context, $args, $body, 'sub');
}


# Perform the 'if' operation.  Return the result of the last closure
# evaluated.  The third closure (the 'else' clause) is optional and may be NIL.
sub builtin_iffn {
  my ($test, $trueBlock, $falseBlock) = @_;

  my $result = $test->();
  if ($result->isTrue()) {
	$result = $trueBlock->();
  } elsif ($falseBlock->isFunction()) {
	$result = $falseBlock->();
  }

  $result;
}

sub builtin_whilefn {
  my ($test, $body) = @_;

  my $result = NIL;
  while ($test->()->isTrue()) {
	$result = $body->();
  }

  return $body;
}


=pod

Notes:

	-Now uses square brackets for infix but tests don't all do that.

	-let's use ':' as the quote.

	-Tolerates [x], {x} and :[x] as proc argument lists.

	-** is not right-associative.

	-I think I'll bow to starting arrays with index 0.

Todo:
X	-return values
	-implement a "compiler" to produce perl subs
	-catch arg. count mismatches.
X	-consts
X	-equality, equivalence


X	- '=' as alias for 'set'
X	- infix in LoLs.
X	- escaped operators
	- procs should return nil by default.
	- list access via @, @= and set macro.
		-need to update infix to handle it.
		-foreach
		-multi-dimensional list access (e.g. 'a@b@c = 42')
	- macros
	- namespaces
	- objects
	- integers
	- Shouldn't allow user-defined operators

	- Unary minus: space (e.g. "- 1") -> binary, no space (e.g. "-1") -> unary

=cut
