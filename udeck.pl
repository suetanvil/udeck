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

sub checkNumber {shift; die "Expected number, got @{[ref(shift)]}@_\n"}
sub checkString {shift; die "Expected string, got @{[ref(shift)]}@_\n"}
sub checkList   {shift; die "Expected list, got @{[ref(shift)]}@_\n"}
sub checkSymbol {shift; die "Expected symbol, got @{[ref(shift)]}@_\n"}
sub checkQuote  {shift; die "Expected quoted expr, got @{[ref(shift)]}@_\n"}
sub checkLoL    {shift; die "Expected quoted LoL, got @{[ref(shift)]}@_\n"}
sub isAtom {return 0}
sub isSymbol {return 0}
sub isEol {return 0}
sub isParen {return 0}
sub isLiteral {return 0}
sub isEmptyList {return 0}
sub isList {return 0}
sub isQuote {return 0}
sub isNil {return 0}
sub isMacro {return 0}
sub isFunction {return 0}
sub isTrue {return 1}
sub isNumber {return 0}
sub isLoL {return 0}	# Is a quoted list containing only lists
sub matchesOpen {return 0}
sub printStr {my ($self) = @_; return "${$self}"};

sub equals {
  my ($self, $other) = @_;
  return LL::Main::boolObj(ref($self) eq ref($other) && $self->inTypeEq($other));
}
sub inTypeEq {my ($self, $other) = @_; return $self == $other }


package LL::Number;
use base 'LL::Object';
sub checkNumber {}
sub isTrue {my ($self) = @_; !! ${$self} }
sub isLiteral {return 1}
sub isNumber {return 1}
sub inTypeEq {my ($self, $other) = @_; return ${$self} == ${$other} }



package LL::Stringlike;
use base 'LL::Object';
sub equals {my ($self, $other) = @_; return LL::Main::boolObj(${$self} eq ${$other})}

package LL::String;
use base 'LL::Stringlike';
sub checkString {}
sub isAtom {return 1}
sub isLiteral {return 1}		# ???
sub isTrue {my ($self) = @_; return ${$self} ne ''}
sub printStr {my ($self) = @_; return "${$self}"};

package LL::Symbol;
use base 'LL::Stringlike';
sub checkSymbol {}
sub isAtom {return 1}
sub isSymbol {return 1};
sub printStr {my ($self) = @_; return ":${$self}"};


package LL::List;
use base 'LL::Object';
sub checkList {}
sub isEmptyList {my ($self) = @_; return scalar @{$self} == 0}
sub isTrue {my ($self) = @_; return ! $self->isEmptyList()}
sub isList {return 1}
sub printStr {
  my ($self) = @_;
  return "[".join (" ", map { $_->printStr() } @{$self})."]";
}
sub inTypeEq {
  my ($self, $other) = @_;

  my $nil = LL::Main::boolObj(0);

  return $nil unless scalar @{$self} == scalar @{$self};

  for my $n (0 .. $#{$self}) {
	return $nil unless ( $self->[$n] -> equals($other->[$n]) )->isTrue();
  }

  return LL::Main::boolObj(1);
}



package LL::Nil;
use base 'LL::Object';
sub new {my ($class) = @_; my $x = ''; return bless \$x, $class}
sub isAtom {return 1}
sub isNil {return 1}
sub isTrue {return 0}
sub printStr {"nil"}
sub inTypeEq {my ($self, $other) = @_; LL::Main::boolObj($other->isNil)}

use constant NIL => LL::Nil->new();	# The only instance you should use

package LL::Eol;
use base 'LL::Object';
sub isEol {return 1}
sub isTrue {return 0}	# Maybe not necessary
sub printStr {"<EOL>"}

package LL::Paren;
use base 'LL::Object';
sub isParen {return 1}
sub isOpen {local $_ = ${ shift() }; return /^( \[ | \( |\{)$/x }
sub isClose {local $_ = ${ shift() }; return /^( \) | \) |\})$/x }
sub isBrace {local $_ = ${ shift() }; return /^[{}]$/x }
sub isRoundParen {local $_ = ${ shift() }; return /^[()]$/x }
sub isSquareParen {local $_ = ${ shift() }; return /^[\[\]]$/x }
sub printStr {my ($self) = @_; return "paren:'${$self}'"};

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
sub printStr {my ($self) = @_; return ':' . $self->value()->printStr()}
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
  die "Expecting a quoted LoL, got @{[$self->printStr()]}@_\n"
	unless $self->isLoL();
}
sub inTypeEq {my ($self, $other) = @_;
			  return $self->value()->equals($other->value())
}


package LL::Macro;
use base 'LL::Object';
sub isMacro {return 1}
sub printStr {return "<macro>"}


package LL::Function;
use base 'LL::Object';
sub isAtom {return 1}
sub isFunction {return 1}
sub printStr {return "<function>"}




# ---------------------------------------------------------------------------

package LL::Context;

sub new {
  my ($class, $parent) = @_;
  return bless {' ' => $parent}, $class;
}

sub def {
  my ($self, $symbol) = @_;

  die "Expecting string, not reference!\n" unless ref($symbol) eq '';
  die "Illegal variable name: one ASCII space.\n" if $symbol eq ' ';

  $self->{$symbol} = LL::Nil::NIL;
}

sub set {
  my ($self, $symbol, $value) = @_;

  die "Expecting string, not reference!\n" unless ref($symbol) eq '';

  exists($self->{$symbol}) and do {
	$self->{$symbol} = $value;
	return;
  };

  defined($self->{' '}) and return $self->{' '}->set($symbol, $value);

  die "Unknown variable: '$symbol'\n";
}

sub defset {
  my ($self, $symbol, $value) = @_;

  $self->def($symbol);
  $self->set($symbol, $value);
}

sub lookup {
  my ($self, $symbol) = @_;

  exists($self->{$symbol})	and return $self->{$symbol};
  defined($self->{' '})		and return $self->{' '}->lookup($symbol);

  die "Unknown variable: '$symbol'\n";
}

sub present {
  my ($self, $symbol) = @_;

  exists($self->{$symbol}) and return 1;
  defined($self->{' '}) and return $self->{' '}->present($symbol);

  return 0;
}



# ---------------------------------------------------------------------------

package LL::Main;

use Term::ReadLine;

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

	interp();
  }
} else {

  while (1) {
	eval {interp()};
	last unless $@;
	print "Error: $@\n";
  }

  print "\n";
}

exit(0);



# ---------------------------------------------------------------------------


sub interp {
  while (1) {
	$NeedPrompt = 1;

	my $expr = readLoLLine(0);
	next if ($expr->isEmptyList());

	$expr = applyMacrosRecursively($expr, $Globals);

	print $expr->printStr(), "\n"
	  if $dumpExpr;

	evalExpr($expr, $Globals);
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

  return LL::List->new(\@result);
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
	($tok->isSquareParen() && $tok->isOpen()) and do {
	  return readSexp(${$tok});
	};
	
	($tok->isBrace() && $tok->isOpen()) and do {
	  return readLoL();
	};

	$tok->isRoundParen()
	  and die "Round parens currently unsupported.\n";
  };

  die "Unexpected token type: @{[ref($tok)]} (@{[$tok->printStr()]}).\n";
}


# Read a sexp and return a ref. to it.
sub readSexp {
  my ($openChar) = @_;
  my @result;

  while (1) {
	my $tok = readExpr($openChar);

	next if $tok->isEol();

	last if $tok->matchesOpen($openChar);

	push @result, $tok;
  }

  return LL::List->new(\@result);
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
	while (!@tokens) {
	  my $line = $Input ? <$Input> : getLine();
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

		# Quote characters return an empty Quote object.  It's up to
		# the caller to put them together with the following
		# expression.
		$line =~ s/^\:// and do {
		  push @tokens, LL::Quote->new(NIL);
		  next;
		};

		$line =~ s/^(\d+)\.(\d+)(\W?)/$3/ and do {
		  $tok = "$1.$2" + 0;
		  push @tokens, LL::Number->new($tok);
		  next;
		};

		$line =~ s/^(\d+)(\W?)/$2/ and do {
		  $tok = $1 + 0;
		  push @tokens, LL::Number->new($tok);
		  next;
		};
		
		$line =~ s/^\"([^\"]+)\"// and do {
		  push @tokens, LL::String->new($1);
		  next;
		};

		$line =~ s/^( \w+ | [!@\$\%^&*+-=?<>\/]+ )//x and do {
		  push @tokens, LL::Symbol->new($1);
		  next;
		};

		$line =~ s/^( \[ | \] | \{ | \} |\( | \) )//x and do {
		  push @tokens, LL::Paren->new($1);
		  next;
		};

		die "Syntax error: >> '$line'\n";
	  }

	  push @tokens, LL::Eol->new(';');
	}
  }
}


{
  my $term;
  sub getLine {
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

  die "evalExpr: Don't know what to do with @{[$expr->printStr()]}.\n";
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
	last unless $name->isSymbol();
	
	my $val = $context->lookup(${$name});
	last unless $val->isMacro();

	$expr = $val->(@{$expr});

	push @backtrace, $expr;
	if (scalar @backtrace > 20) {
	  die "Probable recursive macro:\n" .
		join("\n", map { $_->printString()."\n" } @backtrace) . "\n";
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
  if ($fname =~ /^_::(set|var|sub)$/) {
	unshift @args, $context;
  }

  return $fn->(@args);
}

# ---------------------------------------------------------------------------

sub compile {
  my ($outerContext, $args, $body, $name) = @_;

  $name ||= 'unnamed function';
  $outerContext ||= $Globals;


  my $nargs = scalar @{$args};
  my $isVararg = $nargs > 0 && ${$args->[-1]} eq 'args';
  if ($isVararg) {
	pop @{$args};
	--$nargs;
  }

  my @fixedBody;
  for my $expr (@{$body}) {
	push @fixedBody, applyMacrosRecursively ($expr, $outerContext);
  }


  my $fn = sub {
	my $context = LL::Context->new($outerContext);

	#  Check for argument mismatch
	(scalar @_ == $nargs || $isVararg && scalar @_ > $nargs)
	  or die "Arg. count mismatch in call to $name.  Expecting $nargs, got " .
		scalar @_ . ".\n";

	# Bind arguments
	for my $arg (@{$args}) {
	  $context->defset(${$arg}, shift @_);
	}

	# Bind varargs
	if ($isVararg) {
	  my @args = @_;

	  $context->defset(LL::Symbol->new('args'), \@args);
	}

	my $lastexpr;
	for my $expr (@fixedBody) {
	  $lastexpr = evalExpr($expr, $context);
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


# ---------------------------------------------------------------------------

sub macro ( $$ ) {
  my ($name, $transformation) = @_;

  $Globals->defset($name, LL::Macro->new($transformation));
}


sub macro_proc {
  my @result = @_;

  $result[0] = LL::Symbol->new('_::proc');

  my $sym = $result[1];
  $sym->checkSymbol(" in macro 'proc' arg 1");
  $result[1] = LL::Quote->new($sym);

  my $args = $result[2];

  # We tolerate three types of arg. lists: [a b c], '[a b c] and {a b c}
  # This case handles the second two.  
  if ($args->isQuote() && $args->value()->isList()) {

	# Identify the list type.
	my $type = ref($args->value()->[0]);
	for my $arg (@{$args->value()}) {
	  die "Malformed argument in list: '@{[$arg->printStr()]}'\n"
		unless ref($arg) eq $type;
	}

	if ($type eq 'LL::Symbol') {
	  $args = $args->value();
	} else {
	  my @flatArgs = ();

	  for my $arg (@{$args->value()}) {
		push @flatArgs, @{$arg};
	  }

	  $args = LL::List->new(\@flatArgs);
	}
  }

  $args->checkList(" in macro 'proc' arg 2");
  $result[2] = LL::Quote->new($args);

  return LL::List->new(\@result);
}

sub macro_var {
  my @result = (LL::Symbol->new('_::var'));

  shift;
  for my $sym (@_) {
	die "Argument for 'var' '@{[$sym->printStr()]}' is not a symbol.\n"
	  unless $sym->isSymbol();

	push @result, LL::Quote->new($sym);
  }
  return LL::List->new(\@result);
}

sub macro_quoteSecond {
  my @result = @_;
  $result[0] = LL::Symbol->new('_::' . $ {$result[0]} );

  my $sym = $result[1];
  $sym->checkSymbol();
  $result[1] = LL::Quote->new($sym);

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

  return LL::List->new(\@args)
}


sub macro_iffn {
  my @args = @_;

  $args[0] = LL::Symbol->new('_::if');

  # Remove the 'else' word if present
  if (defined($args[3]) && $args[3]->isSymbol()) {
	die "Expecting 'else', got '@{[$args[3]->printStr()]}'\n"
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
				   ['_::set',	\&builtin_set],
				   ['_::var',	\&builtin_var],
				  ) {
	$Globals->defset($special->[0], LL::Function->new($special->[1]));
  }

  # Simple primitive functions
  prim 'Number', '+',  "Number Number", sub { return $ {$_[0]} +  ${$_[1]} };
  prim 'Number', '-',  "Number Number", sub { return $ {$_[0]} -  ${$_[1]} };
  prim 'Number', '*',  "Number Number", sub { return $ {$_[0]} *  ${$_[1]} };
  prim 'Number', '/',  "Number Number", sub { return $ {$_[0]} /  ${$_[1]} };
  prim 'Number', '<',  "Number Number", sub { return $ {$_[0]} <  ${$_[1]} };
  prim 'Number', '<=', "Number Number", sub { return $ {$_[0]} <= ${$_[1]} };
  prim 'Number', '>',  "Number Number", sub { return $ {$_[0]} >  ${$_[1]} };
  prim 'Number', '>=', "Number Number", sub { return $ {$_[0]} >= ${$_[1]} };
  prim '',      '===', "Object Object", sub { return boolObj($_[0] == $_[1])};
  prim '',       '==', "Object Object", sub { return $_[0]->equals($_->[1]) };

  # Macros
  macro 'var',	\&macro_var;
  macro 'proc', \&macro_proc;
  macro 'set',  \&macro_quoteSecond;
  macro 'sub',  \&macro_subfn;
  macro 'if',   \&macro_iffn;
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

sub builtin_var {
  my ($context, @names) = @_;

  for my $name (@names) {
	die "'var' arguments must be symbols.\n"
	  unless $name->isSymbol();

	$context->defset(${$name}, NIL);
  }

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

  my $func = compile ($Globals, $args, $body, ${$name});

  $Globals->defset(${$name}, $func);

  return $func;
}


sub builtin_subfn {
  my ($context, $args, $body) = @_;

  $args->checkList();
  $body->checkList();

  return compile ($context, $args, $body);
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



=pod

Notes:

	-Now uses square brackets for infix but tests don't all do that.

	-let's use ':' as the quote.

	-let's use a [] list for the args to proc

Todo:
	-return values
	-implement a "compiler" to produce perl subs
	-catch arg. count mismatches.
	-consts
	-equality, equivalence

=cut
