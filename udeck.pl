#!/usr/bin/perl

# Interpreter for a minimal implementation of Deck


use strict;
use warnings;

use feature "switch";

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
sub checkByte   {die "Expected integer from 0 to 255, got @{[ref(shift)]}@_\n"}
sub checkString {die "Expected string, got @{[ref(shift)]}@_\n"}
sub checkList   {die "Expected list, got @{[ref(shift)]}@_\n"}
sub checkSymbol {die "Expected symbol, got @{[ref(shift)]}@_\n"}
sub checkQuote  {die "Expected quoted expr, got @{[ref(shift)]}@_\n"}
sub checkLoL    {die "Expected quoted LoL, got @{[ref(shift)]}@_\n"}
sub checkFun	{die "Expected function, got @{[ref(shift)]}@_\n"}
sub checkLocalName {die "Expected unqualified name, got @{[ref(shift)]}@_\n"}
sub isAtom {return 0}
sub isSymbol {return 0}
sub isLocalName {return 0}
sub isStringlike {0}
sub isString {0}
sub isInterpString {return 0}
sub isOperator {return 0}
sub isUnescapedOperator {return 0}
sub isEscapedOperator {return 0}
sub isEol {return 0}
sub isEof {return 0}
sub isExplicitEol {return 0}
sub isParen {return 0}
sub isRoundParen {return 0}
sub isLiteral {return 0}
sub isEmptyList {return 0}
sub isList {return 0}
sub isIndexable {return 0}
sub isByteArray {return 0}
sub isInfixList {return 0}
sub isQuote {return 0}
sub isNil {return 0}
sub isMacro {return 0}
sub isFunction {return 0}
sub isCallable {return 0}
sub isTrue {return 1}
sub isNumber {return 0}
sub isLoL {return 0}	# Is a quoted list containing only lists
sub isPerlObj {return 0}
sub matchesOpen {return 0}
sub storeStr {my ($self) = @_; return "${$self}"}
sub printStr {my ($self) = @_; return $self->storeStr()};
sub perlForm {my ($self) = @_; die "No perl form for @{[$self->printStr]}\n"}

sub equals {
  my ($self, $other) = @_;
  return LL::Main::boolObj(ref($self) eq ref($other) &&
						   $self->inTypeEq($other));
}
sub inTypeEq {my ($self, $other) = @_; return $self == $other }

sub at {die "Expecting indexed object, got @{[shift()->printStr()]}\n"}
sub atPut {die "Expecting indexed object, got @{[shift()->printStr()]}\n"}
sub size {die "Expecting indexed object, got @{[shift()->printStr()]}\n"}
sub checkIndexable {
  my ($self) = @_;
  die "Expecting indexable type, got @{[ref($self)]}\n"
	unless $self->isIndexable();
}



package LL::Number;
use base 'LL::Object';
sub checkNumber {}
sub checkByte {
  my ($self) = @_;
  my $val = ${$self};

  $self->SUPER::checkNumber()
	unless ( $val >= 0 && $val <= 255 && $val eq int($val) );
}

sub isTrue {my ($self) = @_; !! ${$self} }
sub isLiteral {return 1}
sub isNumber {return 1}
sub inTypeEq {my ($self, $other) = @_; return ${$self} == ${$other} }
sub perlForm {my ($self) = @_; return ${$self}}


package LL::Stringlike;
use base 'LL::Object';
sub equals {my ($self, $other) = @_;
			return LL::Main::boolObj(${$self} eq ${$other})}
sub printStr {my ($self) = @_; return $ {$self} }
sub isStringlike {1}
sub isIndexable {return 1}
sub perlForm {my ($self) = @_; return ${$self}}

sub _sanitizeIndex {
  my ($self, $index) = @_;

  $index->checkNumber();
  die "Index out of range: ${$index}\n"
	if (int(${$index}) > length (${$self}) - 1 ||
		int(${$index}) < -length(${$self}) );

  return int(${$index});
}

sub size {my ($self) = @_; return length(${$self}) }
sub at {
  my ($self, $index) = @_;
  return LL::String->new( substr(${$self}, $self->_sanitizeIndex($index), 1) );
}

sub atPut {
  my ($self, $index, $value) = @_;

  $value->checkString();
  die "Attempted to store a multi-character string inside another string.\n"
	if length(${$value}) != 1;

  $index = $self->_sanitizeIndex($index);
  substr(${$self}, $index, 1) = ${$value};

  return $value;
}


package LL::String;
use base 'LL::Stringlike';
sub checkString {}
sub isAtom {return 1}
sub isLiteral {return 1}		# ???
sub isTrue {my ($self) = @_; return ${$self} ne ''}
sub storeStr {my ($self) = @_; return "\"${$self}\""};
sub isString {return 1}


# Interpolated string.
package LL::InterpString;
use base 'LL::String';
sub isInterpString {return 1}


package LL::ByteArray;
use base 'LL::Stringlike';

sub new {
  my ($class, @bytes) = @_;

  for my $byte (@bytes) {
	$byte->checkByte();
  }

  my $contents = join("", map { chr(${$_}) } @bytes);
  my $self = \$contents;

  return bless $self, $class;
}

sub newSized {
  my ($class, $size) = @_;

  my $contents = "\0" x $size;
  my $self = \$contents;

  return bless $self, $class;
}

sub isAtom {return 1}
sub isTrue {my ($self) = @_; return ${$self} ne ''}
sub storeStr {
  my ($self) = @_;
  my $body = join (" ", map {sprintf '0x%02x', ord($_)} split(//, ${$self}) );
  return "[byteArray $body]";
}
sub printStr {my ($self) = @_; return $self->storeStr() }

sub at {
  my ($self, $index) = @_;

  my $char = substr(${$self}, $self->_sanitizeIndex($index), 1);
  return LL::Number->new(ord($char));
}

sub atPut {
  my ($self, $index, $value) = @_;

  $value->checkByte();
  $index = $self->_sanitizeIndex($index);
  substr(${$self}, $index, 1) = chr(${$value});

  return $value;
}



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

sub isLocalName {
  my ($self) = @_;

  return ${$self} !~ /\:\:/;
}

sub checkLocalName {
  my ($self, @args) = @_;
  $self->SUPER::checkLocalName(@args) unless $self->isLocalName();
}

sub atPut {die "Attempted to alter a symbol.\n"}


package LL::List;
use base 'LL::Object';
sub checkList {}
sub isEmptyList {my ($self) = @_; return scalar @{$self} == 0}
sub isTrue {my ($self) = @_; return ! $self->isEmptyList()}
sub isList {return 1}
sub isIndexable {return 1}
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

sub perlForm {
  my ($self) = @_;

  my @result = map { $_->perlForm() } @{$self};
  return \@result;
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

	# Quoted objects are a special case.
	if ($entry->isQuote() && $entry->value()->isEscapedOperator()) {
	  $entry->[0] = $entry->[0]->asUnescapedOperator();
	}

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

sub parsedAsInfix {
  my ($self) = @_;

  return LL::InfixList->new($self)->asPrefixList();
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
	  if ($p <= $prec) {
		$index = $ndx;
		$prec = $p;
		$op = ${$entry};
	  }
	}
	
	# Fail if we don't find an unescaped operator
	return -1 unless $index > -1;

	# '=' is right-associative, so if that's the op, we're done.
	return $index unless ($op eq '=');

	# Find the right-most use of the operator
	for my $ndx (0 .. $#{$self}) {
	  my $elem = $self->[$ndx];
	  return $ndx if ($elem->isSymbol() && ${ $elem } eq $op);
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
sub perlForm {my ($self) = @_; return undef}

use constant NIL => LL::Nil->new();	# The only instance you should use

package LL::Eol;
use base 'LL::Object';
sub isEol {return 1}
sub isTrue {return 0}	# Maybe not necessary
sub storeStr {"<EOL>"}
sub isExplicitEol {my ($self) = @_; return ${$self} eq ';'}

package LL::Eof;
use base 'LL::Object';
sub isEof {return 1}
sub isTrue {return 0}	# Maybe not necessary
sub storeStr {"<EOF>"}


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


package LL::Function;
use base 'LL::Object';
sub isAtom {return 1}
sub isFunction {return 1}
sub checkFun {}
sub isCallable {return 1}
sub storeStr {return "<function>"}


package LL::PerlObj;
use base 'LL::Object';
sub new {
  my ($class, $obj) = @_;
  die "Expecting a ref.\n" unless ref($obj);
  return bless [$obj], $class;
}
sub isPerlObj {return 1}
sub storeStr {my ($self) = @_; return "<perlobj @{[ref($self->[0])]}>"}
sub perlForm {my ($self) = @_; return $self->[0];}






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

sub isQualified {
  my ($self, $name) = @_;

  return $name =~ /\:\:/;
}

sub deferToGlobal {my ($self, $name) = @_; return $self->isQualified($name)}
sub normalizeName {my ($self, $name) = @_; return $name}

sub checkName {
  my ($self, $name) = @_;

  die "Qualified name defined in local context."
	if $self->isQualified($name);
}

sub def {
  my ($self, $name) = @_;

  $name = $self->normalizeName($name);
  die "Expecting string, not reference!\n" unless ref($name) eq '';
  die "Name contains whitespace.\n" if $name =~ /\s/;
  $self->checkName($name);

  return $self->{$name} = LL::Nil::NIL;
}

sub set {
  my ($self, $name, $value) = @_;

  $name = $self->normalizeName($name);
  die "Expecting string, not reference!\n" unless ref($name) eq '';
  die "Attempted to modify a const: $name.\n"
	if defined($self->{' consts'}->{$name});

  exists($self->{$name}) and do {
	$self->{$name} = $value;
	return $value;
  };

  defined($self->{' parent'}) and return $self->{' parent'}->set($name, $value);

  die "Unknown variable: '$name'\n";
}

sub defset {
  my ($self, $name, $value) = @_;

  $name = $self->normalizeName($name);
  $self->checkName($name);

  $self->def($name);
  $self->set($name, $value);

  return $value;
}

sub defsetconst {
  my ($self, $name, $value) = @_;

  $name = $self->normalizeName($name);
  $self->checkName($name);

  $self->defset($name, $value);
  $self->{' consts'}->{$name} = 1;

  return $value;
}


sub lookup {
  my ($self, $name) = @_;

  $name = $self->normalizeName($name);

  exists($self->{$name})      and return $self->{$name};
  defined($self->{' parent'}) and return $self->{' parent'}->lookup($name);

  die "Unknown variable: '$name'\n";
}

sub present {
  my ($self, $name) = @_;

  $name = $self->normalizeName($name);

  exists($self->{$name}) and return 1;
  defined($self->{' parent'}) and return $self->{' parent'}->present($name);

  return 0;
}



package LL::GlobalContext;
use base 'LL::Context';

sub new {
  my $self = LL::Context::new(@_);
  $self->{' namespace'} = '';			# The current namespace
  $self->{' namespaces'} = {};			# The set of declared namespaces
  $self->{' imported symbols'} = {};	# The set of names that are imports

  return $self;
}

sub _chkns {
  my ($self, $ns) = @_;
  die "Undefined namespace '$ns'\n"
	unless defined($self->{' namespaces'}->{$ns});
}

# Set default namespace
sub setNamespace {
  my ($self, $ns) = @_;
  $self->_chkns($ns);
  $self->{' namespace'} = $ns;
  return;
}

sub getNamespace {my ($self) = @_; return $self->{' namespace'}}
sub defNamespace {my ($self, $ns) = @_; $self->{' namespaces'}->{$ns} = 1}
sub hasNamespace {my ($self, $ns) = @_;
				  return exists($self->{' namespaces'}->{$ns})}


sub normalizeName {
  my ($self, $name) = @_;

  return $name if $self->isQualified($name);
  return $self->getNamespace() . '::' . $name;
}

# Split a name into ($namespace, $name) pairs
sub _splitName {
  my ($self, $name) = @_;

  my @np = split (/::/, $name);
  my $namePart = pop @np;
  my $namespacePart = join ("::", @np);

  return ($namespacePart, $namePart);
}

sub deferToGlobal {return 0}

# Ensure $name is valid for this context
sub checkName {
  my ($self, $name) = @_;

  # We allow qualified names here but the namespace must be declared.
  my ($namespace, $baseName) = $self->_splitName($name);
  $self->_chkns($namespace);

  return 1;
}


# Copy all public names in namespace $src to namespace $dest
sub importPublic {
  my ($self, $src, $dest, $withNames, $withoutNames, $renameNames) = @_;

  $dest = $self->{' namespace'} unless $dest;

  $self->_chkns($src);
  $self->_chkns($dest);

  my @srcNames = ();
  foreach my $key (keys %{$self}) {
	next if $key =~ /^\s/;	# Skip internal variables

	# Skip existing imports
	next if defined($self->{' imported symbols'}->{$key});

	my ($namespace, $name) = $self->_splitName($key);
	next unless $namespace eq $src;
	next if $name =~ /^_/;

	my $newVar = "${dest}::${name}";
	if ($withNames) {
	  next unless exists($withNames->{$name});
	  $newVar = "${dest}::" . $withNames->{$name};
	} elsif ($withoutNames) {
	  next if exists($withoutNames->{$name});
	} elsif ($renameNames && defined($renameNames->{$name})) {
	  $newVar = "${dest}::" . $renameNames->{$name};
	}

	die "Importing name '$key' into '$dest' as '$newVar' overwrites existing " .
	  "name.\n"
	  if exists($self->{$newVar});

	$self->defsetconst($newVar, $self->{"${src}::${name}"});

	$self->{' imported symbols'}->{$newVar} = 1;
  }

  return;
}

# Test if $name is a qualified private symbol in any scope.
sub nameIsQualifiedPrivate {
  my ($self, $name) = @_;

  return $name =~ /[^:]\:\:_[^:]*$/;
}

# Test if $name is legally accessible from the current namespace.
sub isLegallyAccessible {
  my ($self, $name) = @_;

  my $ns = $self->getNamespace();
  return 1 unless $self->nameIsQualifiedPrivate($name);
  return $name =~ m{^${ns}};
}



# ---------------------------------------------------------------------------

package LL::Main;

use Term::ReadLine;
use Scalar::Util qw(looks_like_number);
use UNIVERSAL 'isa';		# Deprecated but I need it to identify LL::Objects
use Cwd qw{abs_path getcwd};
use File::Basename;

use constant NIL => LL::Nil::NIL;
use constant TRUE => LL::Number->new(1);

our $Input = undef;		# Input filehandle or undef for stdin.
my $NeedPrompt = 0;		# If true, reader is inside a LoL Line
my $Globals = LL::GlobalContext->new();

# ---------------------------------------------------------------------------

# Flags:
my $dumpExpr = 0;

use Getopt::Long;

{
  my $args = pullOutArgs();

  GetOptions ('dump-expr'			=> \$dumpExpr)
	or die "Invalid argument.\n";

  initGlobals($args);
  run();
  exit(0);
}


sub pullOutArgs {
  my $first = 0;
  for my $arg (@ARGV) {
	if ($arg eq '-' || $arg !~ /^\-\-/) {
	  last;
	}
	$first++;
  }

  my @result = @ARGV[$first .. $#ARGV];
  @ARGV = @ARGV[0..$first-1];

  unshift @result, '-' unless scalar @result > 0;

  return \@result;
}


# ---------------------------------------------------------------------------

# Run the command-line program or the REPL, depending.
sub run {

  # Case 1: file on command-line.
  my $argv0 = ${ $Globals->lookup('Sys::Argv0') };
  if ($argv0 ne '-') {
	readfile($argv0, 'Main', 0, 0);
	return;
  }

  # Otherwise, drop into the REPL
  while (1) {
	eval {
	  readfile('', 'Main', 0, 1);
	};
	
	last unless $@;
	print "Error: $@\n";
  }

  print "\n";
}


sub readfile {
  my ($file, $module, $checkName, $print) = @_;

  local $Input;		# Push for the life of this function
  if ($file) {
	open $Input, "<$file"
	  or die "Unable to open '$file'.";
  }

  my $oldNamespace;

  while (1) {
	$NeedPrompt = 1;		# We are at the start of a logical LoL line

	my $expr = readLoLLine(0);
	next if ($expr->isEmptyList());

	# If $file was loaded via a 'use' directive (setting $checkName to
	# 1), the first line MUST be a 'package' declaration and the
	# package's module must match '$module'.
	if ($checkName) {
	  checkPkgDecl($file, $module, $expr);

	  $oldNamespace = $Globals->getNamespace();
	  $Globals->importPublic('Lang', $module);

	  $checkName = 0;
	  next;
	}

	# Exit if the first (and only) element of $expr is EOF.  Ignore it
	# if EOF is at the end, since we'll see it again next iteration.
	last if ($expr->[0]->isEof());
	pop @{$expr} if $expr->[-1]->isEof();

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

  close $Input if $Input;
  $Globals->setNamespace($oldNamespace) if $oldNamespace;
}


sub checkPkgDecl {
  my ($file, $module, $expr) = @_;

  # Die unless the 'package' statement is here.
  die "First line of module file '$file' does not begin with a matching " .
	"package declaration.\n"
	  unless ($expr->isList() &&
			  $expr->[0]->isSymbol() &&
			  ${$expr->[0]} eq 'package');

  # Die unless the package declaration has one argument, a symbol.
  die "Malformed package declaration.\n"
	unless ($expr->size() == 2 && $expr->[1]->isSymbol());

  my $pkgName = ${$expr->[1]};

  # Die unless the package argument matches $module.
  die "Needed module '$module'; got '$pkgName' in file '@{[basename $file]}'\n"
	unless $module eq $pkgName;

  # Actually create the namespace and import 'Lang' into it.
  $Globals->defNamespace($pkgName);
  $Globals->setNamespace($pkgName);
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

	# The EOF is expected to be in the result
	last if $item->isEof();
  }

  # Create the result object
  my $rlist = LL::List->new(\@result);

  # See if this line could be treated as infix
  if ($rlist->couldBeImpliedInfix()) {
	$rlist = LL::InfixList->new(\@result)->asPrefixList();
  }

  # Strip out any operator escapes.  We no longer need them.
  $rlist->unescapeAllOperators();

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

  $tok->isInterpString() and do {
	return expandInterpString($tok);
  };

  ($tok->isLiteral() || $tok->isSymbol() || $tok->isEol() || $tok->isEof())
	and do {
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

	die "End of file inside an expression!\n" if $tok->isEof();

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

	die "File ends inside of a LoL.\n" if $line->[-1]->isEof();

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
		push @tokens, LL::Eof->new('');
		last;
	  }
	  chomp $line;

	  # If this is the start of a POD section, skip ahead to the end
	  if ($line =~ /^=\w+/) {
		my $podLine;
		do {
		  $podLine = getLine();
		  die "End-of-file inside a POD section.\n"
			unless defined($podLine);

		} while ($podLine !~ /^=cut\s/);

		next;
	  }

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

		# Floating-point literal
		$line =~ s/^(\-?[0-9][0-9_]*)\.([0-9_]*)(\W?)/$3/ and do {
		  $tok = "$1.$2";
		  $tok =~ s/_//g;
		  push @tokens, LL::Number->new($tok);
		  next;
		};

		# Hex literal
		$line =~ s/^(\-?)0x([0-9a-fA-F_]*)(\W?)/$3/ and do {
		  my ($sign, $num) = ($1, $2);
		  $num =~ s/_//g;
		  $tok = oct("0x$num");
		  $tok = -$tok if $sign eq '-';
		  push @tokens, LL::Number->new($tok);
		  next;
		};

		# Binary literal
		$line =~ s/^(\-?)0b([01_]*)(\W?)/$3/ and do {
		  my ($sign, $num) = ($1, $2);
		  $num =~ s/_//g;
		  $tok = oct("0b$num");
		  $tok = -$tok if $sign eq '-';
		  push @tokens, LL::Number->new($tok);
		  next;
		};

		# Decimal literal.  Perl's conversion to number ignores
		# leading zeroes
		$line =~ s/^(\-?\d[0-9_]*)(\W?)/$2/ and do {
		  my $num = $1;
		  $num =~ s/_//g;
		  $tok = $num + 0;
		  push @tokens, LL::Number->new($tok);
		  next;
		};

		# Empty single-quoted string is a special case, since there
		# needs to be an even number of quotes.
		$line =~ s/^(\'\')+([^\'])/$2/ and do {
		  push @tokens, LL::String->new("");
		  next;
		};
		
		$line =~ /^"/ and do {
		  my $string;
		  ($line, $string) = _readDoubleQuoteString($line);
		  push @tokens, LL::InterpString->new($string);
		  next;
		};

		$line =~ /^'/ and do {
		  my $string;
		  ($line, $string) = _readSingleQuoteString($line);
		  push @tokens, LL::String->new($string);
		  next;
		};

		my ($nline, $sym) = parseSymbol($line);
		defined($sym) and do {
		  push @tokens, LL::Symbol->new($sym);
		  $line = $nline;
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


  my $escapes;
  BEGIN {
	$escapes = {n => "\n",
				r => "\r",
				f => "\f",
				a => "\a",
				t => "\t",
			   '$'=> "\\\$",
			   '@'=> "\\\@"};
  };

  sub _readDoubleQuoteString {
	my ($line) = @_;

	my $eol = "Reached end-of-file inside a string constant.\n";
	die "WTF????" unless $line =~ /^\"/;

	$line = substr($line, 1);

	my $result = "";
	while (1) {

	  if ($line eq '') {
		$result .= "\n";

		$line = getLine();
		die $eol unless defined($line);

		chomp $line;

		next if $line eq '';
	  }

	  my $char = substr($line, 0, 1);
	  $line = substr($line, 1);

	  if ($char eq '\\') {
		my $nc = substr($line, 0, 1);
		$line = substr ($line, 1);

		defined ($escapes->{$nc}) and do {
		  $result .= $escapes->{$nc};
		  next;
		};

		$result .= $nc;
	  } elsif ($char eq '"') {
		return ($line, $result);
	  } else {
		$result .= $char;
	  }
	}
  }


  sub _readSingleQuoteString {
	my ($line) = @_;

	$line =~ s/^('+)//
	  or die "WTF???\n";
	my $delimCount = length($1);

	my $result = '';
	while (1) {

	  if ($line eq '') {
		$line = getLine();
		die "Reached end-of-file inside a string constant.\n"
		  unless defined($line);

		chomp $line;

		$line = "\n" . $line;
	  }

	  my $char = substr($line, 0, 1);
	  $line = substr($line, 1);

	  if ($char eq q{'}) {
		my $quotesFound = 1;
		
		while (1) {
		  $char = substr($line, 0, 1);
		  last unless $char eq q{'};

		  $line = substr($line, 1);

		  ++$quotesFound;
		}
		
		if ($quotesFound < $delimCount) {
		  $result .= q{'} x $quotesFound;
		} else {
		  $result .= q{'} x ($quotesFound - $delimCount);
		  return ($line, $result);
		}

	  } else {
		$result .= $char;
	  }
	}
  }

}


# Attempt to parse the start of $line as a symbol.  On success, return
# the modified $line and the symbol text; on failure, return false.
sub parseSymbol {
  my ($line) = @_;

  # Regexp to match operators: may begin with '\'; may not end with
  # '-' (to keep from interfering with trailing negative int) except
  # if it's the minus operator.
  my $OPER_REGEX = qr{\\? [-!@\$\%^&*+=?<>\/]+}x;

  # Regexp to parse a name segment.
  my $WRE = qr{(?: [a-zA-Z_] \w*)}x;

  # Regex to parse a complete name, global or local
  my $NSRE = qr{ (?: $WRE \:\:)+ }x;

  $line =~ s/^($NSRE? (?: $WRE | ${OPER_REGEX}))//x
	or return undef;

  return ($line, $1);
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


# Given a perl object or list of objects, convert it to the equivalent
# Deck types.
sub decktype {

  if (scalar @_ > 1) {
	my @result = map { decktype($_) } @_;
	return LL::List->new(\@result);
  }

  my ($arg) = @_;

  # Handle non-reference scalars.
  return NIL unless defined($arg);
  return LL::Number->new($arg) if looks_like_number($arg);
  return LL::String->new($arg) unless ref($arg);

  given (ref($arg)) {
	when ('SCALAR') {
	  return decktype(${$arg});
	}

	when ('ARRAY') {
	  return decktype(@{$arg});
	}

	default {
	  return LL::PerlObj->new($arg);
	}
  }
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
		join("\n", map { $_->storeStr()."\n" } @backtrace) . "\n";
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

  if (!$fn->isCallable()) {
	my $nm = $fname ? $fname : $fn->storeStr();

	# Give a more useful error message.
	die "Attempted to call macro '$nm' as a function.  (The macro was\n" .
	  "not defined when the expression was compiled).\n"
		if $fn->isMacro();

	die "Attempted to call non-function '$nm' as a function.\n";
  }
	

  return $fn->(@args);
}


# Given a string, split it into plain string chunks and the
# expressions to evaluate.  Expressions take the form of a list
# ref. containing the sigil as a string followed by the symbol name.
sub splitInterpString {
  my ($str) = @_;

  # First, divvy into vars and strings.
  my @parts = ("");

  while (1) {
	last unless length($str) > 0;

	$str =~ s/^( .*? ) ([\$\@])//x
	  or last;
	my ($leader, $sigil) = ($1, $2);

	# If the @ or $ was escaped, stick it onto $leader and try again.
	if ($leader =~ m{\\$} && $leader !~ m{\\\\$}) {
	  $leader =~ s{\\$}{};
	  $leader .= $sigil;
	  push @parts, $leader;
	  next;
	}

	push @parts, $leader;

	my $brace = ($str =~ s/^\{//);

	my ($nstr, $sym) = parseSymbol($str);
	die "Error parsing interpolated string at \"...$str\"\n"
	  unless defined($sym);
	$str = $nstr;

	if ($brace) {
	  $str =~ s/^\}//
		or die "Missing close brace in interpolated string by '$sym'\n";
	}

	push @parts, [$sigil, $sym];
  }

  push @parts, $str
	unless $str eq "";

  # Merge adjacent boring strings
  my @newParts = ();
  my $current = "";
  while (scalar @parts) {
	my $part = shift @parts;

	if (!ref($part)) {
	  $current .= $part;
	  next;
	}

	if ($current ne '') {
	  push @newParts, $current;
	  $current = "";
	}

	push @newParts, $part;
  }

  push @newParts, $current
	if $current ne '';

  return \@newParts;
}


sub expandInterpString {
  my ($expr) = @_;

  my $parts = splitInterpString (${$expr});

  # If $expr turns out to be a single string with nothing to
  # interpolate, just return that.
  if (scalar @{$parts} == 1 && !ref($parts->[0])) {
	return LL::String->new($parts->[0]);
  }

  # Otherwise...
  my @result = (LL::Symbol->new('_::mkstr'));

  for my $part (@{$parts}) {
	!ref($part) and do {
	  push @result, LL::String->new(\$part);
	  next;
	};

	my ($sigil, $name) = @{$part};
	my $nameSym = LL::Symbol->new($name);

	if ($sigil eq "\@") {
	  push @result, LL::List->new([LL::Symbol->new('_::mkstr_all'), $nameSym]);
	} else {
	  push @result, $nameSym;
	}
  }

  return LL::List->new(\@result);
}


# ---------------------------------------------------------------------------


# Check for accesses to private variables in other modules.
sub checkForScopeViolations {
  my ($expr, $name) = @_;

  if ($expr->isSymbol()) {
	my $ns = $Globals->getNamespace();
	die "Use of a qualified private name ('${$expr}') in function '$name'\n"
	  unless $Globals->isLegallyAccessible(${$expr});

	return;
  }

  return unless $expr->isList();

  # Assignments are a special case
  checkForScopeViolations($expr->[1]->value(), $name)
	if ($expr->[0]->isSymbol() &&
		${$expr->[0]} eq '_::set' &&
		$expr->[1]->isQuote);

  for my $elem (@{$expr}) {
	checkForScopeViolations($elem, $name);
  }
}



# Return a blessed func. ref which executes a sub with $args and $body
# in the given context.  If $context is undef, the $Global context is
# used, allowing the function to define and set global variables.
# $name is used for error messages and may be omitted.
sub compile {
  my ($outerContext, $args, $body, $mode, $name) = @_;

  $name ||= 'unnamed function';

  my $isMacro = 0;
  if ($mode eq 'macro') {
	$isMacro = 1;
	$mode = 'proc';
  }

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
	  my $newExpr = applyMacrosRecursively ($expr, $macroContext);

	  checkForScopeViolations($newExpr, $name);

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
	  $context->defset (${$arg}, shift @_ );
	}

	# Bind varargs
	if ($isVararg) {
	  my $args = LL::List->new([@_]);

	  $context->defset('args', $args);
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

	# Procs return NIL by default.  Only explicit returns return a
	# value.
	return NIL if ($mode eq 'proc' && !$@);

	return $lastexpr;
  };

  return $isMacro ? LL::Macro->new($fn) : LL::Function->new($fn);
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

# Ensure that argument types of $args matches $types.
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

# Ensure that the number of elements in $args is one of the numbers
# give in @counts.
sub checkNargs {
  my ($args, @counts) = @_;

  for my $count (@counts) {
	return if scalar @{$args} == $count;
  }

  die "Expecting $counts[0] arguments; got @{[scalar @counts]}.\n"; 
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

# Produce a sub from a list.  List may be either a LoL or a single
# quoted expression.  If $expr is not a list, it is returned
# unchanged.  @args is the list of formal arguments.  If it contains
# only a number, this is the number of single-letter arguments
# automatically created.
sub subify {
  my ($expr, @args) = @_;
  my $body;

  if ($expr->isLoL()) {
	$body = $expr;
  } elsif ($expr->isList()) {
	my $outer = LL::List->new([$expr]);
	$body = LL::Quote->new($outer);
  } else {
	return $expr;
  }

  my $arglist;
  if (scalar @args == 0) {
	# Case 1: No args? Just make a zero-arg sub
	$arglist = LL::Quote->new( LL::List->new([]) );
  } elsif (scalar @args == 1 && looks_like_number($args[0])) {
	# Case 2: Just a number.  In this case, generate an arg list

	my $nargs = $args[0];
	die "Invalid argument count: $nargs\n"
	  if $nargs < 0 || $nargs > 26;

	my @autoArgs;
	for my $letter ('a' .. 'z') {
	  last unless $nargs--;
	  push @autoArgs, LL::Symbol->new($letter);
	}
	$arglist = LL::Quote->new( LL::List->new(\@autoArgs) );
  } else {
	# Case 3: we have a list of args.
	map { $_->checkSymbol() } @args;
	$arglist = LL::Quote->new( LL::List->new(\@args) );
  }

  return  LL::List->new([	LL::Symbol->new('_::sub'),
							$arglist,
							$body
						]);
}


sub subifyStrict {
  my ($expr, @args) = @_;

  my $result = subify($expr, @args);
  return $result if $result != $expr;

  my $bs = $expr->storeStr();
  die "Expecting a single expression or quoted list of list. Got '$bs'\n";
}


sub quoteIfSym {
  my ($sym, $strict) = @_;

  $sym->checkSymbol(" in mproc argument.") if $strict;
  return $sym unless $sym->isSymbol();
  return LL::Quote->new($sym);
}


sub quoteIfList {
  my ($ls, $strict) = @_;

  $ls->checkList(" in mproc argument.") if $strict;
  return $ls unless $ls->isList();
  return LL::Quote->new($ls);
}


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


sub macro_mproc {
  my ($mproc, $name, $args, $body) = @_;

  $name->checkSymbol();
  my $qname = LL::Quote->new($name);

  return LL::List->new([LL::Symbol->new("_::mproc"),
						$qname,
						$args,
						$body]);
}


sub macro_macro {
  my ($macro, $name, $args, $body) = @_;

  $name->checkSymbol();
  $args = fixFormalArgs($args);
  $body->checkLoL();

  return LL::List->new([LL::Symbol->new('_::macro'),
						LL::Quote->new($name),
						$args,
						$body]);
}


sub launder_varconst {
  my ($isConst, @macroArgs) = @_;

  my @args;
  if (scalar @macroArgs == 1 && $macroArgs[0]->isLoL()) {
	my $argList = shift @macroArgs;
	@args = @{ $argList->value() };
  } else {
	my $argList = LL::List->new(\@macroArgs);

	if ($argList->couldBeImpliedInfix()) {
	  $argList = $argList->parsedAsInfix();
	}

	push @args, $argList;
  }

  my @result = ();
  for my $decl (@args) {
	$decl->checkList();
	next unless scalar @{$decl};

	$decl->[0]->checkSymbol(" instead of '=' in var or const declaration.");

	if (${ $decl->[0] } eq '=') {
	  push @result, LL::Quote->new($decl->[1]), $decl->[2];
	  next;
	}

	for my $word (@{ $decl }) {
	  $word->checkSymbol(" (@{[$word->printStr()]})");
	  die "Const '${$word}' declared without a value.\n" if $isConst;

	  push @result, LL::Quote->new($word), NIL;
	}
  }

  return \@result;
}


# Macro to handle 'var' and 'const'
sub macro_varconst {
  my ($name, @args) = @_;

  my $isConst = (${$name} eq 'const');

  my $result = launder_varconst($isConst, @args);
  unshift @{ $result }, LL::Symbol->new($isConst ? '_::const' : '_::var');

  return LL::List->new($result);
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

#   my ($if, $cond, $trueBlock, $else, $falseBlock) = @_;

#   if (defined($else)) {
# 	if ($else->isList()) {
# 	  die "Malformed 'if' statement.\n"
# 		unless !defined($falseBlock);
# 	  $falseBlock = $else;
# 	} else {
# 	  die "Malformed 'if': expecting 'else', got @{[$else->printStr()]}\n"
# 		unless ($else->isSymbol() && ${$else} eq 'else');
# 	}
#   }

#   my $result = [LL::Symbol->new('_::if')
#			   ];
	
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
	$args[$i] = subifyStrict($args[$i]);
  }

  # Add the empty else clause
  push @args, NIL
	if scalar @args == 3;

  return LL::List->new(\@args);


}


sub macro_whilefn {
  my ($while, $cond, $body) = @_;

  die "Expecting 2 arguments to 'while'; got @{[scalar @_ - 1]}\n"
	unless scalar @_ == 3;

  my @result = (LL::Symbol->new('_::while'), subifyStrict($cond), subifyStrict($body));
  return LL::List->new(\@result);
}

sub macro_foreachfn {
  my ($foreach, $var, $in, $list, $body) = @_;

  die "Malformed 'foreach' macro call: expecting 4 arguments, got " .
	"@{[scalar @_ - 1]}.\n"
	  unless scalar @_ == 5;

  $var->checkSymbol();
  $in->checkSymbol();

  die "Malformed 'foreach': expecting 'in', got '@{[$in->printStr()]}'.\n"
	unless ${$in} eq 'in';

  my @result = (LL::Symbol->new('_::foreach'),
				$list,
				subifyStrict($body, $var)
			   );
  return LL::List->new(\@result);
}

sub macro_mapfn {
  my ($map, $fn, $list) = @_;

  return LL::List->new( [LL::Symbol->new('_::map'),
						 subifyStrict($fn, 1),
						 $list] );
}


# 'package' always gets consumed by 'readfile'.
sub macro_packagefn {
  die "Attempted to declare a package inside an existing package.\n";
}


# The 'use' macro.
sub macro_usefn {
  my ($use, $pkg, $with, $moduleList) = @_;

  die "Mangled 'use' statement.\n"
	if (scalar @_ < 2 || scalar @_ == 3 || scalar @_ > 4);

  $pkg->checkSymbol();

  my ($modifier, $symbols) = (NIL, NIL);

  if (scalar @_ == 4) {
	$modifier = $with;
	$symbols = $moduleList;
  }

  return LL::List->new([LL::Symbol->new('_::use'),
						LL::Quote->new($pkg),
						LL::Quote->new($modifier),
						$symbols])
}


sub macro_perlproc {
  my ($perlproc, $name, $args, $body) = @_;

  die "Expecting 3 arguments to 'perlproc', got @{[scalar @_ - 1]}\n"
	unless scalar @_ == 4;

  $name->checkSymbol(" in 'perlproc'");
  $args = fixFormalArgs($args);
  return LL::List->new([LL::Symbol->new('_::perlproc'),
						LL::Quote->new($name),
						$args,
						$body]);
}


sub macro_perluse {
  my ($perluse, $name) = @_;

  die "Expecting 1 arguments to perluse; got @{[scalar @_]}\n"
	unless scalar @_ == 2;

  $name->checkSymbol(" in 'perluse'");
  return LL::List->new([LL::Symbol->new('_::perluse'),
						LL::Quote->new($name)]);

}


# ---------------------------------------------------------------------------
sub initGlobals {
  my ($args) = @_;

  # Create default namespaces
  for my $ns (qw{_ __ Main Lang Sys}) {
	$Globals->defNamespace($ns);
  }

  # Set the initial load path
  $Globals->defset('Sys::ModPath', mkModPath());

  # Set the script path and arguments
  {
	my @deckArgs = map { LL::String->new($_) } @{$args};

	$Globals->defset('Sys::Argv0', shift @deckArgs);
	$Globals->defset('Sys::Argv', LL::List->new(\@deckArgs));
  }

  # All unqualified names defined here go to Lang.
  $Globals->setNamespace('Lang');


  $Globals->defsetconst ('nil', NIL);

  # Externally-defined primitive functions
  for my $special (
				   ['println',		\&builtin_println],
				   ['puts',			\&builtin_println],
				   ['storestr',		\&builtin_storestr],
				   ['show',			\&builtin_show],
				   ['_::proc',		\&builtin_proc],
				   ['_::sub',		\&builtin_subfn],
				   ['_::if',		\&builtin_iffn],
				   ['_::while',		\&builtin_whilefn],
				   ['_::set',		\&builtin_set],
				   ['_::var',		\&builtin_var],
				   ['_::const',		\&builtin_const],
				   ['_::atput',		\&builtin_atput],
				   ['atput',		\&builtin_atput],
				   ['_::map',		\&builtin_mapfn],
				   ['_::foreach',	\&builtin_foreachfn],
				   ['_::macro',		\&builtin_macro],
				   ['_::mproc',		\&builtin_mproc],
				   ['_::mkstr',		\&builtin_mkstr],
				   ['_::mkstr_all',	\&builtin_mkstr_all],
				   ['_::use',		\&builtin_usefn],
				   ['_::perlproc',	\&builtin_perlproc],
				   ['_::perluse',	\&builtin_perluse],
				   ['apply',		\&builtin_apply],
				   ['intern',		\&builtin_intern],
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
  prim2 '===',			sub { return boolObj($_[0] == $_[1])};
  prim2 '==',			sub { return $_[0]->equals($_[1]) };
  prim2 'list',			sub { return LL::List->new(\@_) };
  prim2 'val',			sub { return NIL unless scalar @_; return $_[-1] };
  prim2 '@',			sub { my ($l, $ndx) = @_; return $l->at($ndx) };
  prim2 'size',			sub { my ($l) = @_; return LL::Number->new($l->size())};
  prim2 'byteArray',	sub { return LL::ByteArray->new(@_) };
  prim2 'bytesSized',	sub { my ($size) = @_;
							  $size->checkNumber();
							  die "Invalid byteArray size: ${$size}\n"
								unless ${$size} > 0;
							  return LL::ByteArray->newSized(${$size})
							};
  prim2 'die',			sub { die join("", map { $_->printStr() } @_) . "\n" };
  prim2 'listSized',	sub { my ($size) = @_;
							  $size->checkNumber();
							  die "Invalid list size: ${$size}\n"
								unless ${$size} > 0;
							  return LL::List->new([(NIL) x ${$size}]);
							};
  prim2 '_::defns',		sub { my ($ns) = @_;
							  $ns->checkSymbol();
							  $Globals->defNamespace(${$ns});
							};
  prim2 'defined',		sub { my ($name) = @_;
							  $name->checkSymbol(" in 'defined'");
							  return NIL
								unless $Globals->isLegallyAccessible(${$name});
							  return $Globals->present(${$name}) ? TRUE:NIL;
							};
  prim2 'lookup',		sub { my ($name) = @_;
							  $name->checkSymbol(" in 'lookup'");
							  die "Unknown or inaccessable symbol: ${$name}\n"
								unless $Globals->isLegallyAccessible(${$name});
							  return $Globals->lookup(${$name});
							};

  # Macros
  macro 'var',			\&macro_varconst;
  macro 'const',		\&macro_varconst;
  macro 'proc',			\&macro_proc;
  macro 'mproc',		\&macro_mproc;
  macro 'set',			\&macro_assign;
  macro '=',			\&macro_assign;
  macro 'sub',			\&macro_subfn;
  macro 'if',			\&macro_iffn;
  macro 'while',		\&macro_whilefn;
  macro 'foreach',		\&macro_foreachfn;
  macro 'map',			\&macro_mapfn;
  macro 'macro',		\&macro_macro;
  macro 'mproc',		\&macro_mproc;
  macro 'package',		\&macro_packagefn;
  macro 'use',			\&macro_usefn;
  macro 'perlproc',		\&macro_perlproc;
  macro 'perluse',		\&macro_perluse;

  # Finally, switch to Main and import all public system names.
  $Globals->importPublic('Lang', 'Main');
  $Globals->setNamespace('Main');
}


# Create the module path.  To do: make this reasonable.
sub mkModPath {
  my @path = ();

  push @path, getcwd();

  my $binpath = dirname(abs_path($0)) . "/lib/";
  push @path, $binpath if -d $binpath;

  @path = map { LL::String->new($_) } @path;
  return LL::List->new(\@path);
}


sub builtin_println {
  for my $obj (@_) {
	die "Not an object: '$obj'\n"
	  unless (ref($obj) && isa($obj, 'LL::Object'));
	print $obj->printStr();
  }
  print "\n";

  return NIL;
}

sub builtin_storestr {
  my $result = "";

  for my $obj (@_) {
	die "Not an object: '$obj'\n"
	  unless (ref($obj) && isa($obj, 'LL::Object'));
	$result .= $obj->storeStr();
  }

  return LL::String->new($result);
}

sub builtin_show {
  my $result = builtin_storestr(@_);
  print ${$result}, "\n";
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
  my ($context, @argPairs) = @_;

  while (@argPairs) {
	my $name = shift @argPairs;
	my $value = shift @argPairs;

	$name->checkSymbol(" in 'var' argument.");
	$context->defset(${$name}, $value);
  }

  return NIL;
}

sub builtin_const {
  my ($context, @argPairs) = @_;

  while (@argPairs) {
	my $name = shift @argPairs;
	my $value = shift @argPairs;

	$name->checkSymbol(" in 'const' argument.");
	$context->defsetconst(${$name}, $value);
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

  my $func = compile ($Globals, $args, $body, 'proc', ${$name});

  $Globals->defset(${$name}, $func);

  return $func;
}


# Given an mproc argument list ($args), produce a matching array of
# arrays of functions (one per arg.) which performs the necessary
# transformations on the matching argument.
sub mk_mproc_macro_argfilter {
  my ($argList) = @_;

  my @filters = ();
  for my $arg (@{$argList}) {
	my @argFilter = ();

	$arg->checkList(" in mproc argument list.");

	my $argName = pop @{$arg};
	$argName->checkSymbol(" in mproc argument.");
	$argName = ${$argName};

	die "Invalid mproc argument name '$argName'\n"
	  if $argName =~ /^(sub|sym|list|strict)$/;

 	my $strict = (scalar @{$arg} > 0 && ${$arg->[0]} eq 'strict');
 	shift @{$arg} if $strict;

	my $mod = LL::Symbol->new('');
	$mod = shift @{$arg} if scalar @{$arg};
	$mod->checkSymbol() if $mod;

	given (${$mod}) {
	  when ("sub") {
		my $nargs = 0;
		if (scalar @{$arg} > 0 && $arg->[0]->isNumber()) {
		  $nargs = ${ shift @{$arg} };
		}

		die "Invalid arg count in mproc: $nargs -- must be between 0 and 26\n"
		  if ($nargs < 0 || $nargs > 26);

		if ($strict) {
		  push @argFilter, sub {subifyStrict(shift, $nargs)};
		} else {
		  push @argFilter, sub {subify(shift, $nargs)};
		}
	  }

	  when ("symbol") {
		push @argFilter, sub {quoteIfSym(shift, $strict)};
	  }

	  when ("list") {
		push @argFilter, sub {quoteIfList(shift, $strict)};
	  }

	  when ('') {
		# no modifiers.  Carry on.
	  }

	  default {
		die "Invalid mproc argument '${$mod}'\n" if $mod ne '';
#		die "mproc argument contains 'strict' by itself.\n" if $strict;
	  }
	}

	push @filters, \@argFilter;
  }

  return \@filters;
}


# Return a macro (i.e. blessed Perl sub) which performs mproc argument
# munging on its argument list.  $args is dismantled.
sub mk_mproc_macro {
  my ($name, $args) = @_;

  my $resultName = LL::Symbol->new("__::${$name}");

  my $filters = mk_mproc_macro_argfilter($args);

  my $macro = sub {
	my ($givenName, @args) = @_;
	my @newArgs = ();
	die "Arg count mismatch in call to mproc '$givenName'\n"
	  unless scalar @args == scalar @{$filters};

	push @newArgs, $resultName;

	for my $filterList (@{$filters}) {
	  my $arg = shift @args;

	  for my $filter (@{$filterList}) {
		$arg = $filter->($arg);
	  }

	  push @newArgs, $arg;
	}

	return LL::List->new(\@newArgs);
  };

  return LL::Macro->new($macro);
}



sub builtin_mproc {
  my ($name, $args, $body) = @_;

  # Argument checking.
  die "'mproc' expects 3 arguments: got @{[scalar @_]}\n"
	unless scalar @_ == 3;

  $name->checkSymbol();
  $args->checkList(" for mproc argument list.");
  $body->checkList();

  # Ensure that this macro hasn't already been defined.
  die "Redefinition of macro '${$name}'.\n"
	if ($Globals->present(${$name}) && $Globals->{${$name}}->isMacro());

  # Gather the argument names from $args before mk_mproc_macro
  # disassembles it.  (mk_mproc_macro does a lot of sanity checking so
  # we don't need to do it here.)
  my @pargs = ();
  for my $elem (@{$args}) {
	$elem->checkList(" in mproc argument.");
	my $argName = $elem->[-1];
	push @pargs, $argName;
  }

  # Create the wrapper macro.
  my $macro = mk_mproc_macro($name, $args);

  # Create the function to call.
  my $procArgs = LL::List->new(\@pargs);
  my $proc = builtin_proc (LL::Symbol->new("__::${$name}"), $procArgs, $body);

  # Give the macro a name.
  $Globals->defset(${$name}, $macro);

  return $proc;
}



sub builtin_macro {
  my ($name, $args, $body) = @_;

  # Argument checking.
  die "'macro' expects 3 arguments: got @{[scalar @_]}\n"
	unless scalar @_ == 3;

  $name->checkSymbol();
  $args->checkList();
  $body->checkList();

  my $macro = compile ($Globals, $args, $body, 'macro', ${$name});

  $Globals->defset(${$name}, $macro);

  return $macro;
}


sub builtin_subfn {
  my ($context, $args, $body) = @_;

  checkNargs(\@_, 3);

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

  checkNargs(\@_, 2);

  my $result = NIL;
  while ($test->()->isTrue()) {
	$result = $body->();
  }

  return $body;
}


sub builtin_mapfn {
  my ($fn, $list) = @_;

  checkNargs(\@_, 2);

  $list->checkIndexable();
  $fn->checkFun();

  my @result = ();
  for my $index (0 .. $list->size() - 1) {
	my $item = $list->at(LL::Number->new($index));
	push @result, $fn->($item);
  }

  return LL::List->new(\@result);
}


sub builtin_foreachfn {
  my ($list, $fn) = @_;

  checkNargs(\@_, 2);

  $list->checkIndexable();
  $fn->checkFun();

  for my $index (0 .. $list->size() - 1) {
	my $item = $list->at(LL::Number->new($index));
	$fn->($item);
  }

  return NIL;
}


# Given a list of arguments, call each of their printStr() methods,
# append the result and return it.  This gets created from
# double-string literals.
sub builtin_mkstr {
  my @strings = map { $_->printStr() } @_;
  my $result = join("", @strings);
  return LL::String->new(\$result);
}

# Given a single list of objects, call their printStr methods and
# return the results concatenated together and separated by a single
# space.
sub builtin_mkstr_all {
  my ($args) = @_;

  checkNargs(\@_, 1);

  my @strings = map { $_->printStr() } @{$args};
  my $result = join(" ", @strings);
  return LL::String->new(\$result);
}


sub _findFileFor {
  my ($moduleName) = @_;

  my $modPath = $Globals->lookup('Sys::ModPath');
  die "Unable to find a usable Sys::ModPath\n"
	unless ($modPath && $modPath->isList());

  my @fileParts = split (/\:\:/, $moduleName);

  my $filename = pop @fileParts;
  $filename .= '.dk';

  my @searchDir=map { $_->checkString(" in Sys::ModPath"); ${$_} } @{$modPath};

  for my $baseDir (@searchDir) {

	my $path = $baseDir;
	for my $dirPart (@fileParts) {
	  $path .= "/$dirPart";
	}

	$path .= "/$filename";
	return $path if -f $path;
  }

  return;
}


# Given the body of a use 'with/without/rename' clause, produce a hash
# mapping the given names to their new names if specified or the
# original name.
sub _getImportNameList {
  my ($list, $with) = @_;
  my %result = ();

  for my $sublist (@{$list}) {
	$sublist->checkList(" in '$with' clause element.");
	($sublist->size() == 1 || $sublist->size() == 3)
	  or die "Empty list as '$with' clause element.\n";

	my $sym = $sublist->[0];
	$sym->checkLocalName(" as imported symbol.");

	my $newName = $sym;
	if ($sublist->size() == 3) {
	  my $asn = ${ $sublist->[1] };
	  die "Malformed rename expression for '${$sym}': expecting '=>', "
		. "got '$asn'.\n"
		unless ($asn eq '=>');

	  $newName = $sublist->[2];
	  $newName->checkLocalName("as rename target for '${$sym}'.");
	}

	$result{${$sym}} = ${$newName};
  }

  return \%result;
}


sub builtin_usefn {
  my ($moduleName, $with, $list) = @_;

  checkNargs(\@_, 3);

  $moduleName->checkSymbol();

  # Import the file
  my $mn = ${$moduleName};
  my $path = _findFileFor($mn);
  die "Unable to find module '$mn'\n"
	unless $path;

  if (!$Globals->hasNamespace($mn)) {
	my $currModule = $Globals->getNamespace();

	readfile($path, $mn, 1, 0);

	$Globals->setNamespace($currModule);
  }

  # If requested, construct the hash of names to copy over.
  my $names;
  if (!$with->isNil()) {
	$with->checkSymbol();
	$list->checkList(" in '_::use'.");

	$names = _getImportNameList($list, $with);
  }

  my ($withSet, $withoutSet, $renameSet);

  given(${$with}) {
	when ('with')	{$withSet = $names}
	when ('without'){$withoutSet = $names}
	when ('rename') {$renameSet = $names}
	when ('') {}
	default {die "Invalid 'use' modifier clause: '${$with}'\n"}
  }

  $Globals->importPublic($mn, $Globals->getNamespace(), $withSet,
						 $withoutSet, $renameSet);
}


# Eval the first argument and return the result.  Used to compile Perl
# code while minimizing the chance that it will interfere with this
# program.
sub strEval {
  return eval "package LL::USER; $_[0]";
}

sub builtin_perlproc {
  my ($name, $args, $bodyStr) = @_;

  checkNargs(\@_, 3);

  $args->checkList();
  $name->checkSymbol();
  $bodyStr->checkString();

  my $perlArgs = "";

  if ($args->size() > 0) {
	$perlArgs .= 'my (';
	for my $a (@{$args}) {
	  $a->checkSymbol(" in perlproc argument.");
	  $perlArgs .= '$' . ${$a} . ',';
	}

	$perlArgs .= ') = map { $_ && $_->perlForm() } @_;' . "\n";
  }

  my $fn = 'sub{' . $perlArgs . ${$bodyStr} . '}';

  my $sub = strEval($fn);
  die "Error: $@\nCompiling perlsub:\n'''\n$fn\n'''\n"
	if ($@ || ref($sub) ne 'CODE');

  my $proc = sub {return decktype($sub->(@_))};

  return $Globals->defset(${$name}, LL::Function->new($proc));
}



sub builtin_perluse {
  my ($moduleSym) = @_;

  checkNargs(\@_, 1);

  $moduleSym->checkSymbol();

  my $mod = ${$moduleSym};

  # Prevent Bobby Tables-type errors.
  die "Invalid module name '$mod'\n"
	if ($mod =~ /[^a-zA-Z0-9_:]/);

  eval "require $mod;";
  die "Error loading module $mod: $@\n"
	if $@;

  return NIL;
}


sub builtin_apply {
  my ($fun, $args) = @_;

  checkNargs(\@_, 2);

  die "Expecting 2 args, got @{[scalar @_]}\n"
	unless scalar @_ == 2;

  $fun->checkFun(" in 'apply'");
  $args->checkList(" in 'apply'");

  return $fun->(@{$args});
}

sub builtin_intern {
  my ($string) = @_;

  checkNargs(\@_, 1);
  $string->checkString(" in 'intern'");

  return LL::Symbol->new(${$string});
}
