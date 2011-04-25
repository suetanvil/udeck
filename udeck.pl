#!/usr/bin/perl

# Interpreter for a minimal implementation of Deck


use strict;
use warnings;

use feature "switch";

# Forward-declare NIL as an alias for LL::Nil::NIL.
sub NIL {return LL::Nil::NIL()}

# ---------------------------------------------------------------------------

package LL::Context;

sub new {
  my ($class, $parent) = @_;
  return bless {
				# Reserved fields:
				' parent'		=> $parent,
				' consts'		=> {},		# <- list of const names
				' namespace'    => undef,	# The current namespace
			   }, $class;
}

sub isQualified {
  my ($self, $name) = @_;

  return $name =~ /\:\:/;
}


# Test if $name is a qualified private symbol in any scope.
sub nameIsQualifiedPrivate {
  my ($self, $name) = @_;

  return $name =~ /[^:]\:\:_[^:]*$/;
}

# Test if $name is legally accessible from the current scope.
sub isLegallyAccessible {
  my ($self, $name) = @_;

  my $ns = $self->getNamespace();
  return 1 unless $self->nameIsQualifiedPrivate($name);
  return $name =~ m{^${ns}};
}

sub deferToGlobal {my ($self, $name) = @_; return $self->isQualified($name)}

# Defer to the master list of namespaces in $Globals
sub _chkns {
  my ($self, $ns) = @_;
  $self->{' parent'}->_chkns($ns);
}

# Set default namespace.  This overrides the parent's.
sub setNamespace {
  my ($self, $ns) = @_;
  $self->_chkns($ns);
  $self->{' namespace'} = $ns;
  return;
}

# Get the namespace $self was defined in.  This may be inherited from
# the parent.
sub getNamespace {
  my ($self) = @_;
  return $self->{' namespace'}
	if defined($self->{' namespace'});

  return $self->{' parent'}->getNamespace();
}


sub checkScopeFor {
  my ($self, $name) = @_;

  die "Qualified name '$name' defined in local context.\n"
	if $self->isQualified($name);

}

# Ensure $name is a valid Deck variable
sub checkName {
  my ($self, $name) = @_;

  (LL::Symbol->new($name))->checkValidName(" as variable name.");
}

sub def {
  my ($self, $name) = @_;

  die "Expecting string, not reference!\n" unless ref($name) eq '';

  $self->checkName($name);
  $self->checkScopeFor($name);

  return $self->{$name} = main::NIL;
}

sub set {
  my ($self, $name, $value) = @_;

  die "Expecting string, not reference!\n" unless ref($name) eq '';

  $name = $self->findFullNameOrDie($name);

  die "Attempted to modify a const: $name.\n"
	if defined($self->{' consts'}->{$name});

  exists($self->{$name}) and do {
	$self->{$name} = $value;
	return $value;
  };

  defined($self->{' parent'}) and return $self->{' parent'}->set($name, $value);

  die "Unknown variable: '$name'\n";	# not reached
}

sub defset {
  my ($self, $name, $value) = @_;

  $self->def($name);
  $self->set($name, $value);

  return $value;
}

sub defsetconst {
  my ($self, $name, $value) = @_;

  $self->defset($name, $value);
  $self->{' consts'}->{$name} = 1;

  return $value;
}

sub lookup {
  my ($self, $name) = @_;

  $name = $self->findFullNameOrDie($name);

  exists($self->{$name})      and return $self->{$name};
  defined($self->{' parent'}) and return $self->{' parent'}->lookup($name);

  die "Unknown variable: '$name'\n";	# not reached.
}

sub present {
  my ($self, $name) = @_;

  return !! $self->findFullName($name);
}


# Test for the presence of $name without qualifying the name
sub presentAsIs {
  my ($self, $name) = @_;

  exists($self->{$name}) and return 1;
  defined($self->{' parent'})
	and return $self->{' parent'}->presentAsIs($name);

  return 0;
}


# Return the most qualified form of $name, as defined.  If $name is
# undefined, returns undef.
sub findFullName {
  my ($self, $name) = @_;

  # Case 1: it's already qualified.  In this case, just check that
  # it's defined somewhere.
  if ($self->isQualified($name)) {
	return unless $self->presentAsIs($name);
	return $name;
  }

  # Case 2: It's stored in a non-global scope.
  return $name if $self->presentAsIs($name);

  # Case 3: It's a global
  $name = $self->getNamespace() . '::' . $name;
  return $name if $self->present($name);

  # Otherwise, it's undefined.
  return;
}

# Perform findFullName on $name and die if it is undefined.
sub findFullNameOrDie {
  my ($self, $name) = @_;

  my $fullName = $self->findFullName($name);
  die "Unknown variable: '$name'\n"
	unless defined($fullName);

  return $fullName;
}




package LL::GlobalContext;
use base 'LL::Context';

sub new {
  my $self = LL::Context::new(@_);
  $self->{' namespaces'} = {};			# The set of declared namespaces
  $self->{' imported symbols'} = {};	# The set of names that are imports

  return $self;
}

sub _chkns {
  my ($self, $ns) = @_;
  die "Undefined namespace '$ns'\n"
	unless defined($self->{' namespaces'}->{$ns});
}


sub defNamespace {
  my ($self, $ns) = @_;
  $self->checkName($ns);
  $self->{' namespaces'}->{$ns} = 1;
}

sub hasNamespace {
  my ($self, $ns) = @_;
  return exists($self->{' namespaces'}->{$ns});
}


sub _normalizeName {
  my ($self, $name) = @_;

  die "Expecting string, not reference!\n" unless ref($name) eq '';
  return $name if $self->isQualified($name);
  return $self->getNamespace() . '::' . $name;
}


# Overridden access: ensure all names are normalized.
sub def {
  my ($self, $name) = @_;
  return $self->SUPER::def($self->_normalizeName($name));
}

sub set {
  my ($self, $name, $value) = @_;
  return $self->SUPER::set($self->_normalizeName($name), $value);
}

sub defset {
  my ($self, $name, $value) = @_;
  return $self->SUPER::defset($self->_normalizeName($name), $value);
}

sub defsetconst {
  my ($self, $name, $value) = @_;
  return $self->SUPER::defsetconst($self->_normalizeName($name), $value);
}

sub lookup {
  my ($self, $name) = @_;
  return $self->SUPER::lookup($self->_normalizeName($name));
}

sub present {
  my ($self, $name) = @_;
  return $self->SUPER::present($self->_normalizeName($name));
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
sub checkScopeFor {
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
sub checkQtLoL  {die "Expected quoted LoL, got @{[ref(shift)]}@_\n"}
sub checkLoL    {die "Expected LoL, got @{[ref(shift)]}@_\n"}
sub checkFun	{die "Expected function, got @{[ref(shift)]}@_\n"}
sub checkClass	{die "Expected class, got @{[ref(shift)]}@_\n"}
sub checkStruct	{die "Expected struct, got @{[ref(shift)]}@_\n"}
sub checkLocalName {die "Expected unqualified name, got @{[ref(shift)]}@_\n"}
sub checkValidName {die "Expected valid name, got @{[ref(shift)]}@_\n"}
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
sub isByteArray {return 0}
sub isInfixList {return 0}
sub isQuote {return 0}
sub isNil {return 0}
sub isMacro {return 0}
sub isFunction {return 0}
sub isCallable {return 0}
sub isTrue {return 1}
sub isNumber {return 0}
sub isQtLoL {return 0}	# Is a quoted list containing only lists
sub isQtList {return 0}	# Is a quoted list?
sub isLoL {return 0}	# Is an list containing only lists
sub isPerlObj {return 0}
sub isClass {return 0}
sub isStruct {return 0}
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

sub checkIndexable {
  my ($self) = @_;
  die "Expecting indexable type, got @{[ref($self)]}\n"
	unless $self->isIndexable();
}


# Return the corresponding Deck class for $self.  This won't work
# until initGlobals has been called.
{
  my %builtinClasses;		# Registry of built-in classes

  sub class {
	my ($self) = @_;

	my $name = ref($self);
	$name =~ s/^LL:://
	  or die "Invalid class name '$name'\n";

	return $builtinClasses{$name};
  }

  # Register $deckClass as a builtin class so that 'class' can find
  # it.
  sub registerBuiltin {
	my ($class, $name, $deckClass) = @_;

	$builtinClasses{$name} = $deckClass;
  }

  # Force a method cache refresh of all classes.
  sub refreshAllBuiltinClassMethodCaches {
	my ($class) = @_;

	for my $cl (values %builtinClasses) {
	  $cl->refreshCache();
	}
  }
}

# Call deck method named by $name on $self.  All arguments must go
# through decktype correctly.  The result is still a deckType
sub deckCall {
  my ($self, $name, @args) = @_;

  @args = map { LL::Main::decktype($_) } @args;

  my $class = $self->class();
  $class or die "deckCall on internal class.\n";

  my $method = $class->lookup($name);
  my $result = $method->($self, @args);
  return $result;
}

# Builtin-type behaviours
sub isIndexable {my ($self) = @_;
				 $self->deckCall('isIndexable_get')->perlForm()}
sub at          {my ($self, @args) = @_;
				 $self->deckCall('at', @args)}
sub atPut       {my ($self, @args) = @_;
				 $self->deckCall('atPut', @args)}
sub size        {my ($self) = @_;
				 $self->deckCall('size')->perlForm()}



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
sub isTrue {my ($self) = @_; return ${$self} ne ''}
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
sub storeStr {my ($self) = @_; return "\"${$self}\""};
sub isString {return 1}


# Interpolated string.
package LL::InterpString;
use base 'LL::String';
sub isInterpString {return 1}


package LL::ByteArray;
use base 'LL::Stringlike';
sub isByteArray {return 1}

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

# Return the list of patterns to detect an operator that is
# auto-infixed.  Order is in decreasing precedence.
sub autoInfixPatterns {
  my ($selfOrClass) = @_;

  return (qr{(\.|\=\>)}, qr{\-\>});
}

# Test if $self is one of the few operators which is promoted to an
# infix expression from inside a prefix expression.
sub isAutoInfixOperator {
  my ($self) = @_;

  return 0 unless $self->isUnescapedOperator();

  for my $pattern ($self->autoInfixPatterns()) {
	return 1 if ${$self} =~ /^($pattern)$/;	
  }

  return 0;
}



sub isLocalName {
  my ($self) = @_;

  return ${$self} !~ /\:\:/;
}

sub checkLocalName {
  my ($self, @args) = @_;
  $self->SUPER::checkLocalName(@args) unless $self->isLocalName();
}

# Test if $self follows the lexical rules required for variable names.
sub isValidName {
  my ($self) = @_;

  # We test this by running the text of $line through the parser to
  # see if it parses.
  my $line = ${$self};
  my ($newLine, $tok) = LL::Main::parseSymbol($line);

  return 1 if ($tok eq $line && $newLine eq '');
  return 0;
}

sub checkValidName {
  my ($self) = @_;
  die "Symbol '${$self}' is not a valid variable name.\n"
	unless $self->isValidName();
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

sub isLoL {	# Is this a list containing only lists?
  my ($self) = @_;

  for my $elem ( @{$self} ) {
	return 0 unless $elem->isList();
  }
	
  return 1;
}

sub checkLoL {
  my ($self, @args) = @_;

  $self->SUPER::checkLoL(@args)
	unless $self->isLoL();
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

  # True if it takes the form '<x>.<y> = <z>'
  return 1 if ($self->[1]->isUnescapedOperator() && ${$self->[1]} eq '.' &&
			   $self->[3]->isUnescapedOperator() && ${$self->[3]} eq '=');

  # False otherwise
  return 0;
}

sub parsedAsInfix {
  my ($self) = @_;

  return LL::InfixList->new($self)->asPrefixList();
}

=pod xxx

# If $self takes the form [ <term> -> <term> ...], turn the first
# three into a call to the '->' operator.
sub withArrowResolved {
  my ($self) = @_;

  return $self if scalar @{$self} < 3;
  return $self
	unless ($self->[1]->isUnescapedOperator() && ${$self->[1]} eq '->');

  my $arrowExpr = LL::List->new([$self->[1], $self->[0], $self->[2]]);
  return LL::List->new([ $arrowExpr, @{$self}[3..$#{$self}] ]);
}

=cut


# Search for auto-infixed operations (e.g '.' and '=>') and turn them
# into infix subexpressions.  $oper is a regex that matches the
# operator.
sub withAutoInfixed {
  my ($self, $opRegex) = @_;

  my @result = @{$self};

  while (1) {
	my $dotIndex = 0;

	# Find '<expr> . <expr>' or '<expr> => <expr>' sequences
	my $oper;
	for my $item (@result) {
	  last if ($item->isUnescapedOperator() &&
			   ${$item} =~ /^($opRegex)$/);
	  $dotIndex++;
	}
	$oper = $1;
	last if $dotIndex > $#result;

	# Check for errors
	die "Unescaped '$oper' at start or end of expression.\n"
	  if ($dotIndex == 0 || $dotIndex == $#result);

	# Replace the sequence with a single sub-expression (prefix) of
	# the operator being called on left and right operands
	my $expr = LL::List->new([]);
	my @op = splice @result, $dotIndex-1, 3, $expr;
	push @{$expr}, $op[1], $op[0], $op[2];	# Order makes it prefix
  }

  return LL::List->new(\@result);
}


# Automatically convert to infix those operators that support it.
sub withAutoInfixDone {
  my ($self) = @_;

  my $result = $self;
  for my $op (LL::Symbol->autoInfixPatterns()) {
	$result = $result->withAutoInfixed($op);
  }

  return $result;
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
sub withAutoInfixDone {my ($self) = @_; return $self}	# asPrefixList does it.

{
  my %precPerOp;
  my $userPrec;
  BEGIN {
	my @precedence = ([qw{. @ =>}],	# field lookup, seq. access, closure
					  [qw{->}],		# method lookup
					  [qw{**}],		# power
					  [qw{* / // %}],# mult, div, div rounded toward zero, mod
					  [qw{+ -}],	# add, subtract
					  [qw{<< >> >>> <<<}],	# shifts	
					  [qw{== === != !== < > <= >=}],  # Equality and magnitude
					  [qw{&}],		# Bitwise AND.
					  [qw{| ^}],	# Bitwise OR, bitwise XOR
					  [qw{&&}],		# Logical AND, short-circuited
					  [qw{||}],		# Logical OR, short-circuited
					  [qw{}],		# User-defined operators
					  [qw{=}],		# Assignment
					 );
	my $prec = 1;
	for my $lev (reverse @precedence) {
	  map { $precPerOp{$_} = $prec } @{$lev};
	  ++$prec;
	}

	$userPrec = $precPerOp{'='} + 1;
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

sub isQtLoL {	# Is this a quoted list of lists?
  my ($self) = @_;
  return $self->value()->isLoL();
}

sub isQtList {	# Is this a quoted list?
  my ($self) = @_;
  return $self->value()->isList();
}


sub checkQtLoL {
  my ($self, @args) = @_;
  die "Expecting a quoted LoL, got @{[$self->storeStr()]}@_\n"
	unless $self->isQtLoL();
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

package LL::Method;
use base 'LL::Function';
sub storeStr {return "<method>"}

package LL::MethodCall;
use base 'LL::Function';
sub storeStr {return "<method call>"}

package LL::UndefinedFunction;
use base 'LL::Function';
sub storeStr {return "<undefined function>"}
sub new {
  my ($class, $name) = @_;
  die "Expecting a simple Perl string, got @{[ref($name)]}\n"
	if ref($name);

  my $self = sub {die "Called declared proc '$name' before it was defined.\n"};
  return bless $self, $class;
}


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


package LL::Struct;
use base qw{LL::Object LL::Context};

sub isStruct {return 1}
sub checkStruct {}
sub class {my ($self) = @_; return $self->{' class'};}

sub new {
  my ($class, $deckClass) = @_;
  my $self = $class->LL::Context::new($LL::Main::Globals);
  $self->{' class'}	= $deckClass;

  $self->setNamespace($deckClass->{namespace});

  for my $field (keys %{$deckClass->{fieldCache}}) {
	$self->defset($field, main::NIL);
  }

  return bless $self, $class;
}

# Perlform is the unmodified object. This may change
sub perlForm {my ($self) = @_; return $self;}
sub storeStr {return "<struct>"}




package LL::Class;
use base 'LL::Object';
sub isClass {return 1}
sub checkClass {}
sub storeStr {my ($self) = @_;
			  my $nm = $self->{name};
			  $nm = $nm ? " '$nm'" : $nm;
			  return "<class$nm>"}

sub new {
  my ($class, $fields, $methods, $superclass, $structured, $builtin, $name)=@_;

  my $self = {fields		=> $fields,
			  fieldCache	=> {},
			  methods		=> $methods,
			  methodCache	=> {},
			  superclass	=> $superclass,
			  structured	=> $structured,
			  builtin		=> $builtin,
			  name			=> $name,
			  namespace		=> $LL::Main::Globals->getNamespace()};

  bless $self, $class;
  $self->refreshCache();
  $self->refreshFieldCache();

  return $self;
}

sub isStructuredClass {
  my ($self) = @_;
  return !!$self->{structured};
}


sub addMethods {
  my ($self, $methods) = @_;

  $self->{methods} = { %{$self->{methods}}, %{$methods} };
  $self->refreshCache();
}



sub refreshFieldCache {
  my ($self) = @_;

  $self->{fieldCache} = {};
  if (!$self->{superclass}->isNil()) {
	$self->{fieldCache} = { %{ $self->{superclass}->{fieldCache} } };
  }

  for my $name (@{$self->{fields}}) {
	die "Redefinition of field '$name'.\n"
	  if exists ($self->{fieldCache}->{$name});

	$self->{fieldCache}->{$name} = 1;
  }
}

sub refreshCache {
  my ($self) = @_;

  $self->{methodCache} = {};
  if (!$self->{superclass}->isNil()) {
	$self->{superclass}->refreshCache();
	$self->{methodCache} = { %{ $self->{superclass}->{methodCache} } };
  }

  for my $name (keys %{$self->{methods}}) {
	$self->{methodCache}->{$name} = $self->{methods}->{$name};
  }
}


# Lookup and return a reference to the method in this (Deck) class
# associated with '$name'.
sub lookup {
  my ($self, $name) = @_;

  # If the method is defined, just return it.
  return $self->{methodCache}->{$name}
	if defined($self->{methodCache}->{$name});

  # If this class implements 'doesNotUnderstand', call it with the
  # name and arguments.
  if (defined($self->{methodCache}->{doesNotUnderstand})) {
	my $dnu = $self->{methodCache}->{doesNotUnderstand};
	my $method = sub {
	  my ($dkSelf, @args) = @_;

	  return $dnu->($dkSelf, LL::Symbol->new($name), LL::List->new(\@args));
	};
	return LL::Method->new($method);	
  }

  # Otherwise, it's an error.
  die "Unknown method: '$name'\n";
}




# ---------------------------------------------------------------------------

package LL::Main;

use Term::ReadLine;
use Scalar::Util qw(looks_like_number blessed);
use UNIVERSAL 'isa';		# Deprecated but I need it to identify LL::Objects
use Cwd qw{abs_path getcwd};
use File::Basename;

sub NIL {return LL::Nil::NIL;}


use constant TRUE => LL::Number->new(1);

our $Input = undef;		# Input filehandle or undef for stdin.
my $NeedPrompt = 0;		# If true, reader is inside a LoL Line
our $Globals = LL::GlobalContext->new();
my %fnNeedsContext;		# Hash of functions that get the parent's context


# ---------------------------------------------------------------------------

# Flags:
my $dumpExpr = 0;
my $noLib = 0;

use Getopt::Long;

{
  my $args = pullOutArgs();

  GetOptions ('dump-expr'			=> \$dumpExpr,
			  'no-lib'				=> \$noLib)
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

	my $fn = compile($Globals, $args, $expr, 'toplevel', "*top*");

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

  # Fix up the OO syntactic sugar
  $rlist = $rlist->withAutoInfixDone();

  # Strip out any operator escapes.  We no longer need them.
#  $rlist->unescapeAllOperators();

  # Warn of the case where an explicit list is the only element of a
  # line because the programmer may have accidentally bracketted the
  # line.
# autobracketting causes this
#  if (scalar @{$rlist} == 1 && $rlist->[0]->isList()) {
#	dkwarn ("Entire LoL line is bracketed.  This may not be what",
#			"you want.");
#  }

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
	  return readSexp(${$tok})->withAutoInfixDone()->asPrefixList();
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

		# See if this is a number
		my ($newLine, $numObj) = readNumber($line);
		if ($numObj) {
		  push @tokens, $numObj;
		  $line = $newLine;
		  next;
		}

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


sub readNumber {
  my ($line) = @_;

  my $result;

  {
	my $tok;

	# Floating-point literal
	$line =~ s/^(\-?[0-9][0-9_]*)\.([0-9][0-9_]*)(\W?)/$3/ and do {
	  $tok = "$1.$2";
	  $tok =~ s/_//g;
	  $result = LL::Number->new($tok);
	  next;
	};

	# Hex literal
	$line =~ s/^(\-?)0x([0-9a-fA-F_]*)(\W?)/$3/ and do {
	  my ($sign, $num) = ($1, $2);
	  $num =~ s/_//g;
	  $tok = oct("0x$num");
	  $tok = -$tok if $sign eq '-';
	  $result = LL::Number->new($tok);
	  next;
	};

	# Binary literal
	$line =~ s/^(\-?)0b([01_]*)(\W?)/$3/ and do {
	  my ($sign, $num) = ($1, $2);
	  $num =~ s/_//g;
	  $tok = oct("0b$num");
	  $tok = -$tok if $sign eq '-';
	  $result = LL::Number->new($tok);
	  next;
	};

	# Decimal literal.  Perl's conversion to number ignores
	# leading zeroes
	$line =~ s/^(\-?\d[0-9_]*)(\W?)/$2/ and do {
	  my $num = $1;
	  $num =~ s/_//g;
	  $tok = $num + 0;
	  $result = LL::Number->new($tok);
	  next;
	};
  }

  return ($line, $result);

}



# Attempt to parse the start of $line as a symbol.  On success, return
# the modified $line and the symbol text; on failure, return false.
sub parseSymbol {
  my ($line) = @_;

  # Regexp to match operators: may begin with '\' to indicate escaped
  # version.
  my $OPER_REGEX = qr{\\? [-.\|!@\$\%^&*+=?<>\/]+}x;

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
# false.  Note that NIL and other Deck false values are Perl true
# values.
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
  return $arg if (blessed($arg) && $arg->can('checkType'));
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
	last unless ($name->isSymbol() && $context->present(${$name})) ;

	my $val = $context->lookup(${$name});
	last unless $val->isMacro();

	print "*macro* ${$name}(@{[$expr->printStr()]}) => " if $dumpExpr;
	$expr = $val->(@{$expr});
	print $expr->printStr() . "\n" if $dumpExpr;

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

  # Functions listed in %fnNeedsContext are special cases and get
  # access to the context.
  if (defined($fnNeedsContext{$fn})) {
	unshift @args, $context;
  }

  if (!$fn->isCallable()) {
	my $fname = $expr->[0]->isSymbol() ? ${$expr->[0]} : '';
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


# Search $expr for uses of words undefined in $context.  If $expr is a
# var or const declaration, first adds the names to $context.
sub ensureVarsDeclaredRecursively {
  my ($expr, $context) = @_;

  # If this is a var or const declaration, add the elements to $context
  if ($expr->[0]->isSymbol() && ${ $expr->[0] } =~ /^_::(var|const)$/) {
	my $kw = $1;
	my @elems = @{$expr};
	shift @elems;	# lose leading _::var or _::const

	# Strip quotes to mimic what evaluation does.
	@elems = map { $_->isQuote() ? $_->value() : $_ } @elems;

	if ($kw eq 'var') {
	  builtin_var($context, @elems);
	} else {
	  builtin_const($context, @elems);
	}
  }

  for my $element (@{$expr}) {
	if ($element->isSymbol()) {
	  die "Use of undeclared variable '${$element}'\n"
		unless $context->present(${$element});
	} elsif ($element->isList()) {
	  ensureVarsDeclaredRecursively($element, $context);
	}
  }
}

# Search $body for uses of undeclared variables.
sub ensureVarsDeclared {
  my ($outerContext, $args, $body, $name, $mode, $isVararg) = @_;

  # We skip toplevel expressions because a) it complicates this hack and
  # b) compile() will detect this stuff soon enough anyway.
  return if $mode eq 'toplevel';

  # Check for uses of udeclared variables.
  my $scratchContext = LL::Context->new($outerContext);

  $scratchContext->def('return') if $mode =~ /^(method|proc|macro)$/;
  $scratchContext->def('subreturn') if $mode eq 'sub';
  $scratchContext->def('self') if $mode eq 'method';
  $scratchContext->def('args') if $isVararg;

  for my $arg (@{$args}) {
	$arg->checkSymbol(" in formal argument of '$name'.");
	$scratchContext->def(${$arg});
  }

  for my $entry (@{$body}) {
	ensureVarsDeclaredRecursively($entry, $scratchContext);
  }

}


# Return a blessed func. ref which executes a sub with $args and $body
# in the given context.  If $context is undef, the $Global context is
# used, allowing the function to define and set global variables.
# $name is used for error messages and may be omitted.
sub compile {
  my ($outerContext, $args, $body, $mode, $name) = @_;
  $body->checkLoL();

  $name ||= '<unnamed function>';

  die "Unknown compiler mode '$mode'\n"
	unless $mode =~ /^(macro|proc|sub|method|toplevel)$/;

  my ($isMacro, $isProc, $isSub, $isMethod, $isTop)
	= ($mode eq 'macro', $mode eq 'proc', $mode eq 'sub', $mode eq 'method',
	   $mode eq 'toplevel');
  my $firstArgIsConst = 0;

  if ($isMethod) {
	unshift @{$args}, LL::Symbol->new('self');
	$firstArgIsConst = 1;
  }

  my $nargs = scalar @{$args};
  my $isVararg = $nargs > 0 && ${$args->[-1]} eq 'args';
  if ($isVararg) {
	pop @{$args};
	--$nargs;
  }

  # Determine the default namespace if needed.
  my $namespace = $isProc ? $Globals->getNamespace : undef;

  # Expand all macros (and also check for scope violations)
  my @fixedBody;
  {
	for my $expr (@{$body}) {
	  my $newExpr = applyMacrosRecursively ($expr, $Globals);

	  $newExpr->unescapeAllOperators();

	  checkForScopeViolations($newExpr, $name);

	  push @fixedBody, $newExpr;
	}
  }

  # Find undeclared variables.
  ensureVarsDeclared($outerContext, $args, [@fixedBody], $name, $mode,
					 $isVararg);

  print "$name: ", LL::List->new(\@fixedBody)->storeStr(), "\n"
	if $dumpExpr;

  my $fn = sub {
	my $context;

	if ($isMethod) {
	  my $mthSelf = $_[0];
	  $context = $mthSelf->class()->isStructuredClass() ?
		LL::Context->new($mthSelf):
		LL::Context->new($outerContext);
	} elsif ($isTop) {
	  $context = $Globals;
	} else {
	  $context = LL::Context->new($outerContext);
	  die "WTF? null outerContext!\n" if !defined($outerContext);
	}

	# Set the namespace for the current context if required.
	$context->setNamespace($namespace) if $namespace;

	#  Check for argument mismatch
	if (scalar @_ != $nargs && !($isVararg && scalar @_ > $nargs)) {
	  my ($expecting, $got) = ($nargs, scalar @_);
	  do {--$expecting, --$got} if $isMethod;	# Skip 'self'
	  my $atleast = $isVararg ? "at least " : "";
	  die "Argument count mismatch in call to $name.  Expecting $atleast"
		. "$expecting, got $got.\n";
	}

	# Bind arguments
	for my $arg (@{$args}) {
	  if ($firstArgIsConst) {
		$firstArgIsConst = 0;
		$context->defsetconst(${$arg}, shift @_);
		next;
	  }

	  $context->defset (${$arg}, shift @_ );
	}

	# Bind varargs
	if ($isVararg) {
	  my $args = LL::List->new([@_]);

	  $context->defset('args', $args);
	}
	
	my $lastexpr;

	my $retname =
	  ($isProc || $isMethod || $isMacro) ? 'return' :
	  ($isSub)                           ? 'subreturn' : '';
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
	return NIL if ($isProc && !$@);

	# Methods return 'self' by default.
	return $context->{' parent'} if ($isMethod && !$@);

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
# give in @counts.  If the first element of @counts (i.e. the second
# argument) is the string '-', ignore the first argument in
# $args--it's 'self'.
sub checkNargs {
  my ($args, @counts) = @_;

  my $offset = 0;
  if ($counts[0] eq '-') {
	$offset = 1;
	shift @counts;
  }

  for my $count (@counts) {
	return if scalar @{$args} == $count + $offset;
  }

  die "Expecting @{[join (' or ', @counts)]} arguments; "
	. "got @{[scalar @{$args} - $offset]}.\n";
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

  if ($expr->isQtLoL()) {
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


# Attempt to subify the arguments, dying if this is impossible.
sub subifyStrict {
  my ($expr, @args) = @_;

  my $result = subify($expr, @args);
  return $result if $result != $expr;

  my $bs = $expr->storeStr();
  die "Expecting a single expression or quoted list of list. Got '$bs'\n";
}


# Subify the argument.  If it is not a list, just return a sub that
# evaluates the naked expression.
sub subifyOrDelay {
  my ($expr) = @_;

  die "WTF: args passed to subifyOrDelay.\n"
	if scalar @_ > 1;

  return delayed($expr)
	unless ($expr->isList() || $expr->isQtList());
  return subify($expr);
}



# Wrap $expr with a sub which, when called, evaluates the expression
# and returns it.
sub delayed {
  my ($expr) = @_;

  $expr = LL::List->new([LL::Symbol->new('_::val'), $expr]);

  return LL::List->new([LL::Symbol->new('_::sub'),
						LL::Quote->new(LL::List->new([])),
						LL::Quote->new(LL::List->new([$expr])),
					   ]);
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
  my ($proc, $name, $args, $body) = @_;
  checkNargs(\@_, 4);

  $name->checkSymbol(" in '${$proc}' arg 1");
  $args = fixFormalArgs($args);
  $body->checkQtLoL(" in function body of '${$proc}'.");

  my $procOrMethod = ${$proc} eq 'method' ? '_::method' : '_::proc';

  return LL::List->new([LL::Symbol->new($procOrMethod),
						LL::Quote->new($name),
						$args,
						$body]);
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
  $body->checkQtLoL();

  return LL::List->new([LL::Symbol->new('_::macro'),
						LL::Quote->new($name),
						$args,
						$body]);
}


sub launder_varconst {
  my ($isConst, @macroArgs) = @_;

  my @args;
  if (scalar @macroArgs == 1 && $macroArgs[0]->isQtLoL()) {
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
  my ($set, $dest, $value) = @_;

  my $err = "Malformed assignment: " . LL::List->new(\@_)->printStr() . "\n";

  my @result;
  if ($dest->isSymbol()) {
	# Case 1: simple assignment to variable
	@result = (LL::Symbol->new('_::set'),
			   LL::Quote->new($dest),
			   $value);

  } elsif ($dest->isList() && scalar @{$dest} == 3 &&
		   $dest->[0]->isUnescapedOperator()) {
	
	if (${$dest->[0]} eq '@') {
	  # Case 2: List element assignment (eg: 'l@5 = 42')
	  @result = (LL::Symbol->new('_::atput'),
				 $dest->[1],	# list
				 $dest->[2],	# index
				 $value);
	} elsif (${$dest->[0]} eq '.') {
	  # Case 3: Struct field assignment (eg: 'foo.a = 42')
	  my $object = $dest->[1];
	  my $field = $dest->[2];
	  $field->checkSymbol(" in object field name.");

	  my $setter = LL::Symbol->new("${$field}_set");
	  my $lookup = macro_methodLookupOp('', $object, $setter);

	  @result = ($lookup, $value);

	} else {
	  die $err;
	}

  } else {
	die $err;
  }

  return LL::List->new(\@result);
}


sub macro_subfn {
  my ($sub, $args, $body) = @_;
  checkNargs (\@_, 3, 2);

  # If args are omitted, add them.
  if (scalar @_ == 2) {
	$body = $args;
	$args = LL::Quote->new(LL::List->new([]));
  }

  $args = fixFormalArgs($args);

  return LL::List->new([LL::Symbol->new('_::sub'),
						$args,
						$body]);
}


sub macro_iffn {
  my ($if, $cond, $trueBlock, $else, $falseBlock) = @_;
  checkNargs(\@_, 5, 4, 3);

  # If there's a 'falseBlock' but $else was omitted, we need to fix
  # that.
  if (defined($else) && $else->isQtLoL()) {
	die "Malformed 'if' statement.\n" if $falseBlock;
	$falseBlock = $else;
  }

  my $falsePart = $falseBlock ? subifyStrict($falseBlock) : NIL;
  return LL::List->new([LL::Symbol->new('_::if'),
						subifyOrDelay($cond),
						subifyStrict($trueBlock),
						$falsePart]);
}


sub macro_whilefn {
  my ($while, $cond, $body) = @_;

  die "Expecting 2 arguments to 'while'; got @{[scalar @_ - 1]}\n"
	unless scalar @_ == 3;

  return LL::List->new([LL::Symbol->new('_::while'),
						subifyOrDelay($cond),
						subifyStrict($body)]);
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


sub macro_class {
  my ($class, $name, $superclass, $body) = @_;
  checkNargs(\@_, 3, 4);

  # Handle omitted superclass
  if ($superclass->isQtLoL()) {
	$body = $superclass;
	$superclass = LL::Symbol->new('Struct');
  }

  $name->checkSymbol(" in class definition.");
  $superclass->checkSymbol(" in class definition");
  $body->checkQtLoL(" in class definition.");

  my $classDef = LL::List->new([LL::Symbol->new('_::class'),
								$superclass,
								$body,
								LL::String->new(${$name})]);

  return LL::List->new([LL::Symbol->new('_::var'),
						LL::Quote->new($name),
						$classDef]);
}


sub macro_class_ext {
  my ($classExt, $name, $body) = @_;
  checkNargs(\@_, 3, 4);

  $name->checkSymbol(" in class definition.");
  $body->checkQtLoL(" in class definition.");

  die "Undefined (class) name '${$name}'\n"
	if (!$Globals->present(${$name}));

  return LL::List->new([LL::Symbol->new('_::class_ext'),
								$name,
								$body]);
}





# The -> operator
sub macro_methodLookupOp {
  my ($arrow, $object, $method) = @_;

  $method->checkSymbol(" on the RHS of a '->' operation.");

  my $lookupFn = '_::getMethod';
  if ($object->isSymbol() && ${$object} eq 'super') {
	$object = LL::Symbol->new('self');
	$lookupFn = '_::getSuperMethod';
  }

  # Enforce method privacy rules (as such).
  if (${$method} =~ /^_/ &&
	  !($object->isSymbol() && ${$object} =~ /^(self|super)$/)) {
	die "Attempted to send private message ${$method} to something not "
	  . "self or super.\n";
  }

  return LL::List->new([LL::Symbol->new($lookupFn),
						$object,
						LL::Quote->new($method)]);
}


# The '.' operator (reading only)
sub macro_fieldget {
  my ($dot, $object, $field) = @_;

  $field->checkSymbol(" on the RHS of a '.' operation.");

  my $method = LL::Symbol->new("${$field}_get");
  my $lookupExpr = macro_methodLookupOp('-> ignored', $object, $method);

  return LL::List->new([$lookupExpr]);
}


# Logical AND (&&).  This expands into an _::if statement, which works
# because _:if returns the result of the last expression evaluated.
sub macro_logand {
  my ($or, $left, $right) = @_;

  return LL::List->new([LL::Symbol->new('_::if'),
						delayed($left),
						delayed($right),
						NIL]);
}

# Logical OR (||).  This expands into an _::if statement with the
# 'true' block nil.  This results in the getting the LHS if TRUE or
# the RHS.
sub macro_logor {
  my ($or, $left, $right) = @_;

  return LL::List->new([LL::Symbol->new('_::if'),
						delayed($left),
						NIL,
						delayed($right)]);
}


sub macro_suboper {
  my ($arrow, $left, $right) = @_;

  die "LHS of '=>' operator was created from an auto-infix expression.\n"
	if ($left->isList() && $left->size() > 0 &&
		$left->[0]->isAutoInfixOperator());

  $left = fixFormalArgs($left);
  $right->checkQtLoL(" in RHS of ${$arrow} operator.");

  return LL::List->new([LL::Symbol->new('_::sub'),
						$left,
						$right]);
}


# Implement the '-' operator.  This is tricky because '-' can be both
# unary and binary.
sub macro_minus {
  my ($minus, $left, $maybeRight) = @_;
  die "Expecting 1 or 2 arguments for macro '-', got @{[scalar @_]}\n"
	unless (scalar @_ == 2 || scalar @_ == 3);

  # If there's only one argument, this is a negation.
  return LL::List->new([LL::Symbol->new('Lang::neg'), $left])
	if (scalar @_ == 2);

  # If there are two arguments, this is a call of op_Minus
  my $lookup = LL::List->new([LL::Symbol->new('->'),
							  $left,
							  LL::Symbol->new('op_Sub')]);

  return LL::List->new([$lookup, $maybeRight]);
}


# ---------------------------------------------------------------------------

# Define a builtin class.
sub defclass ($$$) {
  my ($name, $superclass, $methods) = @_;

  my $sc = ($superclass eq '') ? NIL : $Globals->lookup($superclass);

  my $class = LL::Class->new([], $methods, $sc, $name eq 'Struct', 1, $name);
  $Globals->defset($name, $class);
  LL::Class->registerBuiltin($name, $class);

  return $class;
}


# Given an operator and a method name, create a macro with the
# operator's name that calls the method on the LHS with the RHS as
# argument.
sub op_method ($$) {
  my ($operator, $method) = @_;

  macro $operator, sub {
	my ($name, $left, $right) = @_;
	checkNargs(\@_, 3);

	my $lookup = LL::List->new([LL::Symbol->new('->'),
								$left,
								LL::Symbol->new($method)]);

	return LL::List->new([$lookup, $right]);
  };
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

  # Temp. definition of Struct.  Fix later.  XXX
  $Globals->defset('Struct', NIL);

  # Externally-defined primitive functions
  for my $special (
				   ['println',			\&builtin_println],
				   ['puts',				\&builtin_println],
				   ['say',				\&builtin_say],
				   ['storestr',			\&builtin_storestr],
				   ['show',				\&builtin_show],
				   ['_::proc',			\&builtin_proc],
				   ['_::sub',			\&builtin_subfn],
				   ['_::if',			\&builtin_iffn],
				   ['_::while',			\&builtin_whilefn],
				   ['_::set',			\&builtin_set],
				   ['_::var',			\&builtin_var],
				   ['_::const',			\&builtin_const],
				   ['_::atput',			\&builtin_atput],
				   ['atput',			\&builtin_atput],
				   ['_::map',			\&builtin_mapfn],
				   ['_::foreach',		\&builtin_foreachfn],
				   ['_::macro',			\&builtin_macro],
				   ['_::mproc',			\&builtin_mproc],
				   ['_::mkstr',			\&builtin_mkstr],
				   ['_::mkstr_all',		\&builtin_mkstr_all],
				   ['mkstr',			\&builtin_mkstr],
				   ['_::use',			\&builtin_usefn],
				   ['_::perlproc',		\&builtin_perlproc],
				   ['_::perluse',		\&builtin_perluse],
				   ['apply',			\&builtin_apply],
				   ['intern',			\&builtin_intern],
				   ['_::class',			\&builtin_class],
				   ['_::class_ext',		\&builtin_class_ext],
				   ['_::getMethod',		\&builtin_getMethod],
				   ['getMethod',		\&builtin_getMethod],
				   ['_::getSuperMethod',\&builtin_getSuperMethod],
				   ['getSuperMethod',	\&builtin_getSuperMethod],
				   ['new',				\&builtin_new],
				   ['defined',			\&builtin_definedfn],
				   ['lookup',			\&builtin_lookup],
				  ) {
	$Globals->defset($special->[0], LL::Function->new($special->[1]));
  }

  # Other simple primitives
  prim 'Symbol', 'typeof', "Object", sub { local $_=ref($_[0]); s/^LL:://; $_};

  # More complex primitive functions
  prim2 '===',			sub { checkNargs(\@_, 2); return boolObj($_[0] == $_[1])};
  prim2 '==',			sub { checkNargs(\@_, 2); return $_[0]->equals($_[1]) };
  prim2 'list',			sub { return LL::List->new(\@_) };
  prim2 '@',			sub { my ($l, $ndx) = @_;  checkNargs(\@_, 2);
							  return $l->at($ndx) };
  prim2 'size',			sub { my ($l) = @_;  checkNargs(\@_, 1);
							  return LL::Number->new($l->size())};
  prim2 'byteArray',	sub { return LL::ByteArray->new(@_) };
  prim2 'bytesSized',	sub { my ($size) = @_;  checkNargs(\@_, 1);
							  $size->checkNumber();
							  die "Invalid byteArray size: ${$size}\n"
								unless ${$size} > 0;
							  return LL::ByteArray->newSized(${$size})
							};
  prim2 'die',			sub { die join("", map { $_->printStr() } @_) . "\n" };
  prim2 'listSized',	sub { my ($size) = @_;  checkNargs(\@_, 1);
							  $size->checkNumber();
							  die "Invalid list size: ${$size}\n"
								unless ${$size} > 0;
							  return LL::List->new([(NIL) x ${$size}]);
							};
  prim2 '_::defns',		sub { my ($ns) = @_;  checkNargs(\@_, 1);
							  $ns->checkSymbol();
							  $Globals->defNamespace(${$ns});
							};
  prim2 'not',			sub { my ($arg) = @_; checkNargs(\@_, 1);
							  return boolObj(!$arg->isTrue());
							};
  prim2 'int',			sub { my ($arg) = @_; checkNargs(\@_, 1);
							  $arg->checkNumber(" in 'int'");
							  return LL::Number->new(int(${$arg}));
							};
  prim2 'neg',			sub { my ($l) = @_; checkNargs(\@_, 1);
							  $l->checkNumber(" in 'Lang::neg'");
							  return LL::Number->new(-${$l});
							};
  prim2 'str2num',		sub { my ($str) = @_; checkNargs(\@_, 1);
							  $str->checkString(" in 'num'");
							  my ($ns, $num) = readNumber(${$str});
							  return NIL unless $ns eq "";
							  return decktype($num);
							};
  prim2 'exit',			sub { my ($status) = @_; checkNargs(\@_, 1);
							  $status->checkNumber(" in 'exit'");
							  exit(${$status});
							  return NIL;	# not reached
							};


  prim2 '_::val',		sub { return NIL unless scalar @_; return $_[-1] };
  $Globals->defset('val', $Globals->{'_::val'});


  # Create the hash of functions that take the context as arg. 0.
  for my $name (qw{_::set _::var _::sub _::const lookup defined}) {
	my $fn = $Globals->lookup($name);
	$fnNeedsContext{$fn} = 1;
  }

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
  macro 'for',			\&macro_foreachfn;
  macro 'map',			\&macro_mapfn;
  macro 'macro',		\&macro_macro;
  macro 'mproc',		\&macro_mproc;
  macro 'package',		\&macro_packagefn;
  macro 'use',			\&macro_usefn;
  macro 'perlproc',		\&macro_perlproc;
  macro 'perluse',		\&macro_perluse;
  macro 'class',		\&macro_class;
  macro '_class_ext',	\&macro_class_ext;
  macro '->',			\&macro_methodLookupOp;
  macro '.',			\&macro_fieldget;
  macro '&&',			\&macro_logand;
  macro '||',			\&macro_logor;
  macro '=>',			\&macro_suboper;
  macro '-',			\&macro_minus;

  # Operator-to-method mappings
  op_method '+',  'op_Add';
  op_method '%',  'op_Mod';
  op_method '*',  'op_Mult';
  op_method '/',  'op_Div';
  op_method '//', 'op_DivTrunc';
  op_method '<',  'op_Lt';
  op_method '<=', 'op_Lte';
  op_method '>',  'op_Gt';
  op_method '>=', 'op_Gte';
  op_method '**', 'op_Pow';
  op_method '|',  'op_BitOr';
  op_method '&',  'op_BitAnd';
  op_method '^',  'op_BitXor';


  # Define the built-in classes.
  defclass 'Object', '',
	{class_get  => sub {my ($self) = @_; return $self->class()},
	 isTrue_get => sub {
	   my ($self) = @_; checkNargs(\@_, 1);
	   return $self->isTrue() ? decktype(1) : NIL;
	 },

	};

  defclass 'Class',		'Object',
	{new		=> sub {
	   my ($self, @argv) = @_;
	   die "'new' only works on Struct-derived classes.\n"
		 unless $self->isStructuredClass();
	   return builtin_new($self, @argv);
	 },
	 name_get	=> sub {my ($self) = @_; checkNargs(\@_, 1);
						decktype($self->{name})},

	 name_set	=> sub {my ($self, $value) = @_; checkNargs(\@_, 2);
						$value->checkString (" in 'name' class attribute.");
						$self->{name} = $value;
						return $value},

	 selectors_get => sub {
	   my ($self) = @_; checkNargs(\@_, 1);
	   my @names =
		 map { LL::Symbol->new($_) }
		   sort keys %{ $self->{methodCache} };
	   return LL::List->new(\@names);
	 },

	};

  defclass 'List',			'Object',
	{at			=> sub {my ($self, $index) = @_; checkNargs(\@_, 2);
						return $self->at($index)},
	 atPut		=> sub {my ($self, $index, $value) = @_; checkNargs(\@_, 3);
						return $self->atPut($index, $value)},
	 size_get	=> sub {my ($self) = @_; checkNargs(\@_, 1);
						return decktype($self->size());},
	};


  defclass 'Stringlike',	'Object',
	{at			=> sub {my ($self, $index) = @_; checkNargs(\@_, 2);
						return $self->at($index)},
	 atPut		=> sub {my ($self, $index, $value) = @_; checkNargs(\@_, 3);
						return $self->atPut($index, $value)},
	 size_get	=> sub {my ($self) = @_; checkNargs(\@_, 1);
						return decktype($self->size());},
	};

  defclass 'String',		'Stringlike', {};
  defclass 'Symbol',		'Stringlike', {};
  defclass 'ByteArray',		'Stringlike', {};

  defclass 'Number',		'Object',
	{
	 # Double-dispatched methods.  Remember, the arguments are
	 # reversed, so $self is the RHS and $other is the LHS.
	 addNumber	=> sub {my ($self, $other) = @_;
						$other->checkNumber(" in addNumber");
						return LL::Number->new(${$self} + ${$other})},
					
	 subNumber	=> sub {my ($self, $other) = @_;
						$other->checkNumber(" in addNumber");
						return LL::Number->new(${$other} - ${$self})},
					
	 modNumber	=> sub {my ($self, $other) = @_;
						$other->checkNumber(" in modNumber");
						die "Modulo by zero error\n" if ${$self} == 0;
						return decktype(int(${$other}) % int(${$self}))},

	 multNumber => sub {my ($self, $other) = @_;
						$other->checkNumber(" in multNumber");
						return decktype(${$other} * ${$self})},

	 divNumber  => sub {my ($self, $other) = @_;
						$other->checkNumber(" in divNumber");
						die "Division by zero error\n" if ${$self} == 0;
						return decktype(${$other} / ${$self})},

	 divTruncNumber => sub {my ($self, $other) = @_;
						$other->checkNumber(" in divTruncNumber");
						die "Division by zero error\n" if ${$self} == 0;
						return decktype(int(${$other} / ${$self}))},

	 ltNumber	=> sub {my ($self, $other) = @_;
						$other->checkNumber(" in ltNumber");
						return boolObj(${$other} < ${$self})},

	 lteNumber	=> sub {my ($self, $other) = @_;
						$other->checkNumber(" in lteNumber");
						return boolObj(${$other} <= ${$self})},

	 gtNumber	=> sub {my ($self, $other) = @_;
						$other->checkNumber(" in gtNumber");
						return boolObj(${$other} > ${$self})},

	 gteNumber	=> sub {my ($self, $other) = @_;
						$other->checkNumber(" in gteNumber");
						return boolObj(${$other} >= ${$self})},

	 powNumber  => sub {my ($self, $other) = @_;
						$other->checkNumber(" in powNumber");
						return decktype(${$other} ** ${$self})},

	 bitOrNumber=> sub {my ($self, $other) = @_;
						$other->checkNumber(" in bitOrNumber");
						return decktype(int(${$other}) | int(${$self}))},

	 bitXorNumber=>sub {my ($self, $other) = @_;
						$other->checkNumber(" in bitXorNumber");
						return decktype(int(${$other}) ^ int(${$self}))},

	 bitAndNumber=>sub {my ($self, $other) = @_;
						$other->checkNumber(" in bitAndNumber");
						return decktype(int(${$other}) & int(${$self}))},

	};


  defclass 'Nil',			'Object', {};
  defclass 'Quote',			'Object', {};
  defclass 'Macro',			'Object', {};
  defclass 'Function',		'Object', {};
  defclass 'Method',		'Object', {};
  defclass 'MethodCall',	'Object', {};
  defclass 'PerlObj',		'Object', {};

  defclass 'Struct',		'Object', {};


  # The external 'Lang' module
  if (!$noLib) {
	# Hack: disable dumping when loading the library.
	my $dbk = $dumpExpr;
	$dumpExpr = 0;

	my $path = findFileFor('Lang');
	die "Unable to find system module 'Lang.dk'.  Check your module path.\n"
	  unless $path;
	readfile($path, 'Lang', 0, 0);

	$dumpExpr = $dbk;
  }

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

sub builtin_say {
  for my $obj (@_) {
	die "Not an object: '$obj'\n"
	  unless (ref($obj) && isa($obj, 'LL::Object'));
	print $obj->printStr();
  }

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

  $Globals->def(${$name});
  my $func = compile ($Globals, $args, $body, 'proc', ${$name});
  $Globals->set(${$name}, $func);

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
# evaluated.  The second and third closure (the 'true' and 'else' parts)
# may be NIL in which case they are skipped.
sub builtin_iffn {
  my ($test, $trueBlock, $falseBlock) = @_;
  checkNargs(\@_, 3);

  my $result = $test->();
  if ($result->isTrue()) {
	$result = $trueBlock->() if $trueBlock->isFunction();
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


# Given a package name, find the corresponding source file.
sub findFileFor {
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

  for my $expr (@{$list}) {
	$expr->checkList(" in '$with' clause element.");
	($expr->size() == 1 || $expr->size() == 3)
	  or die "Empty list as '$with' clause element.\n";

	my $isRename = $expr->size() == 3;
	die "Malformed '$with' clause element '" . $expr->printStr() . "'\n"
	  if ($isRename && (!$expr->[0]->isSymbol() || ${$expr->[0]} ne '='));

	my $newName = $isRename ? $expr->[1] : $expr->[0];
	my $oldName = $isRename ? $expr->[2] : $newName;

	$newName->checkSymbol (" in name part of '$with' clause element.");
	$newName->checkLocalName("as imported symbol.");

	$oldName->checkSymbol (" in old name field of '$with' clause element.");
	$oldName->checkLocalName(" as old name of an imported symbol.");

	$result{${$oldName}} = ${$newName};
  }

  return \%result;
}


sub builtin_usefn {
  my ($moduleName, $with, $list) = @_;

  checkNargs(\@_, 3);

  $moduleName->checkSymbol();

  # Import the file
  my $mn = ${$moduleName};
  my $path = findFileFor($mn);
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

	$names = _getImportNameList($list, ${$with});
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

  return NIL;
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


# Create a MethodCall which calls the method named by $methodName on
# object $object.  If $super is true, the method search starts in the
# superclass instead of the class.
sub doMethodLookup {
  my ($object, $methodName, $super) = @_;

#  $object->checkStruct(" in method call.");
  $methodName->checkSymbol();

  my $class = $object->class();
  if ($super) {
	$class = $class->{superclass};
	
	# This should not happen.  The only case where it can is if $class
	# is Struct, which is provided and has no such foolishness.
	die "'super' call in class without superclass.\n"
	  unless $class;
  }

  my $method = $class->lookup (${$methodName});

  # Turn into a self call
  my $sub = sub {$method->($object, @_)};

  return LL::MethodCall->new($sub);
}

# Return the MethodCall that invokes the named method on $object.
# Implements the guts of the '->' operator.
sub builtin_getMethod {
  my ($object, $methodName) = @_;
  return doMethodLookup($object, $methodName, 0);
}

# Like builtin_getMethod but the method search starts in the
# superclass.
sub builtin_getSuperMethod {
  my ($object, $methodName) = @_;
  return doMethodLookup($object, $methodName, 1);
}



# Given an entry in a class definition, return which sort of
# declaration it is, die-ing if it is invalid.
sub fieldType {
  my ($entry) = @_;

  my $start = $entry->[0];
  $start->checkSymbol();

  # Sanity check
  die "Unknown class declaration part: '${$start}...'\n"
	unless ${$start} =~ /^(var|public|readable|writeable|method)$/;

  return ${$start};
}


sub class_fields {
  my ($body) = @_;

  my $fields = {};
  my @attribs;
  for my $entry (@{$body}) {
	next if scalar @{$entry} == 0;		# Maybe too tolerant

	my $start = fieldType($entry);
	next if ($start eq 'method');
	shift @{$entry};

	my $readable = ($start =~ /^(public|readable)$/);
	my $writeable = ($start =~ /^(public|writeable)$/);

	while (@{$entry}) {
	  my $name = shift @{$entry};
	  $name->checkSymbol(" as field name");
	  my $nmStr = ${$name};

	  die "Attempted to use unescaped operator '${$name}' as field name.\n"
		if ($name->isUnescapedOperator());

	  die "Redefinition of field '$nmStr'\n"
		if exists($fields->{$nmStr});

	  $fields->{$nmStr} = NIL;

	  push @attribs, "${nmStr}_get" if $readable;
	  push @attribs, "${nmStr}_set" if $writeable;
	}
  }

  return ($fields, \@attribs);
}





sub mk_method {
  my ($name, $args, $fields, $body) = @_;

  # Create a scratch LL::Context to keep the compiler happy.  (Should
  # this be an LL::Struct?)
  my $fieldsContext = LL::Context->new($Globals);
  for my $key (keys %{$fields}) {$fieldsContext->def($key);}

  my $code = compile($fieldsContext, $args, $body, 'method', ${$name});
  return LL::Method->new($code);
}


sub class_methods {
  my ($body, $fields) = @_;

  my %methods = ();
  for my $entry (@{$body}) {
	next if scalar @{$entry} == 0;		# Maybe too tolerant

	my $start = $entry->[0];
	$start->checkSymbol();
	next if ${$start} ne 'method';

	die "Malformed method declaration: '@{[$entry->printStr()]}'.\n"
	  unless scalar @{$entry} == 4;

	my ($method, $name, $args, $body) = @{$entry};
	$name->checkSymbol(" in method name.");
	$args = fixFormalArgs($args)->value();

	$body->checkQtLoL(" in method definition.");
	$body = $body->value();	# There's no more eval so drop the quote

	$methods{${$name}} = mk_method($name, $args, $fields, $body);
  }

  return \%methods;
}




sub attrib_parts {
  my ($attrib) = @_;

  my ($args, $body);

  die "Malformed attribute: '$attrib'\n"
	unless ($attrib =~ /^(.+)_(set|get)$/);
  my ($field, $type) = ($1, $2);

  $body = LL::List->new([]);

  if ($type eq 'set') {
	$args = LL::List->new([LL::Symbol->new("new_$field")]);

	push @{$body},LL::List->new([LL::Symbol->new('_::set'),
								 LL::Quote->new(LL::Symbol->new($field)),
								 LL::Symbol->new("new_$field")]);

  } else {	# $type eq 'get'
	$args = LL::List->new([]);
  }

  # We always need to return the value
  push @{$body}, LL::List->new([LL::Symbol->new('return'),
								LL::Symbol->new($field)]);

  return ($args, $body);
}



# Add the attributes to $methods
sub class_attributes {
  my ($attribNames, $body, $fields, $methods) = @_;

  for my $attrib (@{$attribNames}) {

	# Don't redefine existing method
	next if defined($methods->{$attrib});

	my $attribSym = LL::Symbol->new($attrib);
	my ($args, $body) = attrib_parts($attrib);
	$methods->{$attrib} = mk_method($attribSym, $args, $fields, $body);
  }
}


sub builtin_class {
  my ($superclass, $body, $name) = @_;

  $superclass->checkClass(" in superclass for class declaration.");
  $name->checkString(" in _::class name argument.");

  my ($fields, $attribNames) = class_fields($body);
  die "Attempted to create fields in non-struct class.\n"
	if (scalar keys %{$fields} > 0 &&
		!$superclass->isStructuredClass());

  my $methods = class_methods($body, $fields);
  class_attributes ($attribNames, $body, $fields, $methods);

  my $class = LL::Class->new([keys %{$fields}], $methods, $superclass, 1,
							 0, $name);

  return $class;
}


sub builtin_class_ext {
  my ($class, $body) = @_;

  $class->checkClass();
  my $methods = class_methods($body, {});
  $class->addMethods ($methods);

  LL::Class->refreshAllBuiltinClassMethodCaches();

  return $class;
}


sub builtin_new {
  my ($class, @args) = @_;

  $class->checkClass(" in function 'new'.");

  my $obj = LL::Struct->new($class);

  my $init = $class->{methodCache}->{_init};	# avoid lookup() to skip dnu
  if ($init) {
	$init->($obj, @args);
  }

  return $obj;
}




sub basicLookup {
  my ($context, $name) = @_;
  checkNargs(\@_, 2);

  $name->checkSymbol(" in 'defined' or 'lookup'");
  my $nm = $context->findFullName(${$name});

  return
	if !$nm
	  || !$context->isLegallyAccessible($nm)
  	  || !$context->isQualified($nm);

  return $context->lookup($nm);
}


sub builtin_definedfn {
  return boolObj(!! basicLookup(@_));
}

sub builtin_lookup {
  my $val = basicLookup(@_);
  die "Undefined symbol '${$_[1]}'\n" unless $val;
  return $val;
}
