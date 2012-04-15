#!/usr/bin/perl

# Copyright (C) 2010-2012 Chris Reuter; GPLv2+Exception; No Warranty.

# The turtle lives 'twixt plated decks
# Which practically conceal its sex.
# I think it clever of the turtle
# In such a fix to be so fertile.
#       -- Ogden Nash

# This is an initial implementation of the Deck programming language.
#
# Note that this source code violates a number of guidelines for
# writing good Perl code.  While this was done for good (IMHO)
# reasons, it means that this program should not be considered an
# example of how to write clean, maintainable Perl code.  For that, I
# strongly recommend reading Damien Conway's book "Perl Best
# Practices".




use strict;
use warnings;

use feature "switch";

# Forward-declare NIL as an alias for LL::Nil::NIL.
sub NIL {return LL::Nil::NIL()}

# ---------------------------------------------------------------------------

package LL::Name;

# NEVER SET THIS VARIABLE.  Instead, use 'local' to create a localized
# version and modify that.
our $Namespace = 'Main';

sub IsQualified {
  my ($name) = @_;

  return $name =~ /\:\:/;
}

sub Normalize {
  my ($name) = @_;

  # Sanity check!
  die "Undefined argument to Normalize().\n" unless defined($name);
  die "Normalize: Expecting string, not reference!\n"
       unless ref($name) eq '';

  return $name if IsQualified($name);
  return $Namespace . '::' . $name;
}


# Split a name into ($namespace, $name) pairs
sub SplitName {
  my ($name) = @_;

  my @np = split (/::/, $name);
  my $namePart = pop @np;
  my $namespacePart = join ("::", @np);

  return ($namespacePart, $namePart);
}


# Test if $name is a member of namespace $namespace.  If $name is
# unqualified, the answer is always false.
sub IsMemberOf {
  my ($name, $namespace) = @_;

  return 0 unless IsQualified($name);

  my ($ns, $nm) = SplitName($name);
  return 1 if $namespace eq $ns;
}


# Test if $name is legally accessible from the current namespace
# (assuming it exists, that is).  Basically, it is either public or
# it's private but to the current namespace.
sub IsLegallyAccessible {
  my ($name) = @_;

  return 1 unless IsQualified($name);

  my ($ns, $namePart) = SplitName($name);
  return 1 if $ns eq $Namespace;
  return 1 unless $namePart =~ /^_/;

  return 0;
}


# ---------------------------------------------------------------------------

package LL::GlobalContext;

sub new {
  my ($class) = @_;
  return bless {
                # Reserved fields:
                ' namespaces'       => {},  # The set of declared namespaces
                ' imported symbols' => {},  # The set of names that are imports
                ' forward decls'    => {},  # The set of forward proc decl'ns
                ' consts'           => {},  # <- list of const names
               }, $class;
}

sub _chkns {
  my ($self, $ns) = @_;
  die "Undefined namespace '$ns'\n"
    unless defined($self->{' namespaces'}->{$ns});
}

# Ensure $name is a valid Deck variable
sub checkName {
  my ($self, $name) = @_;

  (LL::Symbol->new($name))->checkValidName(" as variable name.");
}

sub names {
  my ($self) = @_;

  return grep { $_ !~ /^ / } keys %{$self};
}

sub present {
  my ($self, $name) = @_;

  $name = LL::Name::Normalize($name);
  return exists($self->{$name});
}

# Test for the presence of $name without qualifying the name
sub presentAsIs {
  my ($self, $name) = @_;

  exists($self->{$name}) and return 1;
  return 0;
}

sub lookup {
  my ($self, $name) = @_;

  $name = LL::Name::Normalize($name);
  exists($self->{$name}) and return $self->{$name};

  die "Unknown variable: '$name'\n";    # not reached.
}

sub isConst {
  my ($self, $name) = @_;

  $name = LL::Name::Normalize($name);
  exists($self->{$name}) and return !! $self->{' consts'}->{$name};

  die "Unknown variable: '$name'\n";    # not reached.
}

sub defNamespace {
  my ($self, $ns) = @_;
  $self->checkName($ns);
  $self->{' namespaces'}->{$ns} = 1;
}

# Create the subnamespace in the macro namespace ('__') for $name.
sub ensureMacroNamespace {
  my ($self, $name) = @_;

  $name = '__::' . LL::Name::Normalize($name);
  my ($ns) = LL::Name::SplitName($name);

  $self->defNamespace($ns);
}

sub hasNamespace {
  my ($self, $ns) = @_;
  return exists($self->{' namespaces'}->{$ns});
}

sub def {
  my ($self, $name) = @_;

  die "def: Expecting string, not reference!\n" unless ref($name) eq '';

  $name = LL::Name::Normalize($name);

  $self->checkName($name);
  $self->checkScopeFor($name);

  die "Redefinition of name '$name'.\n"
    if exists($self->{$name});

  return $self->{$name} = main::NIL;
}

sub set {
  my ($self, $name, $value) = @_;

  die "set: Expecting string, not reference!\n" unless ref($name) eq '';

  $name = LL::Name::Normalize($name);

  die "Attempted to modify a const: $name.\n"
    if defined($self->{' consts'}->{$name});

  exists($self->{$name}) and do {
    $self->{$name} = $value;
    return $value;
  };

  fail ("Unknown variable: '$name'\n"); # not reached
}


sub defset {
  my ($self, $name, $value) = @_;

  $self->def($name);
  $self->set($name, $value);

  return $value;
}

sub defsetconst {
  my ($self, $name, $value) = @_;

  $name = LL::Name::Normalize($name);
  $self->defset($name, $value);
  $self->{' consts'}->{$name} = 1;

  return $value;
}

# Modifies a const.  Not something that user code should ever do.
sub setGlobalConst {
  my ($self, $name, $value) = @_;

  $name = LL::Name::Normalize($name);

  die "setGlobalConst called on a non-const name '$name'\n"
    unless defined($self->{' consts'}->{$name});

  delete($self->{' consts'}->{$name});
  $self->set($name, $value);
  $self->{' consts'}->{$name} = 1;

  return $value;
}

# Ensure $name is valid for this context
sub checkScopeFor {
  my ($self, $name) = @_;

  # We allow qualified names here but the namespace must be declared.
  my ($namespace, $baseName) = LL::Name::SplitName($name);
  $self->_chkns($namespace);

  return 1;
}


# Copy all public names in namespace $src to namespace $dest
sub importPublic {
  my ($self, $src, $dest, $withNames, $withoutNames, $renameNames) = @_;

  $self->_chkns($src);
  $self->_chkns($dest);

  my @srcNames = ();
  foreach my $key (keys %{$self}) {
    next if $key =~ /^\s/;  # Skip internal variables

    # Skip existing imports
    next if defined($self->{' imported symbols'}->{$key});

    my ($namespace, $name) = LL::Name::SplitName($key);
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


# Add a forward declaration to the list
sub addForward {
  my ($self, $name, $args) = @_;
  $args ||= 1;  # Ensure $args is a true value.

  $name = LL::Name::Normalize($name);

  die "Forward declaration on undefined name '$name'\n"
    unless defined($self->{$name});

  $self->{' forward decls'}->{$name} = $args;
}

sub getForward {
  my ($self, $name) = @_;

  return $self->{' forward decls'}->{$name};
}


# Return the arg. list for $name if it is an mproc (i.e. defined with
# an arg. list); undef otherwise.
sub mprocForwardArgs {
  my ($self, $name) = @_;

  my $list = $self->{' forward decls'}->{$name};
  return unless defined($list);
  return $list;
}


# Clear all decl's in the given namespace (defaults to current) and
# return the list of deleted names.
sub clearForwards {
  my ($self) = @_;
  my $namespace = $LL::Name::Namespace;

  my @forwards = ();

  for my $forward (keys %{$self->{' forward decls'}}) {
    next unless $forward =~ /^${namespace}\:\:/;

    push @forwards, $forward;
    delete($self->{' forward decls'}->{$forward});
  }

  return @forwards;
}

# ---------------------------------------------------------------------------

package LL::ContextId;

# Instances don't need to do anything except be unique and instances
# of LL::ContextId.
sub new {
  my ($class) = @_;

  return bless {}, $class;
}


# ---------------------------------------------------------------------------

package LL::CompileState;

use Data::Dumper;

sub new {
  my ($class, $desc, $isVararg, $parent, $prefix, $isGlobal, $isClass,
      $namespace) = @_;

  # If there's no parent, we default to the global namespace
  if (!$parent && !$isGlobal) {
    $parent = LL::CompileState->new($desc, 0, undef, "", 1, 0);
  }

  my $self =
    {names      => {},
     args       => [],
     parent     => $parent,
     isGlobal   => $isGlobal,
     isVararg   => $isVararg,
     desc       => $desc,
     indent     => ($parent && $parent->{indent}) || 0,
     hasDynSub  => 0,
     isClass    => $isClass,
     varPrefix  => $prefix,
     namespace  => $namespace,
    };

  return bless $self, $class;
}

# Create an instance from field lists.  This gets used to recreate
#sub newFrom {
#  my ($class, $parent, $args, $vars, $isGlobal) = @_;
#}

sub desc     {my ($self) = @_; return $self->{desc}}
sub isVararg {my ($self) = @_; return $self->{isVararg}}
sub args     {my ($self) = @_; return [ @{$self->{args}} ]}
sub names    {my ($self) = @_; return [ sort keys %{$self->{names}} ]}
sub namespace{my ($self) = @_; return $self->{namespace} }


sub hasDynSub {my ($self) = @_; return $self->{hasDynSub}}
sub foundDynSub {
  my ($self) = @_;
  return $self->{hasDynSub} = 1;
}

sub indent   {my ($self) = @_; return '  ' x $self->{indent} };
sub indentBy {
  my ($self, $num) = @_;

  $self->{indent} += $num;
}

sub localDeclVarNames {
  my ($self) = @_;
  my %names = %{ $self->{names} };

  for my $arg (@{ $self->{args} }) {
    delete $names{$arg};
  }

  return [sort keys %names];
}

sub addRef {
  my ($self, $name, $defIndex, $isConst, $isInstVar) = @_;

  $self->{names}->{$name} = [$defIndex, $isConst, $isInstVar || 0];
}

sub addClass {
  my ($self, $class) = @_;

  for my $name ($class->names()) {
    $self->addRef($name, -1, $class->isConst($name), 1);
  }
}

sub addArgRef {
  my ($self, $name, $isConst) = @_;

  push @{$self->{args}}, $name;
  $self->addRef($name, -1, $isConst);
}

sub hasRef {
  my ($self, $name) = @_;

  return defined($self->{names}->{$name});
}

sub isConst {
  my ($self, $name) = @_;

  return $self->_getRef($name)->[1];
}

sub _getRef {
  my ($self, $name) = @_;

  defined($self->{names}->{$name}) and return $self->{names}->{$name};
  defined($self->{parent})         and return $self->{parent}->_getRef($name);

  return unless $LL::Main::Globals->present($name);

  return [-1, $LL::Main::Globals->isConst($name), 0];
}

sub isUndefined {
  my ($self, $name) = @_;

  # Ensure that $name is legal in the current namespace.  Should only
  # do anything in the global namespace.
  return 1 if !LL::Name::IsLegallyAccessible($name);

  return !$self->_getRef($name);
}


sub lookupExpr {
  my ($self, $name) = @_;

  # Ensure that $name is legal in the current namespace (global only!)
  return undef unless LL::Name::IsLegallyAccessible($name);

  # If this represents a global context, check the global namespace.
  if ($self->{isGlobal}) {
    my $normName = LL::Name::Normalize($name);
    my $expr = "\$LL::Main::Globals->{'$normName'}";

    return $expr if $LL::Main::Globals->present($normName);
    return $expr if defined($self->{names}->{$name});

    return undef;
  }

  # If it's defined in $self...
  return $self->perlVar($name) if defined($self->{names}->{$name});

  # See if it's in the parent.
  return $self->{parent}->lookupExpr($name) if $self->{parent};

  # Not found.
  return undef;
}


# Given name $name, return the name of a Perl variable that will hold
# it underneath.  Non-word variables will be converted.  Instance
# variables are converted to hash field access expressions.
sub perlVar {
  my ($self, $name) = @_;

  die "Qualified name to _perlVar.\n"
    if LL::Name::IsQualified($name);
  die "Escaped name got through!\n"
    if $name =~ /^\\/;

  if ($self->{isClass}) {
    my $name = $self->{varPrefix}.$name;
    return '$V_self->{\'' . $name . '\'}';
  }

  return "\$V_$name" if $name =~ /^\w+$/;

  my %escapes = ('-'    => 'minus',
                 '.'    => 'dot',
                 '|'    => 'pipe',
                 '!'    => 'bang',
                 '@'    => 'at',
                 "\$"   => 'buck',
                 '%'    => 'percent',
                 '^'    => 'caret',
                 '&'    => 'and',
                 '*'    => 'star',
                 '+'    => 'plus',
                 '='    => 'eq',
                 '?'    => 'huh',
                 '<'    => 'lt',
                 '>'    => 'gt',
                 '/'    => 'slash');
  my $esc = sub {
    $_ = shift;
    defined($escapes{$_}) && return $escapes{$_};
    return sprintf ("x%x", ord($_));
  };

  $name =~ s/(.)/'_'.$esc->($1)/eg;

  return "\$VV$name";
}



# Return an expression that defines local variable $name.  $name must
# be defined in $self--it may not be inherited from a parent
# namespace.
sub declExpr {
  my ($self, $name) = @_;

  die "Unknown name '$name'\n" if $self->isUndefined($name);    # Sanity check

  # Globals are special so we handle them separately.
  if ($self->{isGlobal}) {
    my $def = $self->{names}->{$name}->[1] ? 'defsetconst' : 'defset';
    return "\$LL::Main::Globals->$def('$name', NIL);";
  }

  return 'my ' . $self->perlVar($name) . ' = NIL;';
}


# Return perl source that will create a new instance of $self
sub perlConstructorSrc {
  my ($self) = @_;

  my $src = Data::Dumper->Dump([$self]);

  # Strip the assignment off the front.  This is a bit hacky.
  $src =~ s/^\s*\$VAR1\s*=\s*//;

  return $src;
}


# Return a sorted list of all names held by $self or any of its
# parents that are not global.
sub _allLocalNames {
  my ($self) = @_;

  return () if $self->{isGlobal};

  my @names = keys %{$self->{names}};

  push @names, $self->{parent}->_allLocalNames()
    if $self->{parent};

  @names = sort @names;

  return @names;
}


# Emit a series of expressions that reference all of the variables.
# The expression will probably be optimized away but it's enough to
# force Perl to create the variable.
sub forcedRefExpression {
  my ($self) = @_;

  return "" if $self->{isGlobal};

  my $nameList = join(',', map {$self->perlVar($_)} $self->_allLocalNames());
  my $expr = $self->indent() . "($nameList) if 0;\n";

  return $expr;
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

# Printable type name, used by check*().
sub _nm {
  my ($self) = @_;

  my $nm = $self->class()->{name};
  if (!$nm) {
    $nm = ref($self);
    $nm =~ s/^LL:://;
  }

  return $nm;
}

sub checkType {
  my ($self, $type, $name) = @_;

  my $fname = "";
  $fname = " in '$name'" if $name;

  $self->isa("LL::$type")
    or die "Expected 'LL::$type'; got @{[ref($self)]}$fname.\n";
}


# Exit with a message and backtrace as a result of unexpected type.
sub tbt ( $ ) {
  my ($msg) = @_;
  LL::Main::backtrace("Type error: $msg");
}

sub checkNumber {tbt "Expected number, got @{[(shift)->_nm()]}@_"}
sub checkByte   {tbt "Expected integer from 0 to 255, got @{[(shift)->_nm()]}@_"}
sub checkString {tbt "Expected string, got @{[(shift)->_nm()]}@_"}
sub checkList   {tbt "Expected list, got @{[(shift)->_nm()]}@_"}
sub checkSymbol {tbt "Expected symbol, got @{[(shift)->_nm()]}@_"}
sub checkQuote  {tbt "Expected quoted expr, got @{[(shift)->_nm()]}@_"}
sub checkQtLoL  {tbt "Expected quoted LoL, got @{[(shift)->_nm()]}@_"}
sub checkQtSym  {tbt "Expected quoted Symbol, got @{[(shift)->_nm()]}@_"}
sub checkLoL    {tbt "Expected LoL, got @{[(shift)->_nm()]}@_"}
sub checkFun    {tbt "Expected procedure, got @{[(shift)->_nm()]}@_"}
sub checkClass  {tbt "Expected class, got @{[(shift)->_nm()]}@_"}
sub checkStruct {tbt "Expected struct, got @{[(shift)->_nm()]}@_"}
sub checkLocalName {tbt "Expected unqualified name, got @{[(shift)->_nm()]}@_"}
sub checkValidName {tbt "Expected valid name, got @{[(shift)->_nm()]}@_"}
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
sub isProcedure {return 0}
sub isContinuation {return 0}
sub isCallable {return 0}
sub isTrue {return 1}
sub isNumber {return 0}
sub isQtLoL {return 0}  # Is a quoted list containing only lists
sub isQtList {return 0} # Is a quoted list?
sub isQtSym {return 0}  # Is a quoted Symbol?
sub isLoL {return 0}    # Is an list containing only lists
sub isPerlObj {return 0}
sub isClass {return 0}
sub isStruct {return 0}
sub isUndefinedProcedure {return 0}
sub matchesOpen {return 0}
sub storeStr {my ($self) = @_; return "${$self}"}
sub printStr {my ($self) = @_; return $self->storeStr()};
sub perlForm {my ($self) = @_; tbt "No perl form for @{[$self->printStr]}"}

sub equals {
  my ($self, $other) = @_;
  return LL::Main::boolObj(ref($self) eq ref($other) &&
                           $self->_inTypeEq($other));
}
sub _inTypeEq {my ($self, $other) = @_; return $self == $other }

sub checkIndexable {
  my ($self) = @_;
  tbt "Expecting indexable type, got @{[ref($self)]}"
    unless $self->isIndexable();
}


# Return the corresponding Deck class for $self.  This won't work
# until initGlobals has been called.
{
  my %builtinClasses;       # Registry of built-in classes

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
                 $self->deckCall('size_get')->perlForm()}



package LL::Number;
use base 'LL::Object';

sub new {
  my ($class, $numOrRef, $xxx) = @_;

  # Force Perl to turn $numOrRef into a number.
  $numOrRef += 0    unless ref($numOrRef);
  ${$numOrRef} += 0 if ref($numOrRef);

  return $class->SUPER::new($numOrRef, $xxx);
}

sub checkNumber {}
sub checkByte {
  my ($self) = @_;
  my $val = ${$self};

  $self->SUPER::checkByte()
    unless ( $val >= 0 && $val <= 255 && $val eq int($val) );
}

sub isTrue {my ($self) = @_; !! ${$self} }
sub isLiteral {return 1}
sub isNumber {return 1}
sub _inTypeEq {my ($self, $other) = @_; return ${$self} == ${$other} }
sub perlForm {my ($self) = @_; return ${$self}}

sub perlConstructorSrc {
  my ($self) = @_;

  return "LL::Number->new(${$self})";
}


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
sub isLiteral {return 1}        # ???
sub storeStr {my ($self) = @_; return "\"${$self}\""};
sub isString {return 1}

sub perlConstructorSrc {
  my ($self) = @_;

  my $selfStr = LL::Main::perlLit(${$self});
  return "LL::String->new($selfStr)";
}

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

sub newContaining {
  my ($class, $contents) = @_;

  die "Expecting scalar reference, got '$contents'\n"
    unless ref($contents) eq 'SCALAR';

  my $self = $contents;
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

sub perlConstructorSrc {
  my ($self) = @_;

  return "LL::Symbol->new('${$self}')";
}


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
sub _inTypeEq {
  my ($self, $other) = @_;

  return 0 unless scalar @{$self} == scalar @{$other};

  for my $n (0 .. $#{$self}) {
    return 0 unless ( $self->[$n] -> equals($other->[$n]) )->isTrue();
  }

  return 1;
}

sub isLoL { # Is this a list containing only lists?
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

    $entry->value()->unescapeAllOperators()
      if ($entry->isQuote() && $entry->value()->isList());

    # Quoted objects are a special case.
    if ($entry->isQuote() && $entry->value()->isEscapedOperator()) {
      $entry->[0] = $entry->[0]->asUnescapedOperator();
    }

  }

  return undef;
}


# Test if $self is a well-formed infix expression with only boolean
# (&& and ||) operators.
sub isBooleanInfix {
  my ($self) = @_;

  return 0 unless scalar @{$self} % 2 == 1 && scalar @{$self} >= 3;

  my $index = 0;
  for my $item (@{$self}) {
    return 0 if $index % 2 == 0 && $item->isUnescapedOperator();

    if ($index % 2 == 1) {
      return 0 if !$item->isUnescapedOperator();
      return 0 unless (${$item} eq '&&' || ${$item} eq '||');
    }
    $index++;
  }
  return 1;
}


# Assignment statements are turned infix
sub couldBeImpliedInfix {
  my ($self) = @_;

  # Booleans for flow control are allowed.
  return 1 if $self->isBooleanInfix();

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


# Test if $self looks like it was a malformed infix expression that
# went through the wash^W^WasPrefixList().
sub looksLikeMalformedInfix {
  my ($self) = @_;

  return 0 unless scalar @{$self} > 1;

  # 'var' and 'const' are special cases
  return 0 if
    $self->[0]->isSymbol() && ${ $self->[0] } =~ /^(var|const)$/;

  # See if any element other than the first is an unescaped operator.
  for my $elem (@{$self}[1 .. $#{$self}]) {
    return 1 if $elem->isUnescapedOperator();
  }

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
# into infix subexpressions (more precisely: infix subexpressions that
# have been parsed to prefix).  $oper is a regex that matches the
# operator.
sub withAutoInfixed {
  my ($self, $opRegex) = @_;

  my @result = @{$self};

  while (1) {
    my $dotIndex = 0;

    # Find '<expr> . <expr>' or '<expr> => <expr>' sequences
    my $oper;
    for my $item (@result) {
      if ($item->isUnescapedOperator() && ${$item} =~ /^($opRegex)$/) {
        $oper = $1;
        last;
      }
      $dotIndex++;
    }
    last if $dotIndex > $#result;

    # Check for errors
    die "Unescaped '$oper' at start or end of expression.\n"
      if ($dotIndex == 0 || $dotIndex == $#result);

    # Replace the sequence with a single sub-expression (prefix) of
    # the operator being called on left and right operands
    my $expr = LL::List->new([]);
    my @op = splice @result, $dotIndex-1, 3, $expr;
    push @{$expr}, $op[1], $op[0], $op[2];  # Order makes it prefix
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

# If self is a LoL and the first item contains only a string, remove
# it and return the string.
sub stripDocString {
  my ($self) = @_;

  return undef unless $self->isLoL();
  return undef unless $self->size() > 0;
  return undef unless $self->[0]->size() == 1;
  return undef unless $self->[0]->[0]->isString();

  my $result = $self->[0]->[0];
  @{$self} = @{$self}[1..$#{$self}];

  return ${$result};
}


sub _sanitizeIndex {
  my ($self, $index) = @_;

  $index->checkNumber(" as list index.");
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

sub perlConstructorSrc {
  my ($self) = @_;

  my @constructors = map { $_->perlConstructorSrc() } @{$self};
  return 'LL::List->new([' . join(',', @constructors) . '])';
}



package LL::InfixList;
use base 'LL::List';
sub isInfixList {return 1}
sub withAutoInfixDone {my ($self) = @_; return $self}   # asPrefixList does it.

{
  my %precPerOp;
  my $userPrec;
  BEGIN {
    my @precedence = ([qw{. @ =>}], # field lookup, seq. access, closure
                      [qw{->}],     # method lookup
                      [qw{**}],     # power
                      [qw{* / // %}],# mult, div, div rounded toward zero, mod
                      [qw{+ -}],    # add, subtract
                      [qw{<< >> >>>
                          <<<}],    # shifts
                      [qw{== === != !== < > <= >=
                          <=>}],    # Equality and magnitude
                      [qw{&}],      # Bitwise AND.
                      [qw{| ^}],    # Bitwise OR, bitwise XOR
                      [qw{&&}],     # Logical AND, short-circuited
                      [qw{||}],     # Logical OR, short-circuited
                      [qw{}],       # User-defined operators
                      [qw{=}],      # Assignment
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

      my $p = $precPerOp{${$entry}} || $userPrec;
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
sub _inTypeEq {my ($self, $other) = @_; $other->isNil}
sub perlForm {my ($self) = @_; return undef}

use constant NIL => LL::Nil->new(); # The only instance you should use

package LL::Eol;
use base 'LL::Object';
sub isEol {return 1}
sub isTrue {return 0}   # Maybe not necessary
sub storeStr {"<EOL>"}
sub isExplicitEol {my ($self) = @_; return ${$self} eq ';'}

package LL::Eof;
use base 'LL::Object';
sub isEof {return 1}
sub isTrue {return 0}   # Maybe not necessary
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

sub perlConstructorSrc {
  my ($self) = @_;
  return "LL::Quote->new(" . $self->value()->perlConstructorSrc() . ")";
}

sub isQuote {return 1}
sub checkQuote {}
sub value {my ($self) = @_; return $self->[0]}
sub storeStr {my ($self) = @_; return ':' . $self->value()->storeStr()}

sub isQtLoL {   # Is this a quoted list of lists?
  my ($self) = @_;
  return $self->value()->isLoL();
}

sub isQtList {  # Is this a quoted list?
  my ($self) = @_;
  return $self->value()->isList();
}

sub isQtSym {
  my ($self) = @_;
  return $self->value()->isSymbol();
}


sub checkQtLoL {
  my ($self, @args) = @_;
  die "Expecting a quoted LoL, got @{[$self->storeStr()]}@_\n"
    unless $self->isQtLoL();
}

sub checkQtSym {
  my ($self, @args) = @_;
  die "Expecting a quoted Symbol, got @{[$self->storeStr()]}@_\n"
    unless $self->isQtSym();
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


package LL::Procedure;
use base 'LL::Object';
sub isAtom {return 1}
sub isProcedure {return 1}
sub checkFun {}
sub isCallable {return 1}
sub storeStr {return "<procedure>"}
sub perlForm {my ($self) = @_; return $self}

package LL::Method;
use base 'LL::Procedure';
sub storeStr {return "<method>"}

package LL::MethodCall;
use base 'LL::Procedure';
sub storeStr {return "<method call>"}

package LL::Continuation;
use base 'LL::Procedure';
sub storeStr {return "<return continuation>"}
sub isContinuation {return 1}

package LL::UndefinedProcedure;
use base 'LL::Procedure';
sub storeStr {return "<undefined procedure>"}
sub isUndefinedProcedure {return 1}
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
use base qw{LL::Object};

sub isStruct {return 1}
sub checkStruct {}
sub class {my ($self) = @_; return $self->{' class'};}

sub new {
  my ($class, $deckClass) = @_;
  my $self = {};
  $self->{' class'} = $deckClass;

  my $prefix = $deckClass->varprefix();
  for my $field ( $deckClass->heirarchyFields() ) {
    $self->{$prefix.$field} = main::NIL;
  }

  return bless $self, $class;
}

# Make a shallow copy of $self.
sub shallowCopy {
  my ($self) = @_;

  my $copy = { %{$self} };
  return bless $copy, ref($self);
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
  my ($class, $fields, $fieldPrefix, $methods, $superclass, $structured,
      $builtin, $name)=@_;

  my $self = {fields        => $fields,
              methods       => $methods,
              methodCache   => {},
              superclass    => $superclass,
              structured    => $structured,
              builtin       => $builtin,
              name          => $name,
              varprefix     => $fieldPrefix,
              namespace     => $LL::Name::Namespace,
             };

  bless $self, $class;
  $self->refreshCache();

  {# Ensure that names aren't reused in the heirarchy.  To be removed.
    my $names = {};
    for my $name ( $self->heirarchyFields() ) {
      LL::Main::fail ("Redefinition of field '$name'.\n")
          if defined( $names->{$self->varprefix().$name} );
      $names->{$name} = 1;
    }
  }

  return $self;
}


sub varprefix {my ($self) = @_; return $self->{varprefix}}

sub isStructuredClass {
  my ($self) = @_;
  return !!$self->{structured};
}


sub addMethods {
  my ($self, $methods) = @_;

  $self->{methods} = { %{$self->{methods}}, %{$methods} };
  $self->refreshCache();
}

sub heirarchyFields {
  my ($self) = @_;

  my @fields = ();
  if (!$self->{superclass}->isNil()) {
    push @fields, $self->{superclass}->heirarchyFields();
  }

  for my $name (@{$self->{fields}}) {
    push @fields, $name;
  }

  return @fields;
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
use Scalar::Util qw(looks_like_number blessed reftype);
#use UNIVERSAL 'isa';       # Deprecated but I need it to identify LL::Objects
use Cwd qw{abs_path getcwd};
use File::Basename;

sub NIL {return LL::Nil::NIL;}

use constant VERSION => "0.02";
use constant TRUE => LL::Number->new(1);

# Compilable source types
use constant MACRO      => 'macro';
use constant PROC       => 'proc';
use constant SUB        => 'sub';
use constant METHOD     => 'method';
use constant TOPLEVEL   => 'toplevel';



our $Input = undef;     # Input filehandle or undef for stdin.
my $NeedPrompt = 0;     # If true, reader is inside a LoL Line
our $Globals = LL::GlobalContext->new();
my %fnNeedsContext;     # Hash of functions that get the parent's context
my %DocStringHash;      # Hash of docstring information
our $BT;                # Backtrace list.  DO NOT SET; LOCALIZE INSTEAD.

our $CanCallMacro = 0;  # Do not change this!  Use local to override instead.
# ---------------------------------------------------------------------------

# Flags:
my $dumpExpr = 0;
my $dumpPerlSrc = 0;
my $noLib = 0;
my $enableBacktrace = 0;

use Getopt::Long;

{
  my $noNewCompiler = 0;
  my $args = pullOutArgs();
  my $dummy;
  my $flush = 0;

  GetOptions ('dump-expr'           => \$dumpExpr,
              'show-perl'           => \$dumpPerlSrc,
              'no-lib'              => \$noLib,
              'backtrace'           => \$enableBacktrace,
              'flush'               => \$flush,
              'xxxxx'               => \$dummy)
    or die "Invalid argument.\n";

  $| = 1 if $flush;

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
      print "Deck version @{[VERSION]}\n";
      print "Copyright (C) 2011-2012 Chris Reuter, GLPv2+exception, NO WARRANTY!\n";
      print "See documentation for details.\n";
      readfile('', 'Main', 0, 1);
    };

    last unless $@;
    print "Error: $@\n";
  }

  print "\n";
}


sub readfile {
  my ($file, $module, $checkName, $print, $isInternalImport) = @_;

  local $Input;     # Push for the life of this function
  if ($file) {
    open $Input, "<$file"
      or die "Unable to open '$file'.";
  }

  # Adopt the expected namespace.
  local $LL::Name::Namespace = $module;

  my $emptyList = LL::List->new([]);

  my $checkForDocstring = 0;
  while (1) {
    $NeedPrompt = 1;        # We are at the start of a logical LoL line

    my $expr = readLoLLine(0);
    next if ($expr->isEmptyList());

    # If $file was loaded via a 'use' directive (setting $checkName to
    # 1), the first line MUST be a 'package' declaration and the
    # package's module must match '$module'.
    if ($checkName) {
      $checkName = 0;
      $checkForDocstring = 1;

      checkPkgDecl($file, $module, $expr);

      # Special case: if this import was launched during environment
      # setup, we don't need to import 'Lang' into it.  (This is
      # because 'Lang' is the only module for which this is true.  If
      # we end up importing more things, this code will likely need to
      # change.)
      next if $isInternalImport;

      $Globals->importPublic('Lang', $module);
      next;
    }

    # This should get called on the second non-blank input line of a module.
    if ($checkForDocstring) {
      $checkForDocstring = 0;

      if ($expr->size() == 1 && $expr->[0]->isString()) {
        addPackageDocString($module, ${ $expr->[0] });
        next;
      }
    }

    # Exit if the first (and only) element of $expr is EOF.  Ignore it
    # if EOF is at the end, since we'll see it again next iteration.
    last if ($expr->[0]->isEof());
    pop @{$expr} if $expr->[-1]->isEof();

    $expr = LL::List->new([$expr]);
    my $args = LL::List->new([]);

    my $fn = compile (undef, $args, $expr, $emptyList, 'toplevel', "*top*");

    my $result;
    eval {
      $result = $fn->();
    };
    if ($@) {
      die "Called return continuation on a returned procedure.\n"
        if ref($@) && $@->isa('LL::ContextId');
      die $@;
    }

    print $result->storeStr(), "\n"
      if $print;
  }

  clearForwardFns() if $Input;

  close $Input if $Input;
}


# Clear all forward declarations in the current namespace and ensure
# that they all now refer to defined procedures.
sub clearForwardFns {
  my $ns = $LL::Name::Namespace;

  my @forwards = $Globals->clearForwards();
  my $missing = "";
  for my $name (@forwards) {
    $missing .= "Undefined forward (m)proc declaration '$name' in $ns\n"
      if ($Globals->present($name) &&
          $Globals->lookup($name)->isUndefinedProcedure());
  }

  die $missing if $missing;
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
  } else {
    # And do auto-infix conversion.
    $rlist = $rlist->withAutoInfixDone();
  }

  # Catch malformed expressions.
  die "Expression '@{[$rlist->printStr()]}' contains unescaped operators.\n"
    if $rlist->looksLikeMalformedInfix();

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

  return $openChar eq '('           ?
    LL::InfixList->new(\@result)    :
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
          chomp $line;
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

        $line =~ /^\"/ and do {
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

    # There needs to be an odd number of delimiters.  An even number
    # is interpreted as empty because there are matching opening and
    # closing quotes.
    return ($line, "") if ($delimCount % 2 == 0);

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
    $line =~ s/^(\-?)0x([0-9a-fA-F_]+)(\W?)/$3/ and do {
      my ($sign, $num) = ($1, $2);
      $num =~ s/_//g;
      $tok = oct("0x$num");
      $tok = -$tok if $sign eq '-';
      $result = LL::Number->new($tok);
      next;
    };

    # Octal literal
    $line =~ s/^(\-?)0o([0-9_]+)(\W?)/$3/ and do {
      my ($sign, $num) = ($1, $2);
      $num =~ s/_//g;
      die "Non-octal digit found in octal literal: '0o$num'\n"
        if $num =~ /[89]/;
      $tok = oct("0$num");
      $tok = -$tok if $sign eq '-';
      $result = LL::Number->new($tok);
      next;
    };

    # Binary literal
    $line =~ s/^(\-?)0b([01_]+)(\W?)/$3/ and do {
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
    $NeedPrompt = 0;    # Reset the next time we start reading an expression

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
  local $| = 1; # Buffering sometimes produces inconsistent test results
  print STDERR "WARNING: ", join(" ", @_), "\n";
}


# Given a Perl string, return a string which when evaluated by Perl
# returns the original string.  In other words, creates a string
# constant that is safe to parse.
sub perlLit {
  my ($string) = @_;

  $string =~ s/([^-0-9a-zA-Z: ])/sprintf('\x%02x', ord($1))/egmx;
  return '"'.$string.'"';
}


# Fatal error from Deck code.  Exit with a backtrace.  Assumed to be
# called as a result of user error.
sub fail ( $ ) {
  my ($msg) = @_;
  backtrace ("Code error: $msg");
}


# Given an error message from a 'die', parse the error message to see
# if it's a Perl error caused by trying to call a non-procedure as a
# procedure.  If so, throw a more meaningful error message using
# fail().
sub checkForInvalidInvoke {
  my ($msg) = @_;

  return unless $msg =~ /^Not a CODE reference /;
  fail("Attempted to call a non-procedure as a procedure.");
}


# Die with a backtrace.
sub backtrace {
  my $errorMsg = join("", @_, "\n");
  die $errorMsg unless $enableBacktrace;

  my $bt = "";
  my $frame = $LL::Main::BT;
  while ($frame) {
    $bt .= "  " . $frame->[0] . "\n";
    $frame = $frame->[1];
  }

  die "${errorMsg}Backtrace:\n$bt";
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
  return LL::List->new([]) if (ref($arg) eq 'ARRAY' && scalar @{$arg} == 0);
  return NIL unless defined($arg);
  return LL::Number->new($arg) if looks_like_number($arg);
  return LL::String->new($arg) unless ref($arg);

  given (ref($arg)) {
    when ('SCALAR') {
      return decktype(${$arg});
    }

    when ('ARRAY') {
      my $deckval = decktype(@{$arg});

      # Need to distinguish between single argument and one-list argument
      return LL::List->new([$deckval]) if scalar @{$arg} == 1;

      return $deckval;
    }

    default {
      return LL::PerlObj->new($arg);
    }
  }
}


sub chkMacro {
  fail ("Macro called as a function (i.e. by something other than\n" .
        "the compiler).")
    unless $LL::Main::CanCallMacro;
}

# Construct a new backtrace frame.
sub btframe {
  my ($oldframe, $name) = @_;
  return [$name, $oldframe];
}



# ---------------------------------------------------------------------------


# Given a string, split it into plain string chunks and the
# expressions to evaluate.  Expressions take the form of a list
# ref. containing the sigil as a string followed by the symbol name.
sub splitInterpString {
  my ($str) = @_;

  # First, divvy into vars and strings.
  my @parts = ("");

  # The empty string is a special case
  return [""] if $str eq "";

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

# If $expr is a macro invocation, evaluate it and return the result.
# Note that macros are allowed to return macro calls so this procedure
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

       print "*macro* ${$name}(@{[$expr->printStr()]}) ==> " if $dumpExpr;
       $expr = $val->($name, @{$expr}[1 .. $#{$expr}]);
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


# Expand all macros and strip (and return) the docstring.
sub launderProcBody {
  my ($body, $allowDocstring) = @_;

  my @fixedBody;
  my $docstring;

  my $first = 1;
  for my $expr (@{$body}) {

    # If the first item is a docstring, handle it.  For now, that
    # means just skipping it.
    if ($first) {
      $first = 0;

      if ($expr->size() == 1 && $expr->[0]->isString()) {
        fail ("Unexpected docstring.\n") unless $allowDocstring;

        $docstring = ${ $expr->[0] };
        next;
      }
    }

    my $newExpr = applyMacrosRecursively ($expr, $Globals);
    $newExpr->unescapeAllOperators();

    push @fixedBody, $newExpr;
  }

  return (LL::List->new(\@fixedBody), $docstring);
}

# Check the arg list for errors.  Also strip out the 'args' word.
sub checkArgs {
  my ($args) = @_;

  my $nargs = scalar @{$args};
  my $isVararg = $nargs > 0 && ${$args->[-1]} eq 'args';
  if ($isVararg) {
    pop @{$args};
    --$nargs;
  }

  # Test for illegal use of 'args'
  for my $arg (@{$args}) {
    fail "Invalid argument name '${$arg}'\n"
      if !$arg->isSymbol();
    fail "'args' not last in the argument list.\n"
      if ${$arg} eq 'args';
  }

  return ($args, $isVararg);
}


sub compile {
  my ($outerContext, $args, $body, $final, $mode, $desc, $namespace) = @_;

  # XXX remove?
  die "newCompile called on actual LL::Context instead of LL::CompileState\n"
    if ($outerContext && $outerContext->isa('LL::GlobalContext'));

  $args->checkList(" in argument to procedure '$desc'");
  $body->checkLoL(" in a procedure body ('$desc').");
  $final->checkLoL(" in a procedure final block ('$desc').");

  my $perlSrc = CompileToPerl($outerContext, $args, $body, $final,
                              $mode, $desc, $namespace);
  my $proc = doEval($perlSrc);

  print "# '$desc'\n$perlSrc\n" if ($dumpPerlSrc);

  # The eval should always succeed.  If not...
  die "Compile failed: '$@'\nSrc:\n$perlSrc\n" if $@;

  given ($mode) {
    when([PROC, TOPLEVEL, SUB]) {return LL::Procedure->new($proc)}
    when(MACRO)                 {return LL::Macro->new($proc)}
    when(METHOD)                {return LL::Method->new($proc)}

    default {die "Unknown mode: '$mode'\n"}
  }
}


# Evaluate in a minimal namespace
sub doEval {eval $_[0]};


# Called by compiled subs to evaluate calls to _::sub
sub DynamicSubSrc {
  my ($outerContext, $args, $body, $final) = @_;
  $final = LL::List->new([]) if $final->isNil();

  die "Null outerContext!\n" unless $outerContext;

  $args->checkList(" in argument to sub.");
  $body->checkLoL(" in a sub body.");
  $final->checkLoL(" in a sub final block.");

  my $src = CompileToPerl($outerContext, $args, $body, $final, SUB,
                          '*dynamic sub*');

  print "# dynamic sub\n$src\n" if $dumpPerlSrc;

  return $src;
}

# Given a Deck function, create the Perl source code that implements
# its behaviour.
sub CompileToPerl {
  my ($parentContext, $args, $body, $final, $mode, $desc) = @_;
  local $LL::Main::CanCallMacro = 1;

  my $docstring;
  my $isVararg;

  if ($mode eq PROC) {
    die "defined parent context in a proc.\n" if $parentContext;
    $parentContext = LL::CompileState->new("$desc outer", 0, undef, "", 1, 0);
  }

  # Expand macros (and get the docstring)
  { my $hasDoc = $mode ne TOPLEVEL && $mode ne SUB;

    ($body, $docstring) = launderProcBody($body, $hasDoc);
    ($final) = launderProcBody($final, 0);

    addProcDocString($desc, 0, $args->printStr(), $docstring);
  }

  # Launder the arg list and check for errors.
  ($args, $isVararg) = checkArgs($args);

  # Optimize for trivial functions.  (We exclude methods for now.  Fix later!)
  if ($mode ne METHOD && $body->isEmptyList() && $final->isEmptyList()) {
    return 'sub{NIL}';
  }

  # Do the actual code generation
  my $state = LL::CompileState->new($desc, $isVararg, $parentContext,
                                    "", ($mode eq TOPLEVEL), 0);
  AddExistingLocals ($state, $args, $mode);
  $state->indentBy(1);

  my $bodySrc  = CompileBody($state, $body, $mode eq SUB || $mode eq TOPLEVEL);
  my $finalSrc = CompileFinal($state, $final, scalar @{$body});

  my $preamble = CreatePreamble($state, $mode);
  my $postscript = CreatePostscript($state, $mode);

  # And return the source code.
  return $preamble."\n".$bodySrc."\n".$finalSrc."\n".$postscript;
}


# Expands a function call, evaluating several special cases as we go.
sub expandFuncCall {
  my ($state, $expr, $ndx) = @_;

  # First, let's see if $expr calls one of the special-case functions:
  my $fname = '';
  $fname = ${ $expr->[0] } if $expr->[0]->isSymbol();
  given($fname) {

    # _::value and Lang::value are optimized away
    when(/^(_|Lang)::value$/) {
      fail("Call to '$fname' without any arguments.") if scalar @{$expr} <= 1;
      return expandExpr($state, $expr->[-1]);
    }

    # _::sub is inlined if the arguments are all constants.
    when('_::sub') {
      my ($sub, $args, $body, $final) = @{$expr};

      # Handle nil final
      $final = LL::Quote->new( LL::List->new([]) )
        if $final == NIL;

      # Bail and treat as ordinary function call unless all arguments
      # are literals.
      fail "_::sub expects 4 arguments, got @{[scalar @{$expr} - 1]}."
        unless scalar @{$expr} == 4;

      # If all arguments are literal, compile the whole thing now.
      if ($args->isQtList() &&
          $body->isQtLoL() && ($final->isQtLoL() || $final->isNil())) {

        $state->indentBy(1);
        my $src = CompileToPerl($state, $args->value(), $body->value(),
                              $final->value(), SUB, "[@{[$state->desc()]}]");
        return "bless ($src, 'LL::Procedure')";
      }

      # Otherwise, return code to compile the arguments.
      $state->foundDynSub();

      shift @{$expr};
      my @exprCode = map { expandExpr($state, $_) } @{$expr};

      my $result =
        'bless (do{my $x = eval LL::Main::DynamicSubSrc($CContext, '
          . join (',', @exprCode) . ');'
            . 'fail "Fatal error in dynamic sub: $@" if $@; $x},'
              . '"LL::Procedure")';
      return $result;
    }

    when ('_::set') {
      my ($set, $name, $value) = @{$expr};

      fail "'set' expects 3 arguments, got @{[scalar @{$expr}]}\n"
        unless scalar @{$expr} == 3;

      $name->checkQtSym(" in variable assignment ('_::set') operation.");
      $name = $name->value();

      fail "Use of an undefined or private name '${$name}'."
        if $state->isUndefined(${$name});

      fail "Attempted to assign to const '${$name}'."
        if $state->isConst(${$name});

      my $dest = $state->lookupExpr(${$name});
      my $src = expandExpr($state, $value);
      return "$dest = $src";
    }

    when (/^_::(var|const)$/) {
      my ($var, @argPairs) = @{$expr};
      my $isConst = ${$var} eq '_::const';
      my $result = "";

      while (@argPairs) {
        my $qtName = shift @argPairs;
        my $value = shift @argPairs;

        $qtName->checkQtSym(" in a variable declaration.");
        my $name = ${ $qtName->value() };

        fail ("Redefinition of '$name'\n")
          if $state->hasRef($name);

        $state->addRef($name, $ndx, $isConst);

        next if $value->isNil();

        my $perlName = $state->lookupExpr($name);
        my $perlVal = expandExpr($state, $value, $ndx);

        $result .= "$perlName = $perlVal;";
      }

      return "NIL" if $result eq "";
      return 'do{'.$result.'NIL}';
    }
  }

  # Ordinary function call
  my @items = map { expandExpr($state, $_) } @{$expr};

  my $fn = shift @items;
  my $result = "$fn->(" . join(',', @items) . ')';

  return $result;
}



sub expandQuote {
  my ($state, $expr, $ndx) = @_;
  my $val = $expr->value();

  return $val->perlConstructorSrc()
    if ($val->isLiteral() || $val->isList() || $val->isSymbol() ||
        $val->isQuote());

  return expandExpr($state, $val, $ndx);
}


sub expandName {
  my ($state, $expr, $ndx) = @_;

  my $result = $state->lookupExpr(${$expr});
  fail ("Unknown variable '${$expr}' in '".$state->desc()."'.")
    unless $result;

  return $result;
}


sub expandExpr {
  my ($state, $expr, $ndx) = @_;

  $expr->isSymbol()     and return expandName($state, $expr, $ndx);
  $expr->isNil()        and return 'NIL';
  $expr->isList()       and return expandFuncCall($state, $expr, $ndx);
  $expr->isLiteral()    and return $expr->perlConstructorSrc();
  $expr->isQuote()      and return expandQuote($state, $expr, $ndx);

  # Should not be reached.  If we get here, it means $expr is not a
  # Deck type.
  die "Invalid expression type: @{[ref($expr) || $expr]}\n";
}


sub CompileChunk {
  my ($state, $body, $saveLast, $offset) = @_;

  $state->indentBy(1);
  my $pad = $state->indent();

  my $result = "";

  my $count = 0;
  for my $elem (@{$body}) {
    my $comment = $elem->printStr();
    $comment =~ s/^/"$pad#"/egmx;  # $comment can be multiple lines
    $result .= $comment . "\n";

    $result .= $pad;
    my $expr = expandExpr ($state, $elem, $count + $offset);

    if ($saveLast && $count == $#{$body}) {
      $expr = '$lastExpr = ' . $expr;
    }
    $result .= $expr . ";\n";

    $count++;
  }

  $state->indentBy(-1);
  return $result;
}


sub CreatePostscript {
  my ($state, $mode) = @_;
  my $result = "";

  $result .= $state->forcedRefExpression()
    if $state->hasDynSub();

  my $pad = $state->indent();
  $result .= "${pad}return \$lastExpr;\n";

  $state->indentBy(-1);
  $result .= $state->indent() . "}\n";

  return $result;
}


sub CompileFinal {
  my ($state, $body, $offset) = @_;

  my $pad = $state->indent();
  my $result = "";

  # Append the final block
  if (scalar @{$body} > 0) {
    $result .= "${pad}eval {\n";
    $result .= CompileChunk($state, $body, 0, $offset);
    $result .= $pad . "};\n";

    my $trap = <<'EOF';
die "Called return continuation from a final block.\n"
  if blessed($@) && $@->isa('LL::ContextId');
die $@ if $@;
EOF
    $trap =~ s/^/$pad/egmx;
    $result .= $trap;
  } else {
    $result .= "$pad#No final block\n";
  }

  # Append the text
  my $ret .= <<'EOF';
# Catch and prettify non-function-called-as-function error
checkForInvalidInvoke($mainStatus);

# If the return comes from a different context, rethrow it.
die $mainStatus
    if (ref($mainStatus) eq 'LL::ContextId' && $mainStatus != $contextId);

# Otherwise, we're done
#return $lastExpr;
EOF

  $ret =~ s/^/$pad/egmx;
  $result .= $ret;

  return $result;
}


sub CompileBody {
  my ($state, $body, $saveLast) = @_;

  my $pad = $state->indent();
  my $result = "";

  $result .= "${pad}eval {\n";
  $result .= CompileChunk($state, $body, $saveLast, 0);
  $result .= $pad . "};\n";

  my $trap = <<'EOF';
my $mainStatus = $@;

# Catch and prettify non-function-called-as-function error
checkForInvalidInvoke($mainStatus);

# Fail if it's an error, not a return.
die $mainStatus if ($mainStatus && ref($mainStatus) ne 'LL::ContextId');
EOF
  $trap =~ s/^/$pad/egmx;
  $result .= $trap;

  return $result;
}



# Add the existing locals.
sub AddExistingLocals {
  my ($state, $args, $mode) = @_;

  if ($mode eq METHOD) {
    $state->addArgRef('self', 1);
  }

  for my $name (@{$args}) {
    $name->checkSymbol(" in args for procedure @{[$state->desc()]}");
    $state->addArgRef(${$name}, 0);
  }

  $state->addArgRef('args', 0) if $state->isVararg();

  my $return = ($mode eq SUB) ? 'next' : 'return';
  $state->addRef($return) unless $mode eq TOPLEVEL;
}


sub CreateArgCountCheck {
  my ($state, $mode) = @_;

  # Check the arg. count.
  my $expectedArgCount = scalar @{ $state->args() };
  $expectedArgCount-- if ($state->isVararg());  # 'args' doesn't count

  my $dispArgCount = $expectedArgCount;
  --$dispArgCount if ($mode eq METHOD);     # self doesn't count.

  my $atLeast = $state->isVararg() ? "at least " : "";

  # We need to compensate for self if this is a method
  my $countExpr = ($mode ne METHOD) ? '@{[scalar @_]}' : '@{[scalar @_ - 1]}';

  # Expression that creates the error failure error message.
  my $s = $dispArgCount == 1 ? "" : "s";    # correctly pluralize
  my $dieMsg = $state->desc();
  $dieMsg = $dieMsg ? "'$dieMsg' " : "Current procedure ";
  $dieMsg .= "expects $atLeast$dispArgCount argument$s but got $countExpr.";

  # Expression that tests the arg. count
  my $cmpExpr = $state->isVararg() ?
    'scalar @_ < '.$expectedArgCount
      : 'scalar @_ != '.$expectedArgCount;

  return "fail(\"$dieMsg\") if ($cmpExpr);";
}



sub CreatePreamble {
  my ($state, $mode) = @_;

  my $pad = $state->indent();

  my $result = "sub {\n";

  # If tracing is on, enable the backtrace
  if ($enableBacktrace) {
    my $bt = 'local $LL::Main::BT = btframe($LL::Main::BT, "' .
      $state->desc() . '");';
    $result .= $pad . $bt . "\n";
  }

  # Set the namespace if required
  $result .= $pad.'local $LL::Name::Namespace=\''.$LL::Name::Namespace."\';\n";

  # Set the backtrace if enabled
  $result .= $pad.'local $LL::Main::BT = btframe($LL::Main::BT,' .
    LL::Main::perlLit($state->desc()) . ");\n";

  # If this is a macro, we need to insert a check for an illegal call:
  $result .= "${pad}LL::Main::chkMacro();\n"
    if $mode eq MACRO;

  # Add argument count check.
  $result .= $pad . CreateArgCountCheck($state, $mode) . "\n"
    unless $mode eq TOPLEVEL;

  # Add args (except for 'args', which is special).
  for my $name (@{ $state->args() }) {
    next if $name eq 'args';    # This gets handled specially later
    $result .= $pad.'my ' . $state->perlVar($name) . ' = shift;' . "\n";
  }

  # And the vararg, if present.
  if ($state->isVararg()) {
    $result .= $pad.'my ' . $state->perlVar('args') . ' = LL::List->new(\@_);'
      . "\n";
  }

  # Declare the locals.
  for my $name (@{$state->localDeclVarNames()}) {
    $result .= $pad . $state->declExpr($name) . "\n";
  }

  # Declare work variables.
  $result .= $pad . 'my $contextId = LL::ContextId->new();'."\n";
  $result .= $pad . 'my $lastExpr = ' . ($mode eq METHOD ? '$V_self' : 'NIL')
    . ";\n";

  # Declare the return function
  if ($mode ne TOPLEVEL) {
    my $return = ($mode eq SUB) ? 'next' : 'return';
    $result .= $pad . $state->perlVar($return) . '=' .
      'bless sub{my($rv)=@_; $rv||=NIL; $lastExpr=$rv;die $contextId},'
        . '"LL::Continuation";' . "\n";
  }

  # If necessary, declare the compiler context
  if ($state->hasDynSub()) {
    $result .= "${pad}my \$CContext = " . $state->perlConstructorSrc() . "\n";
  }

  return $result;
}





# ---------------------------------------------------------------------------

#[:method <name> <builtin> <classname> <methodname>
#   <args> <docstring>]
#[:attrib <name> <builtin> <class> :readable/:writeable/:public
#   <attribName> <docstring>]

# Name is of the form 'class->name'.  Attributes should look like
# method calls at this point.
sub addMethodDocString {
  my ($name, $builtin, $args, $docstring) = @_;

  my ($className, $methodName) = split (/\-\>/, $name);
  die "Malformed method name for docstring: '$name'\n"
    unless $methodName;

  $args =~ s/\[|\]//g;

  my $tag = 'method';

  if ($methodName =~ s/_([gs]et)$//) {
    my $mode = $1;
    $name = "$className.$methodName";

    my $prev = $DocStringHash{$name};
    if ($prev) {
      die "Internal error: '$name' already exists.\n"
        unless $prev->[0] eq 'attrib';

      $prev->[4] = 'public' if $prev->[4] ne $mode;
      $prev->[6] = $docstring if $mode eq 'get';    # getter trumps setter
      return;
    }

    my $access = ($mode eq 'get') ? 'readable' : 'writeable';
    addDocString($name, 'attrib', $builtin, $className, $access, $methodName,
                 $docstring);
    return;
  }

  addDocString ($name, 'method', $builtin, $className, $methodName,
                $args, $docstring);
}


sub addClassDocString {
  my ($name, $builtin, $docstring) = @_;
  return unless defined($docstring);

  addDocString ($name, 'class', $builtin, $docstring);
}


# Add a macro docstring
sub addMacroDocString {
  my ($name, $builtin, $args, $docstring) = @_;
  return unless defined($docstring);

  $args =~ s/\[|\]//g;

  addDocString($name, 'macro', $builtin, $args, $docstring);
}

# Add a proc docstring
sub addProcDocString {
  my ($name, $builtin, $args, $docstring) = @_;
  return unless defined($docstring);

  $args =~ s/\[|\]//g;

  addDocString($name, 'proc', $builtin, $args, $docstring);
}

# Add a docstring for an mproc
sub addMProcDocString {
  my ($name, $builtin, $args, $docstring) = @_;
  return unless defined($docstring);

  addDocString($name, 'mproc', $builtin, $args, $docstring);
}

# Add a docstring for an mproc
sub addPackageDocString {
  my ($name, $docstring) = @_;
  return unless defined($docstring);

  # Append '::' to namespaces to differentiate between namespace and
  # variable name
  $name =~ s/\:\:$//;
  $name .= '::';

  addDocString($name, 'package', $docstring);
}


# Add a docstring to %DocStringHash.  The remaining arguments are
# stored as a hash.  Formatting is done later.
sub addDocString {
  my ($name, $tag, @values) = @_;

  map { die "addDocString: got ref!\n" if ref($_) } @_;
  $DocStringHash{$name} = [$tag, $name, @values];

  return;
}


# Add a copy of the docstring for $src under the name $dest.
sub cloneDocString {
  my ($src, $dest) = @_;

  my $clone = [ @{$DocStringHash{$src}} ];
  $clone->[1] = $dest;
  $clone->[-1] .= "\n\n($dest is an alias for $src)\n";

  $DocStringHash{$dest} = $clone;
}


# ---------------------------------------------------------------------------

# Ensure that the number of elements in $args is one of the numbers
# give in @counts.  If the first element of @counts (i.e. the second
# argument) is the string '-', ignore the first argument in
# $args--it's 'self' or the macro name.
sub checkNargs {
  my ($fn, $args, @counts) = @_;

  my $offset = 0;
  if ($counts[0] eq '-') {
    $offset = 1;
    shift @counts;
  }

  for my $count (@counts) {
    return if scalar @{$args} == $count + $offset;
  }

  die "Expecting @{[join (' or ', @counts)]} arguments to '$fn'; "
    . "got @{[scalar @{$args} - $offset]}.\n";
}


# Wrap a procedure with a sub that first sets up the backtrace.
sub wrapFnBt {
  my ($proc, $name, @args) = @_;

  $name = "Lang::$name"
    unless LL::Name::IsQualified($name);

  return sub {
    local $LL::Main::BT = btframe($LL::Main::BT, $name, @args);
    return $proc->(@_);
  };
}

# Declare a builtin procedure in the local scope and bind it to the
# given sub.
sub prim {
  my ($name, $args, $docstring, $procedure) = @_;

  # Assertion:
  die "Expecting a sub, got '$procedure'\n"
    unless ref($procedure) eq 'CODE';

  $procedure = wrapFnBt($procedure, $name) if ($enableBacktrace);

  $Globals->defset($name, LL::Procedure->new($procedure));

  my $longName = LL::Name::Normalize($name);
  addProcDocString($longName, 1, $args, $docstring);
}

# Make $dest reference the same thing as $src
sub alias {
  my ($src, $dest) = @_;

  # Sanity check
  die "Alias src '$src' is not qualified.\n"
    unless LL::Name::IsQualified($src);

  $dest = LL::Name::Normalize($dest);

  $Globals->defset($dest, $Globals->lookup($src));

  cloneDocString($src, $dest);
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
    my $outer = ($expr->size() == 0) ?  # Tolerate empty lists.
      $expr :
        LL::List->new([$expr]);
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
    map { $_->checkSymbol(" in 'subify'.") } @args;
    $arglist = LL::Quote->new( LL::List->new(\@args) );
  }

  return  LL::List->new([   LL::Symbol->new('_::sub'),
                            $arglist,
                            $body,
                            NIL
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

  $expr = LL::List->new([LL::Symbol->new('_::value'), $expr]);

  return LL::List->new([LL::Symbol->new('_::sub'),
                        LL::Quote->new(LL::List->new([])),
                        LL::Quote->new(LL::List->new([$expr])),
                        NIL,
                       ]);
}




sub quoteIfSym {
  my ($sym, $strict) = @_;

  $sym->checkSymbol(" in strict mproc argument.") if $strict;
  return $sym unless $sym->isSymbol();
  return LL::Quote->new($sym);
}


sub quoteIfList {
  my ($ls, $strict) = @_;

  $ls->checkList(" in mproc argument.") if $strict;
  return $ls unless $ls->isList();
  return LL::Quote->new($ls);
}


sub macro ( $$$$ ) {
  my ($name, $transformation, $args, $docstring) = @_;

  $name = LL::Name::Normalize($name);

  $transformation = wrapFnBt($transformation, $name);
  $Globals->defset($name, LL::Macro->new($transformation));

  addMacroDocString($name, 1, $args, $docstring);
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
  my ($proc, $name, $args, $body, $finalizer) = @_;
  checkNargs('proc', \@_, '-', 1, 3, 4);
  chkMacro();

  $name->checkSymbol(" in '${$proc}' arg 1");

  if (defined($body)) {
    $args = fixFormalArgs($args);
    $body->checkQtLoL(" in procedure body of '${$proc}'.");

    $finalizer = LL::Quote->new(LL::List->new([]))
      unless defined($finalizer);

    $finalizer->checkQtLoL(" in procedure final block of '${$proc}'.");
  } else {
    $args = NIL;
    $body = NIL;
    $finalizer = NIL;
  }

  return LL::List->new([LL::Symbol->new('_::proc'),
                        LL::Quote->new($name),
                        $args,
                        $body,
                        $finalizer]);
}


sub macro_mproc {
  my ($mproc, $name, $args, $body, $final) = @_;
  checkNargs('mproc', \@_, 3, 4, 5);
  chkMacro();

  $body ||= NIL;
  $final ||= NIL;

  $name->checkSymbol();
  my $qname = LL::Quote->new($name);

  return LL::List->new([LL::Symbol->new("_::mproc"),
                        $qname, $args, $body, $final]);
}


sub macro_macro {
  my ($macro, $name, $args, $body, $final) = @_;
  chkMacro();

  $name->checkSymbol(" as name of macro definition.");
  $args = fixFormalArgs($args);
  $body->checkQtLoL(" in body of macro definition.");
  $final->checkQtLoL(" in final block of macro definition.")
    if defined($final);
  $final ||= NIL;

  return LL::List->new([LL::Symbol->new('_::macro'),
                        LL::Quote->new($name),
                        $args, $body, $final]);
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
      $word->checkSymbol(" (@{[$word->printStr()]}) in var/const declaration.");
      die "Const '${$word}' declared without a value.\n" if $isConst;

      push @result, LL::Quote->new($word), NIL;
    }
  }

  return \@result;
}


# Macro to handle 'var' and 'const'
sub macro_varconst {
  my ($name, @args) = @_;
  chkMacro();

  my $isConst = (${$name} eq 'const');

  my $result = launder_varconst($isConst, @args);
  unshift @{ $result }, LL::Symbol->new($isConst ? '_::const' : '_::var');

  return LL::List->new($result);
}


# Handle the 'set' and '=' procedures.
sub macro_assign {
  my ($set, $dest, $value) = @_;
  chkMacro();

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
                 $dest->[1],    # list
                 $dest->[2],    # index
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
  my ($sub, $args, $body, $finalizer) = @_;
  checkNargs ('sub', \@_, 2, 3, 4);
  chkMacro();

  # If args are omitted, add them.
  if (scalar @_ == 2) {
    $body = $args;
    $args = LL::Quote->new(LL::List->new([]));
  }

  $finalizer = LL::Quote->new(LL::List->new([])) if (scalar @_ < 4);

  $args = fixFormalArgs($args);

  return LL::List->new([LL::Symbol->new('_::sub'),
                        $args,
                        $body,
                        $finalizer]);
}


sub macro_iffn {
  my ($if, $cond, $trueBlock, $else, $falseBlock) = @_;
  checkNargs('if', \@_, 5, 4, 3);
  chkMacro();

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
  checkNargs('while', \@_, 3);
  chkMacro();

  die "Expecting 2 arguments to 'while'; got @{[scalar @_ - 1]}\n"
    unless scalar @_ == 3;

  return LL::List->new([LL::Symbol->new('_::while'),
                        subifyOrDelay($cond),
                        subifyStrict($body)]);
}

sub macro_foreachfn {
  my ($foreach, $var, $in, $list, $body) = @_;
  checkNargs('foreach', \@_, 5);
  chkMacro();

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


# 'package' always gets consumed by 'readfile'.
sub macro_packagefn {
  die "Attempted to declare a package inside an existing package.\n";
}


# The 'use' macro.
sub macro_usefn {
  my ($use, $pkg, $with, $moduleList) = @_;
  chkMacro();

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
  chkMacro();

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
  chkMacro();

  die "Expecting 1 arguments to perluse; got @{[scalar @_]}\n"
    unless scalar @_ == 2;

  $name->checkSymbol(" in 'perluse'");
  return LL::List->new([LL::Symbol->new('_::perluse'),
                        LL::Quote->new($name)]);

}


sub macro_class {
  my ($class, $name, $superclass, $body) = @_;
  checkNargs('class', \@_, 3, 4);
  chkMacro();

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

# The -> operator
sub macro_methodLookupOp {
  my ($arrow, $object, $method) = @_;
  chkMacro();

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
  chkMacro();

  $field->checkSymbol(" on the RHS of a '.' operation.");

  my $method = LL::Symbol->new("${$field}_get");
  my $lookupExpr = macro_methodLookupOp('-> ignored', $object, $method);

  return LL::List->new([$lookupExpr]);
}


# Logical AND (&&).  This expands into an _::if statement, which works
# because _:if returns the result of the last expression evaluated.
sub macro_logand {
  my ($or, $left, $right) = @_;
  chkMacro();

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
  chkMacro();

  return LL::List->new([LL::Symbol->new('_::if'),
                        delayed($left),
                        NIL,
                        delayed($right)]);
}


sub macro_suboper {
  my ($arrow, $left, $right) = @_;
  chkMacro();

  die "LHS of '=>' operator was created from an auto-infix expression.\n"
    if ($left->isList() && $left->size() > 0 &&
        $left->[0]->isAutoInfixOperator());

  $left = fixFormalArgs($left);
  $right->checkQtLoL(" in RHS of ${$arrow} operator.");

  return LL::List->new([LL::Symbol->new('_::sub'),
                        $left,
                        $right,
                        NIL]);
}


# Implement the '-' operator.  This is tricky because '-' can be both
# unary and binary.
sub macro_minus {
  my ($minus, $left, $maybeRight) = @_;
  die "Expecting 1 or 2 arguments for macro '-', got @{[scalar @_]}\n"
    unless (scalar @_ == 2 || scalar @_ == 3);
  chkMacro();

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

  $methods = btWrapMethods($name, $methods) if $enableBacktrace;

  my $class = LL::Class->new([], '', $methods, $sc, $name eq 'Struct', 1, $name);
  $Globals->defset($name, $class);
  LL::Class->registerBuiltin($name, $class);

  return $class;
}


sub btWrapMethods {
  my ($classname, $methods) = @_;

  my $result = {};
  while (my ($key, $value) = each %{$methods}) {
    $result->{$key} = wrapFnBt($value, "$classname.$key");
  }

  return $result;
}


# Given an operator and a method name, create a macro with the
# operator's name that calls the method on the LHS with the RHS as
# argument.
sub op_method ($$) {
  my ($operator, $method) = @_;

  macro $operator, sub {
    my ($name, $left, $right) = @_;
    checkNargs($operator, \@_, 3);

    my $lookup = LL::List->new([LL::Symbol->new('->'),
                                $left,
                                LL::Symbol->new($method)]);

    return LL::List->new([$lookup, $right]);
  }, "left right", "Expands to C<[left-E<gt>$method right]>.";
}




# ---------------------------------------------------------------------------
sub initGlobals {
  my ($args) = @_;

  # Create default namespaces
  for my $ns (qw{_ __ Main Lang Sys}) {
    $Globals->defNamespace($ns);
  }

  # And add docstrings for '_' and '__'
  addPackageDocString("_", "
This namespace is reserved for internal objects.  B<Do not> use it or
anything defined in it--later versions could change them and break
your program.

Some of the objects in this namespace are documented.  This is for the benefit
of the curious and Deck developers only.");
  addPackageDocString("__", "
This namespace is reserved for procs created by C<mproc>.  B<Do not> use it or
anything in it.");

  # All unqualified names defined here go to Lang.
  local $LL::Name::Namespace = 'Lang';  # New compiler

  # Set the version number
  $Globals->defsetconst('Lang::Version', LL::String->new(LL::Main::VERSION));

  # Set the initial load path
  $Globals->defset('Sys::ModPath', mkModPath());

  # Set the script path and arguments
  {
    my @deckArgs = map { LL::String->new($_) } @{$args};

    $Globals->defset('Sys::Argv0', shift @deckArgs);
    $Globals->defset('Sys::Argv', LL::List->new(\@deckArgs));
  }

  $Globals->defsetconst ('nil', NIL);

  # Externally-defined primitive procedures
  for my $special (
                   ['_::puts',          "args",
                    "Identical to C<_::say> except that it also prints a" .
                    " newline.",
                    \&builtin_puts],

                   ['_::say',           "args",
                    "Displays a printable representation of each of its" .
                    " arguments to stdout.  The string representations of" .
                    " the arguments are generated internally, B<not> with" .
                    " the C<printable> attribute so C<_::say> (and C<_::puts>)".
                    " will work on objects with a defective C<printable_get>.",
                    \&builtin_say],

                   ['_::proc',          "name args body final",
                    "Defines a proc described by the arguments.  Should only".
                    " be invoked by the C<proc> macro.",
                    \&builtin_proc],

                   ['_::sub',           "args body final",
                    "Defines a sub.",
                    \&builtin_subfn],

                   ['_::if',            "test trueSub falseSub",
                    "Performs an 'if' comparison.  This is the backend of" .
                    " a number of flow-control macros.",
                    \&builtin_iffn],

                   ['_::while',         "test body",
                    "The back-end for a number of looping macros.",
                    \&builtin_whilefn],

                   ['_::set',           "name value",
                    "Assigns a value to a variable.",
                    \&builtin_set],

                   ['_::var',           "args",
                    "This is a symbol that the compiler expands into a var
                     declaration.  Calling it is a fatal error.",
                    \&builtin_var],

                   ['_::const',         "args",
                    "This is a symbol that the compiler expands into a const
                     declaration.  Calling it is a fatal error.",
                    \&builtin_const],

                   ['atput',            "seq index value",
                    "Store a value in a sequence.",
                    \&builtin_atput],

                   ['_::map',           "func seq",
                    "Primitive implementation of 'map'.",
                    \&builtin_mapfn],

                   ['_::foreach',       "list fn",
                    "Backend for the 'for'/'foreach' macro.",
                    \&builtin_foreachfn],

                   ['_::macro',         "name args body",
                    "Defines a macro.",
                    \&builtin_macro],

                   ['_::mproc',         "name args body",
                    "Define an mproc.",
                    \&builtin_mproc],

                   ['mkstr',            "args",
                    "Return a string containing all of the arguments'" .
                    " B<internal> string representation (i.e. from the" .
                    " interpreter, B<not> the C<printable> attribute).  This" .
                    " is used to expand double-quoted strings.",
                    \&builtin_mkstr],

                   ['_::mkstr_all',     "argList",
                    "Like C<_::mkstr> but takes a single list of objects" .
                    " instead of multiple arguments.",
                    \&builtin_mkstr_all],

                   ['_::use',           "module withTerm list",
                    "Implements the C<use> statement.",
                    \&builtin_usefn],

                   ['_::perlproc',      "name args bodyString",
                    "Define a proc written in Perl with source code given in" .
                    " C<bodyString>. Implements C<perlproc>.",
                    \&builtin_perlproc],

                   ['_::perluse',       "module",
                    "Implement the C<perluse> statement.",
                    \&builtin_perluse],

                   ['apply',            "fun argList",
                    "Call procedure C<fun> with the arguments in list" .
                    " C<argList> and returns the result of the call." .
                    " C<argList> B<must> be a standard Deck list and not" .
                    " merely an object that implements the sequence protocol.",
                    \&builtin_apply],

                   ['intern',           "aString",
                    "Given string C<aString>, return the symbol containing" .
                    " the same text.",
                    \&builtin_intern],

                   ['unintern',         "aSymbol",
                    "Given symbol C<aSymbol>, return a string containing the" .
                    " same text.",
                    \&builtin_unintern],

                   ['_::class',         "superclass body name",
                    "Create the class specified by C<superclass> and C<body>.
                     C<name> (a string) is not required to be the global
                     const that typically references the class but it
                     should be.  Implements macro C<class>.",
                    \&builtin_class],

                   ['_::class_ext',     "class body",
                    "Adds docstrings and methods to built-in classes.",
                    \&builtin_class_ext],

                   ['getMethod',        "object methodName",
                    "Return the C<MethodCall> object that invokes the
                     method named by symbol C<methodName> in object
                     C<object>.  If there is no matching method, the
                     C<MethodCall> invokes C<doesNotUnderstand>.

                     This is almost never called explicitly.  Usually, calls
                     are generated by the macro C<-E<gt>>.",
                    \&builtin_getMethod],

                   ['getSuperMethod',"object methodName",
                    "This is identical to C<getMethod> except that the" .
                    " method search starts at the superclass of C<object>." .
                    "\n\n" .
                    "It is usually invoked by the macro C<-E<gt>> and is not" .
                    " guaranteed to work unless C<object> is the caller's" .
                    " C<self>.",
                    \&builtin_getSuperMethod],

                   ['new',              "class args",
                    "Create and return a new instance of the class given by" .
                    " C<class>.  The remaining arguments are passed to the " .
                    " new object's C<init> method.  This is identical to" .
                    " invoking the class's C<new> method.",
                    \&builtin_new],

                   ['defined',          "name",
                    "Test if there is a global variable named C<name>
                    (a symbol) visible in the current namespace.",
                    \&builtin_definedfn],

                   ['lookup',           "name",
                    "Get the value of the variable named by symbol C<name>
                     in the current context.  If C<name> is undefined, it
                     is a fatal error.  C<name> must be a global variable.",
                    \&builtin_lookup],

                   ['_::docstring_keys',"filter package",
                    "Return a list containing the keys (i.e. names) of all
                     documented objects in memory.  The list is sorted
                     using Perl's C<cmp> operator.  Keys are symbols and
                     generally correspond to global variable names. C<filter>
                     (a Symbol) is one of the docstring types (see
                     C<_::docstring_get> for a list) and C<package> (a Symbol)
                     is the name of the package to which results are restricted.
                     Both arguments may be nil.",
                    \&builtin_docstring_keys],

                   ['_::docstring_get', "key",
                    "Return a list containing the docstring information for
                     the object named by symbol C<key>.  C<key> is typically
                     a global variable.

                     The result is a list in one of the following forms:

                     =over

                     [:class I<name> I<builtin> I<superclass> I<docstring>]

                     [:proc I<name> I<builtin> I<args> I<docstring>]

                     [:method I<name> I<builtin> I<classname> I<methodname>
                         I<args> I<docstring>]

                     [:attrib I<name> I<builtin> I<classname>
                         :readable/:writeable/:public I<attrib-name>
                         I<docstring>]

                     [:macro I<name> I<builtin> I<args> I<docstring>]

                     [:mproc I<name> I<builtin> I<args> I<docstring>]

                     [:package I<name> I<docstring>]

                     =back

                     The first field is always a symbol and identifies the
                     type of object being referenced and, by extension, the
                     format of the rest of the list.

                     The second item is its name.  The third (I<builtin>) is
                     a boolean that indicates whether the object is built into
                     the interpreter.

                     I<args> is the formal argument list and I<docstring> is
                     a string containing the long-form POD-formatted
                     documentation for the object.

                     I<superclass> and I<classname> are references to classes
                     that may be associated with the object.  Note that not
                     all classes are named or are named correctly.  However,
                     this generally only happens to classes that do not contain
                     docstrings.

                     Attributes also have the I<attrib-name> field, which is
                     the name of the attribute, as opposed to the get/set
                     method which implements it.  In addition, its fifth
                     item indicates its access mode.",
                    \&builtin_docstring_get],


                   ['subify',           "exprList args",
                    "From the given arguments, create a list which will, when
                     evaluated as an expression, return a sub.  It is intended
                     to aid in writing macros and provides standard argument
                     handling for many system macros already.

                     C<exprList> is the body of the sub and must be a
                     list or LoL.  If it is not a LoL, it is
                     automatically wrapped with another list, making
                     it into a LoL.

                     C<args> can be a list of symbols or a number.  If it is
                     a list of symbols, those symbols form the sub's formal
                     argument list.  If it is a number, the argument list is
                     generated with that many arguments named C<a> through
                     C<z>.  Naturally, C<args> must be between 0 and 26.

                     Note that C<subify> does no syntax checking at all.  If
                     you provide garbage input, you'll get garbage output.",
                    \&builtin_subify],

                   ['subifyOrDelay',    'expr',
                    "Create a list which, when evaluated as an expression, will
                     return a sub that evaluates C<expr>.  If C<expr> is a
                     list, C<subifyOrDelay> will behave exactly like C<subify>.
                     Otherwise, the sub will evaluate and return C<expr>,
                     whatever it is.",
                    \&builtin_subifyOrDelay],
                  ) {
    # Sanity assertion:
    die "Missing field in '@{$special}'\n"
      unless scalar @{$special} == 4;

    prim (@{$special});
  }

  # Set up common aliases
  for my $alias (qw{getMethod getSuperMethod mkstr atput}) {
    alias ("Lang::$alias", "_::$alias");
  }

  # More complex primitive procedures
  prim ('===',
        "lhs rhs",
        "Test if C<lhs> and C<rhs> are the same object.",
        sub { checkNargs('===', \@_, 2);
              return boolObj($_[0] == $_[1])});

  prim ('list',
        "args",
        "Return a new list containing all of the arguments in the order" .
        " they were given.",
        sub { return LL::List->new(\@_) });

  prim ('byteArray',
        "args",
        "Return a ByteArray containing the arguments.  Arguments must" .
        " be integers with values between 0 and 255.",
        sub { return LL::ByteArray->new(@_) } );

  prim ('bytesSized',
        "size",
        "Create an empty (zero-initialized) ByteArray that is C<size>" .
        " bytes long.",
        sub { my ($size) = @_;  checkNargs('bytesSized',\@_,1);
              $size->checkNumber(" as 'byteSized' argument.");
              die "Invalid byteArray size: ${$size}\n"
                unless ${$size} >= 0;
              return LL::ByteArray->newSized(${$size})} );

  prim ('stringSized',
        "size",
        "Create an empty string C<size> characters long.",
        sub { my ($size) = @_; checkNargs('stringSized',\@_,1);
              $size->checkNumber();
              die "Invalid string size: ${$size}\n"
                unless ${$size} >= 0;
              return LL::String->new("\0" x ${$size})} );

  prim ('quote',
        "obj",
        "Return a Quote object wrapping C<obj>.

         Note: this does B<not> in any way affect the evaluation of
         of its argument.  C<quote> is B<not> a special form.  If you
         want to delay normal evaluation of an expression, use the quote
         operator (C<:>).",
        sub { my ($obj) = @_; checkNargs('quote',\@_,1);
              return LL::Quote->new($obj)} );

  prim ('perlobj',
        'obj',
        "Return a PerlObj wrapping C<obj>.",
        sub { my ($obj) = @_; checkNargs('perlobj', \@_, 1);
              return LL::PerlObj->new($obj);} );

  prim ('die',
        "args",
        "Print the arguments (using the internal printer) concatenated" .
        " together, then exit with an error status.",
        sub { die join("", map { $_->printStr() } @_) . "\n" } );

  prim ('fail',
        "args",
        "Exit with an error message and backtrace.  The error message
         consists of all of the arguments stringified and concatented
         together.",
        sub {my $msg=join("", map { $_->printStr() } @_); fail($msg);});

  prim ('listSized',
        "size",
        "Return an empty (nil-filled) list of size C<size>.",
        sub { my ($size) = @_;  checkNargs('listSized', \@_,1);
              $size->checkNumber();
              die "Invalid list size: ${$size}\n" unless ${$size} >= 0;
              return LL::List->new([(NIL) x ${$size}])}  );

  prim ('_::defns',
        "namespaceName",
        "Define a new namespace named by symbol C<namespaceName>.",
        sub { my ($ns) = @_;  checkNargs('_::defns', \@_, 1);
              $ns->checkSymbol();
              $Globals->defNamespace(${$ns}) } );

  prim ('not',
        "obj",
        "Return the boolean complement of C<obj>.",
        sub { my ($arg) = @_; checkNargs('not', \@_, 1);
              return boolObj(!$arg->isTrue())} );

  prim ('int',
        "aNumber",
        "Return number C<aNumber> as an integer.  C<aNumber> is truncated" .
        " toward zero.",
        sub { my ($arg) = @_; checkNargs('int', \@_, 1);
              $arg->checkNumber(" in 'int'");
              return LL::Number->new(int(${$arg}))  } );

  prim ('neg',
        "aNumber",
        "Return number C<aNumber> negated (i.e. with its sign flipped,".
        " so negative if it was positive and positive if it was negative.)",
        sub { my ($l) = @_; checkNargs('neg', \@_, 1);
              $l->checkNumber(" in 'Lang::neg'");
              return LL::Number->new(-${$l}) } );

  prim ('str2num',
        "aString",
        "Parse C<aString> as the printable form of a numeric constant.  All" .
        " of the forms accepted by the Deck parser are also accepted by" .
        " C<str2num>.  If C<aString> does not contain a valid number," .
        " C<str2num> returns nil.",
        sub { my ($str) = @_; checkNargs('str2num', \@_, 1);
              $str->checkString(" in 'str2num'");
              my ($ns, $num) = readNumber(${$str});
              return NIL unless $ns eq "";
              return decktype($num) } );

  prim ('exit',
        "status",
        "Exit immediately with exit status C<status>.  C<status> must be a" .
        " number.  If it is not an integer, it is truncated down to the" .
        " nearest integer value.",
        sub { my ($status) = @_; checkNargs('exit', \@_, 1);
              $status->checkNumber(" in 'exit'");
              exit(${$status});
              return NIL;   # not reached
            } );

  prim ('_::value',
        "args",
        "Return the last item in the argument list.  If none is given,".
        " return nil.",
        sub { return NIL unless scalar @_; return $_[-1]} );
  alias ('_::value', 'value');

  prim ('chr',
        "number",
        "Return the character represented by C<number> in the character set.
         If C<number> is not valid character in the current encoding, returns
         an empty string instead.",
        sub {my ($num) = @_; checkNargs('chr', \@_, 1);
             $num->checkNumber(" in 'chr'.");
             my $str = $ {$num} <= 0xFF ? chr(${$num}) : "";
             return LL::String->new($str)});

  # Create the hash of procedures that take the context as arg. 0.
  for my $name (qw{_::set _::var _::sub _::const}) {
    my $fn = $Globals->lookup($name);
    $fnNeedsContext{$fn} = 1;
  }

  # Macros
  macro 'var',          \&macro_varconst,       'args',
    "Declares one or more variables in the local scope.";
  macro 'const',        \&macro_varconst,       'args',
    "Declares one or more constants in the local scope.";
  macro 'proc',         \&macro_proc,           'name args body final',
    "Declares a procedure in the current module scope.  C<final> is
     optional.  If C<body> and C<args> are also omitted, the statement instead
     creates a forward declaration of the procedure.";
  macro 'mproc',        \&macro_mproc,          'name args body',
    "Declares an mproc in the current module scope.";
  macro 'set',          \&macro_assign,         'dest value',
    "Performs assignments.  Prefix alias for C<=>.";
  macro '=',            \&macro_assign,         'dest = value',
    "Assigns RHS C<value> to LHS C<value>.  C<dest> is either a bare name, a
     list access (C<@>) expression or an attribute (C<.>) expression.";
  macro 'sub',          \&macro_subfn,          'args body final',
    "Create a C<sub> (i.e. a closure) in the local scope and return it.
     C<args> is the list of arguments (in any of the acceptable formal
     argument formats) and C<body> is a LoL containing the sub's source code.
     C<args> and C<final> are both optional but if C<final> is
     present, C<args> B<must> also be present.";
  macro 'if',           \&macro_iffn,           'cond trueBlock else falseBlock',
    "Evaluate a sequence of instructions conditionally.  Evaluates C<cond> and
     if the result is true, then evaluates C<trueBlock>.  Otherwise, it
     evaluates C<falseBlock> if present (it is optional).  This is the standard
     C<if> statement.

     C<cond> can be C<subified> or C<delayed>; C<trueBlock> and C<falseBlock>
     are always subified.  C<falseBlock> is optional. C<else> is the word
     'else'; it is always optional and should be omitted if C<falseBlock> is
     also absent.";
  macro 'while',        \&macro_whilefn,        'cond block',
    "Repeatedly evaluate C<cond> followed by C<block> until the first time
     C<cond> evaluates false.  This is the standard C<while> loop.

     C<cond> can be C<subified> or C<delayed>; C<block> is always subified.";
  macro 'foreach',      \&macro_foreachfn,      'item in list body',
    "Evaluates C<body> over each element in C<list> from start to end with
     a local variable C<item> set to reference the element.  The second
     argument must be the word C<in>.  This implements the standard C<foreach>
     loop.  Argument C<body> is subified.";
  macro 'for',          \&macro_foreachfn,      'item in list body',
    "Alias for C<foreach>.";
  macro 'macro',        \&macro_macro,          'name args body',
    "Declares a macro in the current module scope.";
  macro 'package',      \&macro_packagefn,      'moduleName',
    "Declare this file to be the package named by word C<moduleName>.
     This is really a compiler directive and it is an error to use anywhere
     other than the first (non-trivial) line of a module.";
  macro 'use',          \&macro_usefn,          'moduleName mode items',
    "Import the module named by word C<moduleName> into the current namespace.
     C<items> is the optional list of items to import, ignore or rename and
     C<mode> must be one of C<with>, C<without> or C<rename>.  The last two
     arguments are optional.";
  macro 'perlproc',     \&macro_perlproc,       'name args body',
    "Define a Perl function plus bindings to Deck.  C<name> and C<args> are
     identical to C<proc> and friends but C<body> is a string constant
     containing Perl code.";
  macro 'perluse',      \&macro_perluse,        'name',
    "Force the Perl interpreter running udeck to load the Perl module named
     by C<name> via the C<require> statement.  This module must be accessed
     via C<perlproc> functions.";
  macro 'class',        \&macro_class,          'name superclass body',
    "Define a class named by word C<name>.  Word C<superclass> is the name
     of the superclass and may be ommitted, in which case C<Struct> is assumed.
     C<body> is the class body and is expected to be a LoL.";
  macro '->',           \&macro_methodLookupOp, 'object message',
    "Performs a method lookup of RHS C<message> on LHS C<object> and returns
     the matching C<MethodLookup> object.  If C<object> is the word C<super>,
     C<self> is used instead but the method search starts at the object's
     superclass.

     Note that syntactic sugar in the compiler will implicitly wrap any
     bare C<-E<gt>> expression at the start of an infix expression with round
     brackets, making it infix.  Hence, C<[foo->bar 1]> becomes
     C<[(foo->bar) 1]>.";
  macro '.',            \&macro_fieldget,       'object attribute',
    "Performs an attribute lookup.  C<object.attribute> expands to
     C<[object->attribute_get].  However, the assignment macros C<=> and
     C<set> will expand to an C<attribute_set> call if the destination of the
     assignment is a C<.> expression.

     In addition, the synactic sugar in the compiler will automatically wrap
     any C<expression . word> sequence in an infix expression with round
     brackets, making them infix.  Hence, C<[add a.val b.val]> becomes
     C<[add (a.val) (b.val)]>.";
  macro '&&',           \&macro_logand,         'left right',
    "Perform a short-circuited logical AND.  C<right> will only be evaluated
     if C<left> has evaluated to true.";
  macro '||',           \&macro_logor,          'left right',
    "Perform a short-circuited logical Or operation.  C<right> is evaluated
     only if C<left> has evaluated to false.";
  macro '=>',           \&macro_suboper,        'args body',
    "Creates a C<sub>.  C<args> and C<body> must be list constants of the
     sorts allowed by C<sub>.

     Note that syntactic sugar in the compiler will automatically wrap any
     C<=E<gt>> expression in an infix expression with round brackets.  Hence,
     C<[map {a} => {value (a*a)} l]> becomes C<[map ({a} => {value (a*a)}) l]>.";
  macro '-',            \&macro_minus,          'left maybeRight',
    "If given two arguments, expands to C<[left->op_Sub maybeRight]>
     (i.e. ordinary subtraction).  If C<maybeRight> is ommitted, expands to
     C<[neg left]> (i.e. negation).";

  # Operator-to-method mappings
  op_method '@',  'at';
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
  op_method '==', 'op_Equals';
  op_method '<<', 'op_LShift';
  op_method '>>', 'op_RShift';

  # Define the built-in classes.
  defclass 'Object', '',
    {class_get  => sub {my ($self) = @_; return $self->class()},
     isTrue_get => sub {
       my ($self) = @_; checkNargs('isTrue_get', \@_, 1);
       return $self->isTrue() ? decktype(1) : NIL;
     },

    };

  defclass 'Class',     'Object',
    {new        => sub {
       my ($self, @argv) = @_;
       return builtin_new($self, @argv);
     },
     name_get   => sub {my ($self) = @_; checkNargs('name_get', \@_, 1);
                        decktype($self->{name})},

     name_set   => sub {my ($self, $value) = @_; checkNargs('name_set', \@_,2);
                        $value->checkString (" in 'name' class attribute.");
                        $self->{name} = $value;
                        return $value},

     selectors_get => sub {
       my ($self) = @_; checkNargs('selectors_get', \@_, 1);
       my @names =
         map { LL::Symbol->new($_) }
           sort keys %{ $self->{methodCache} };
       return LL::List->new(\@names);
     },

     methods_get    => sub {
       my ($self) = @_; checkNargs('methods_get', \@_, 1);
       my @names =
         map { LL::Symbol->new($_) }
           sort keys %{ $self->{methods} };
       return LL::List->new(\@names);
     },

     superclass_get => sub {my ($self)=@_; checkNargs('superclass_get',\@_,1);
                            return $self->{superclass}
                              if defined($self->{superclass});
                            return NIL},

     can            => sub {my ($self, $msg) = @_; checkNargs('can', \@_, 2);
                            $msg->checkSymbol(" in method 'can'.");
                            return TRUE
                              if defined($self->{methodCache}->{${$msg}});
                            return NIL},
    };

  defclass 'List',          'Object',
    {at         => sub {my ($self, $index) = @_; checkNargs('at', \@_, 2);
                        return $self->at($index)},
     atPut      => sub {my ($self, $index, $value) = @_;
                        checkNargs('atPut', \@_, 3);
                        return $self->atPut($index, $value)},
     size_get   => sub {my ($self) = @_; checkNargs('size_get', \@_, 1);
                        return decktype($self->size());},

     shallowCopy=> sub {my ($self) = @_; checkNargs('shallowCopy', \@_, 1);
                        return LL::List->new ( [ @{$self} ] ); },
    };


  defclass 'Stringlike',    'Object',
    {at         => sub {my ($self, $index) = @_; checkNargs('at', \@_, 2);
                        return $self->at($index)},
     atPut      => sub {my ($self, $index, $value) = @_;
                        checkNargs('atPut', \@_, 3);
                        return $self->atPut($index, $value)},
     size_get   => sub {my ($self) = @_; checkNargs('size_get', \@_, 1);
                        return decktype($self->size());},
    };

  defclass 'String',        'Stringlike',
    {
     op_Equals  => sub {my ($self, $other) = @_; checkNargs('op_Equals',\@_,2);
                        return NIL unless ($other->isString() ||
                                           $other->isSymbol());
                        return boolObj(${$self} eq ${$other})},
     shallowCopy=> sub {my ($self) = @_; checkNargs('shallowCopy', \@_, 1);
                        return LL::String->new (${$self})},
     ord_get    => sub {my ($self) = @_; checkNargs('ord', \@_, 1);
                        return LL::Number->new(ord(${$self}));},
    };


  defclass 'Symbol',        'Stringlike', {};
  defclass 'ByteArray',     'Stringlike',
    {
     shallowCopy=> sub {my ($self) = @_; checkNargs('shallowCopy', \@_, 1);
                        my $copy = ${$self};
                        return LL::ByteArray->newContaining (\$copy)},
    };

  defclass 'Number',        'Object',
    {
     # Double-dispatched methods.  Remember, the arguments are
     # reversed, so $self is the RHS and $other is the LHS.
     addNumber  => sub {my ($self, $other) = @_; checkNargs('addNumber',\@_,2);
                        $other->checkNumber(" in addNumber");
                        return LL::Number->new(${$self} + ${$other})},

     subNumber  => sub {my ($self, $other) = @_; checkNargs('subNumber',\@_,2);
                        $other->checkNumber(" in subNumber");
                        return LL::Number->new(${$other} - ${$self})},

     modNumber  => sub {my ($self, $other) = @_; checkNargs('modNumber',\@_,2);
                        $other->checkNumber(" in modNumber");
                        die "Modulo by zero error\n" if ${$self} == 0;
                        return decktype(int(${$other}) % int(${$self}))},

     multNumber => sub {my ($self, $other) = @_;checkNargs('multNumber',\@_,2);
                        $other->checkNumber(" in multNumber");
                        return decktype(${$other} * ${$self})},

     divNumber  => sub {my ($self, $other) = @_; checkNargs('divNumber',\@_,2);
                        $other->checkNumber(" in divNumber");
                        die "Division by zero error\n" if ${$self} == 0;
                        return decktype(${$other} / ${$self})},

     divTruncNumber => sub {my ($self, $other) = @_;
                            checkNargs('divTruncNumber', \@_, 2);
                            $other->checkNumber(" in divTruncNumber");
                            die "Division by zero error\n" if ${$self} == 0;
                            return decktype(int(${$other} / ${$self}))},

     ltNumber   => sub {my ($self, $other) = @_; checkNargs('ltNumber', \@_,2);
                        $other->checkNumber(" in ltNumber");
                        return boolObj(${$other} < ${$self})},

     lteNumber  => sub {my ($self, $other) = @_; checkNargs('lteNumber',\@_,2);
                        $other->checkNumber(" in lteNumber");
                        return boolObj(${$other} <= ${$self})},

     gtNumber   => sub {my ($self, $other) = @_; checkNargs('gtNumber',\@_,2);
                        $other->checkNumber(" in gtNumber");
                        return boolObj(${$other} > ${$self})},

     gteNumber  => sub {my ($self, $other) = @_; checkNargs('gteNumber',\@_,2);
                        $other->checkNumber(" in gteNumber");
                        return boolObj(${$other} >= ${$self})},

     powNumber  => sub {my ($self, $other) = @_; checkNargs('powNumber',\@_,2);
                        $other->checkNumber(" in powNumber");
                        return decktype(${$other} ** ${$self})},

     bitOrNumber=> sub {my ($self, $other) = @_;
                        checkNargs('bitOrNumber', \@_, 2);
                        $other->checkNumber(" in bitOrNumber");
                        return decktype(int(${$other}) | int(${$self}))},

     bitXorNumber=>sub {my ($self, $other) = @_;
                        checkNargs('bitXorNumber', \@_, 2);
                        $other->checkNumber(" in bitXorNumber");
                        return decktype(int(${$other}) ^ int(${$self}))},

     bitAndNumber=>sub {my ($self, $other) = @_;
                        checkNargs('bitAndNumber', \@_, 2);
                        $other->checkNumber(" in bitAndNumber");
                        return decktype(int(${$other}) & int(${$self}))},

     shiftRight => sub {my ($self, $other) = @_;
                        checkNargs('shiftRight', \@_, 2);
                        $other->checkNumber(" in shiftRight");
                        return decktype(int(${$other}) >> int(${$self}))},

     shiftLeft  => sub {my ($self, $other) = @_;
                        checkNargs('shiftLeft', \@_, 2);
                        $other->checkNumber(" in shiftLeft");
                        return decktype(int(${$other}) << int(${$self}))},

     op_Equals  => sub {my ($self, $other) = @_; checkNargs('op_Equals',\@_,2);
                        return NIL unless $other->isNumber();
                        return boolObj(${$self} == ${$other})},



    };

  defclass 'Quote',         'Object',
  {
   value_get    => sub {my ($self) = @_; checkNargs('value_get', \@_, 1);
                        return $self->value()},
  };

  defclass 'Nil',           'Object', {};
  defclass 'Macro',         'Object', {};
  defclass 'Procedure',     'Object', {};
  defclass 'Method',        'Object', {};
  defclass 'MethodCall',    'Object', {};
  defclass 'Continuation',  'Object', {};

  defclass 'PerlObj',       'Object',
  {
   shallowCopy  => sub {my ($self) = @_; checkNargs('shallowCopy', \@_, 1);
                        return LL::PerlObj->new($self->[0])},
   printable_get=> sub {my ($self) = @_; checkNargs('printable_get', \@_, 1);
                        my $desc = "<perlobj @{[ref($self->[0])]}>";
                        return LL::String->new($desc)},
  };

  defclass 'Struct',        'Object',
  {
   _structShallowCopy
                => sub {my ($self) = @_; checkNargs('_structShallowCopy',\@_,1);
                        return $self->shallowCopy()},
  };

  # The external 'Lang' module
  if (!$noLib) {
    # Hack: disable dumping when loading the library.
    my $dbk = $dumpExpr;
    $dumpExpr = 0;

    my $path = findFileFor('Lang');
    die "Unable to find system module 'Lang.dk'.  Check your module path.\n"
      unless $path;

    readfile($path, 'Lang', 1, 0, 1);

    $dumpExpr = $dbk;
  }

  # Finally, switch to Main and import all public system names.
  $Globals->importPublic('Lang', 'Main');
}


# Create the module path.  To do: make this reasonable.
sub mkModPath {
  my @path = ();

  # The environment variable 'DECKLIB' contains the lib path.  To do:
  # handle DOS-style paths for Perl on Windows.
  my $decklib = $ENV{DECKLIB};
  if (defined($decklib)) {
    my @envpath = split(/:/, $decklib);
    @envpath = grep { -d $_ } @envpath;
    @envpath = map { abs_path($_) } @envpath;

    push @path, @envpath;
  }

  # On the development subtree, the library files will be in ./lib
  {
    my $binpath = dirname(abs_path($0)) . "/lib-deck/";
    push @path, $binpath if -d $binpath;
  }

  # And convert to Deck types.
  @path = map { LL::String->new($_) } @path;
  return LL::List->new(\@path);
}


sub builtin_puts {
  builtin_say(@_);
  print "\n";
  return NIL;
}

sub builtin_say {
  for my $obj (@_) {
    die "Not an object: '$obj'\n"
      unless (blessed($obj) && $obj->isa('LL::Object'));

    if (!blessed($obj) || !$obj->isa('LL::Object')) {
      print "[Illegal object: $obj]";
      next;
    }

    print $obj->printStr();
  }

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
  my ($context, @argPairs) = @_;
  fail "Call to '_::var'\n";
  return NIL;
}

sub builtin_const {
  my ($context, @argPairs) = @_;
  fail "Call to '_::const'\n";
  return NIL;
}


sub builtin_proc {
  my ($name, $args, $body, $finalizer) = @_;
  checkNargs('_::proc', \@_, 4);

  $name->checkSymbol();

  $Globals->defsetconst(${$name}, LL::UndefinedProcedure->new(${$name}))
    unless $Globals->present(${$name});

  if ($body->isNil()) {
    $Globals->addForward(${$name});
    return NIL;
  }

  $args->checkList();
  $body->checkLoL(" in proc body.");
  $finalizer->checkLoL(" in proc final block.");

  die "proc: name '${$name}' is already defined.\n"
    unless $Globals->lookup(${$name})->isUndefinedProcedure();

  my $longName = LL::Name::Normalize(${$name});
  my $func = compile (undef, $args, $body, $finalizer, 'proc', $longName);
  $Globals->setGlobalConst(${$name}, $func);

  return $func;
}


# Given an mproc argument list ($args), produce a matching array of
# arrays of procedures (one per arg.) which performs the necessary
# transformations on the matching argument.
sub mk_mproc_macro_argfilter {
  my ($argList, $name) = @_;
  my @filters = ();
  my $needDefault = 0;  # Indicates need/presence of default value
  my $numDefaults = 0;
  my $argNum = 0;
  for my $arg (@{$argList}) {
    my @argFilter = ();

    $arg->checkList(" in mproc argument list.");

    ++$argNum;

    # First, check for the presence of a default value
    my $default;
    my $dfExpr = $arg->[-1];

    if ($dfExpr->isList()        &&
        $dfExpr->size() == 2     &&
        $dfExpr->[0]->isSymbol() &&
        ${ $dfExpr->[0] } eq 'default') {

      $needDefault = 1;
      $default = $dfExpr->[1];

      my $msg="Illegal default value for argument $argNum of mproc '$name'.\n";
      if (!$default->isLiteral()) {
        if ($default->isSymbol() && ${$default} eq 'nil') {
          $default = NIL;

        } elsif ($default->isQtLoL()) {
          # Leave $default as is

        } elsif ($default->isQuote()) {
          my $val = $default->value();
          die $msg
            unless $val->isSymbol() || $val->isList();

        } else {
          die $msg;
        }
      }

      pop @{$arg};  # remove it from the arg. list.
    } else {
      # And if there was previously a default, then there needs to be
      # one here as well.
      die "Argument '$argNum' must have a default value.\n"
        if $needDefault;
    }

    ++$numDefaults if $needDefault;

    # Next, get the actual argument name
    my $argName = pop @{$arg};
    $argName->checkSymbol(" in mproc argument.");
    $argName = ${$argName};

    die "Invalid mproc argument name '$argName'\n"
      if $argName =~ /^(sub|sym|list|strict|word)$/;

    # Now, test for strictness
    my $strict = (scalar @{$arg} > 0 && ${$arg->[0]} eq 'strict');
    shift @{$arg} if $strict;

    # And get the modifier
    my $mod = LL::Symbol->new('');
    $mod = shift @{$arg} if scalar @{$arg};
    $mod->checkSymbol() if $mod;

    given (${$mod}) {
      when ("sub") {
        my @sfyArgs = ();

        # The argument can be either a number or list of names
        if (scalar @{$arg} == 1) {
          if ($arg->[0]->isNumber()) {
            $sfyArgs[0] = ${ shift @{$arg} };
            die "Invalid arg count in mproc: $sfyArgs[0] -- must be " .
              "between 0 and 26\n"
                if ($sfyArgs[0] < 0 || $sfyArgs[0] > 26);
          } elsif ($arg->[0]->isList()) {
            map { $_->checkSymbol(" in a 'sub' modifier arg. list.") }
              @{$arg->[0]};
            @sfyArgs = @{$arg->[0]};
          } else {
            die "Malformed 'sub' mproc argument modifier.\n";
          }
        }

        die "Malformed 'sub' mproc argument: invalid arg specifier.\n"
          unless scalar @{$arg} <= 1;

        # Ensure default type is a list.  (LoL?)
        die "Default argument is not a list in argument $argNum of " .
          "mproc '$name' despite 'sub' modifier.\n"
            if ($needDefault && !$default->isList() && !$default->isQtLoL());

        if ($strict) {
          push @argFilter, sub {subifyStrict(shift() || $default, @sfyArgs)};
        } else {
          push @argFilter, sub {subify(shift() || $default, @sfyArgs)};
        }
      }

      when ("symbol") {
        die "Default argument is not a symbol in argument $argNum of " .
          "mproc '$name' despite 'symbol' modifier.\n"
            if ($needDefault &&
                !($default->isQuote() && $default->value()->isSymbol()));

        my $uqDefault = $needDefault ? $default->value() : undef;
        push @argFilter, sub {quoteIfSym(shift() || $uqDefault, $strict)};
      }

      when ("list") {
        die "Default argument is not a list in argument $argNum of " .
          "mproc '$name' despite 'list' modifier.\n"
            if ($needDefault && (!$default->isQuote() ||
                                 !$default->value()->isList() ));

        push @argFilter, sub {quoteIfList(shift() || $default, $strict)};
      }

      when ("word") {
        my $udef = undef;

        if ($needDefault) {
          $udef = $default->isQuote() ? $default->value() : $default;
          die "Default argument is not the symbol '$argName' of ".
            "mproc '$name' despite 'word' modifier.\n"
              if (!$udef->isSymbol() || ${$udef} ne $argName);
        }

        push @argFilter,
          sub {my ($param) = @_;
               $param ||= $udef;

               $param->checkSymbol(" in mproc argument '$argName' " .
                                   "for '$name'.");
               die "Expecting word '$argName', got '${$param}'\n"
                 unless ${$param} eq $argName;

               return quoteIfSym($param, 1);
             };
      }

      when ('') {
        if ($needDefault) {
          push @argFilter, sub {return shift() || $default}
        } else {
          push @argFilter, sub {return shift()};
        }
      }

      default {
        die "Invalid mproc argument '${$mod}'\n" if $mod ne '';
      }
    }

    push @filters, \@argFilter;
  }

  return (\@filters, $numDefaults);
}


# Return a macro (i.e. blessed Perl sub) which performs mproc argument
# munging on its argument list.  $args is dismantled.
sub mk_mproc_macro {
  my ($procName, $args, $name) = @_;

  my $resultName = LL::Symbol->new($procName);

  my ($filters, $defaults) = mk_mproc_macro_argfilter($args, $name);

  my $macro = sub {
    my ($givenName, @args) = @_;
    my @newArgs = ();
    die "Arg count mismatch in call to mproc '${$givenName}'\n"
      if (scalar @args > scalar @{$filters} ||
          scalar @args < scalar @{$filters} - $defaults);

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


# Return a human-friendly one-line string describing a set of mproc
# arguments.
sub readableMProcArgs {
  my ($args) = @_;

  my $result = "";
  my $first = 1;

  for my $item (@{$args}) {
    $result .= "; " unless $first;
    $first = 0;

    # Create a scratch copy that we can tear apart if necessary.
    $item->checkList(" in mproc argument.");
    my @args = @{$item};

    # Handle the 'strict' if present now so that we can guarantee that
    # the first word determines the argument type.
    if ($args[0]->printStr() eq 'strict') {
      $result .= 'strict ';
      shift @args;
    }

    # 'word' always has a default, which is redundant.  We remove it
    # here.
    if ($args[0]->printStr() eq 'word') {
      @args = @args[0..1];
    }

    # And construct the rest of the description
    my $subFirst = 1;
    for my $subitem (@args) {
      $result .= " " unless $subFirst;
      $subFirst = 0;

      $result .= $subitem->printStr();
    }
  }

  # Final fix-up.
  $result =~ s/\:\[\]/{}/g;

  $result = "{$result}";
  return $result;
}


# Define an mproc or just its forward declaration.
sub builtin_mproc {
  my ($name, $args, $body, $final) = @_;

  # Argument checking.
  checkNargs('_::mproc', \@_, 2, 3, 4);

  $name->checkSymbol();
  $args->checkList (" for mproc argument list.");
  $body->checkLoL (" as mproc body.") unless $body->isNil();
  $final = LL::List->new([]) if $final->isNil();
  $final->checkLoL (" for mproc final block.");;

  $Globals->ensureMacroNamespace(${$name});
  my $fullName = LL::Name::Normalize(${$name});
  my $procName = "__::$fullName";
  my $argSig = $args->printStr();

  # Add the docstring if present
  {
    my $argDesc = readableMProcArgs($args);
    my $docstring = $body->isNil ? undef : $body->stripDocString();
    addMProcDocString($fullName, 0, $argDesc, $docstring) if $docstring;
  }

  # Gather the argument names from $args before mk_mproc_macro
  # disassembles it.  (mk_mproc_macro does a lot of sanity checking so
  # we don't need to do it here.)
  my @pargs = ();
  for my $elem (@{$args}) {
    $elem->checkList(" in mproc argument.");

    my $argName = $elem->[-1];
    $argName = $elem->[-2] unless $argName->isSymbol(); # skip [default xxx]

    push @pargs, $argName;
  }

  my $forward = $Globals->getForward($fullName);

  # Ensure that this macro hasn't already been defined.
  die "Redefinition of macro '$fullName'.\n"
    if $Globals->present($fullName) && !$forward;

  if (!$forward || $body->isNil()) {
    # Create the wrapper macro.
    my $macro = mk_mproc_macro($procName, $args, $fullName);

    # Give the macro a name.
    $Globals->defsetconst($fullName, $macro);

    # Set the placeholder procedure
    $Globals->defsetconst($procName, LL::UndefinedProcedure->new($fullName));

    # And set the forward declaration
    $Globals->addForward($fullName, $argSig);

    $forward = $argSig;
  }

  # If this was just a forward declaration, we're done
  return if $body->isNil();

  die "Argument mismatch with forward declaration in mproc '$fullName'\n"
    unless $forward eq $argSig;

  # Create the procedure to call.
  my $procArgs = LL::List->new(\@pargs);
  my $proc = builtin_proc (LL::Symbol->new($procName), $procArgs, $body,
                           $final);

  return $proc;
}



sub builtin_macro {
  my ($name, $args, $body, $final) = @_;
  checkNargs('_::macro', \@_, 4);
  $final = LL::List->new([]) if $final == NIL;

  $name->checkSymbol(" in macro name.");
  $args->checkList(" in macro '${$name}' argument list.");
  $body->checkLoL(" in macro '${$name}' body.");
  $final->checkLoL(" in macro '${$name}' final block.");

  my $docstring = $body->stripDocString();
  if ($docstring) {
    my $mostArgs = LL::List->new([ @{$args}[1..$#{$args}] ]);
    addMacroDocString(${$name}, 0, $mostArgs->printStr(), $docstring);
  }

  my $longName = LL::Name::Normalize(${$name});
  my $macro = compile (undef, $args, $body, $final, 'macro', $longName);

  $Globals->defset(${$name}, $macro);

  return $macro;
}



sub builtin_subfn {
  my ($context, $args, $body, $finalizer) = @_;

  fail ("'_::sub' cannot be called like an ordinary function.");
}


# Perform the 'if' operation.  Return the result of the last closure
# evaluated.  The second and third closure (the 'true' and 'else' parts)
# may be NIL in which case they are skipped.
sub builtin_iffn {
  my ($test, $trueBlock, $falseBlock) = @_;
  checkNargs("_::if", \@_, 3);

  my $result = $test->();
  if ($result->isTrue()) {
    $result = $trueBlock->() if $trueBlock->isProcedure();
  } elsif ($falseBlock->isProcedure()) {
    $result = $falseBlock->();
  }

  $result;
}

sub builtin_whilefn {
  my ($test, $body) = @_;

  checkNargs('_::while', \@_, 2);

  my $result = NIL;
  while ($test->()->isTrue()) {
    $result = $body->();
  }

  return $body;
}


sub builtin_foreachfn {
  my ($list, $fn) = @_;

  checkNargs('_:::foreach', \@_, 2);

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

  checkNargs('_::mkstr_all', \@_, 1);

  $args = LL::List->new([$args]) unless $args->isList();

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

  checkNargs('_::use', \@_, 3);

  $moduleName->checkSymbol();

  # Import the file
  my $mn = ${$moduleName};
  my $path = findFileFor($mn);
  die "Unable to find module '$mn'\n"
    unless $path;

  if (!$Globals->hasNamespace($mn)) {
    readfile($path, $mn, 1, 0);
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
    when ('with')   {$withSet = $names}
    when ('without'){$withoutSet = $names}
    when ('rename') {$renameSet = $names}
    when ('') {}
    default {die "Invalid 'use' modifier clause: '${$with}'\n"}
  }

  $Globals->importPublic($mn, $LL::Name::Namespace, $withSet,
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

  checkNargs('_::perlproc', \@_, 3);

  $args->checkList();
  $name->checkSymbol();
  $bodyStr->checkString();


  my $perlArgs = "";

  if ($args->size() > 0) {
    my $isArgv = ${ $args->[-1] } eq 'args';
    pop @{$args} if $isArgv;

    $perlArgs .= 'my (';
    for my $a (@{$args}) {
      $a->checkSymbol(" in perlproc argument.");
      $perlArgs .= '$' . ${$a} . ',';
    }

    $perlArgs .= '@args' if $isArgv;

    $perlArgs .= ') = map { $_ && $_->perlForm() } @_;' . "\n";
  }

  my $fn = 'sub{' . $perlArgs . ${$bodyStr} . '}';

  my $sub = strEval($fn);
  die "Error: $@\nCompiling perlsub:\n'''\n$fn\n'''\n"
    if ($@ || ref($sub) ne 'CODE');

  my $proc = sub {return decktype($sub->(@_))};
  return $Globals->defset(${$name}, LL::Procedure->new($proc));
}



sub builtin_perluse {
  my ($moduleSym) = @_;

  checkNargs('_::perluse', \@_, 1);

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

  checkNargs('apply', \@_, 2);

  die "Expecting 2 args, got @{[scalar @_]}\n"
    unless scalar @_ == 2;

  $fun->checkFun(" in 'apply'");
  $args->checkList(" in 'apply'");

  return $fun->(@{$args});
}

sub builtin_intern {
  my ($string) = @_;

  checkNargs('intern', \@_, 1);
  $string->checkString(" in 'intern'");

  return LL::Symbol->new(${$string});
}


sub builtin_unintern {
  my ($symbol) = @_;

  checkNargs('unintern', \@_, 1);
  $symbol->checkSymbol(" in 'unintern'");

  return LL::String->new(${$symbol});
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
    next if scalar @{$entry} == 0;      # Maybe too tolerant

    my $start = fieldType($entry);
    next if ($start eq 'method' || $start eq 'mdoc');
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
  my ($name, $args, $fields, $prefix, $body, $finalizer) = @_;

  my $fieldsContext;

  $fieldsContext = LL::CompileState->new('a class', 0, undef, $prefix, 0, 1);
  for my $nm (keys %{$fields}) {$fieldsContext->addRef($nm, -1, 0, 1)}

  my $code = compile ($fieldsContext, $args, $body, $finalizer,
                      METHOD, ${$name});
  return LL::Method->new($code);
}


# Search $body for method definitions (with fields defined in
# $fields), compile them and return a dictionary mapping name to
# method.  If $methodsOnly is true, do not allow non-method fields in
# $body.  If $allowDoc is true, *do* allow mdoc statements.
sub class_methods {
  my ($body, $fields, $prefix, $methodsOnly, $className, $allowDoc) = @_;

  my %methods = ();
  for my $entry (@{$body}) {
    next if scalar @{$entry} == 0;      # Maybe too tolerant

    my $start = $entry->[0];
    $start->checkSymbol();

    if (${$start} ne 'method') {
      die "Expecting 'method' declaration; got '${$start}'.\n"
        if $methodsOnly && (${$start} eq 'mdoc' && !$allowDoc);
      next;
    }

    die "Malformed method declaration: '@{[$entry->printStr()]}'.\n"
      unless scalar @{$entry} == 4 || scalar @{$entry} == 5;

    my ($method, $name, $args, $body, $finalizer) = @{$entry};
    $name->checkSymbol(" in method name.");
    $args = fixFormalArgs($args)->value();

    $body->checkQtLoL(" in method definition.");

    $finalizer = LL::Quote->new(LL::List->new([]))
      unless $finalizer;
    $finalizer->checkQtLoL(" in method final block definition.");

    # There's no more eval so drop the quotes
    $body = $body->value();
    $finalizer = $finalizer->value();

    # Extract the docstring (if any)
    if ($className) {
      my $ds = $body->stripDocString();
      addMethodDocString("$className->${$name}", 0, $args->printStr(), $ds)
        if $ds;
    }


    $methods{${$name}} = mk_method($name, $args, $fields, $prefix,
                                   $body, $finalizer);
  }

  return \%methods;
}


# Extract 'mdoc' statements from _class_ext declarations.
sub class_ext_docs {
  my ($class, $body) = @_;

  my %methods = ();
  for my $entry (@{$body}) {
    next if scalar @{$entry} == 0;      # Maybe too tolerant

    my $start = $entry->[0];
    $start->checkSymbol();

    next if (${$start} ne 'mdoc');

    die "Malformed method declaration: '@{[$entry->printStr()]}'.\n"
      unless scalar @{$entry} == 4;

    my ($mdoc, $name, $args, $docstring) = @{$entry};
    $name->checkSymbol(" in method name.");
    $args = fixFormalArgs($args)->value();
    $docstring->checkString (" in 'mdoc' statement.");

    # Ensure that $name is a method in the class.
    die "No method named '${$name}' for matching mdoc.\n"
      unless defined($class->{methods}->{${$name}});

    # Set the docstring
    my $className = LL::Name::Normalize($class->{name});
    die "No classname for _class_ext.\n" unless $className;

    addMethodDocString("$className->${$name}", 0, $args->printStr(),
                       ${$docstring});
  }

  return;
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

  } else {  # $type eq 'get'
    $args = LL::List->new([]);
  }

  # We always need to return the value
  push @{$body}, LL::List->new([LL::Symbol->new('return'),
                                LL::Symbol->new($field)]);

  return ($args, $body);
}



# Add the attributes to $methods
sub class_attributes {
  my ($attribNames, $body, $fields, $prefix, $methods) = @_;

  for my $attrib (@{$attribNames}) {

    # Don't redefine existing method
    next if defined($methods->{$attrib});

    my $attribSym = LL::Symbol->new($attrib);
    my ($args, $body) = attrib_parts($attrib);
    my $final = LL::List->new([]);
    $methods->{$attrib} = mk_method($attribSym, $args, $fields, $prefix,
                                    $body, $final);
  }
}


my $mangle = 0;
sub builtin_class {
  my ($superclass, $body, $name) = @_;

  $superclass->checkClass(" in superclass for class declaration.");
  $name->checkString(" in _::class name argument.");
  $body->checkLoL(" in class body.");

  my $prefix = '__class_'.$mangle++;

  # For now, we disable subclassing structured classes.  I need to
  # figure out what to do with them.
  die "Attempted to subclass a non-structured class '$superclass->{name}'.\n"
    unless $superclass->isStructuredClass();

  my $fullNameStr = LL::Name::Normalize(${$name});
  {
    my $docstring = $body->stripDocString();
    addClassDocString($fullNameStr, 0, $docstring)
      if ($fullNameStr && $docstring);
  }

  my ($fields, $attribNames) = class_fields($body);
  die "Attempted to create fields in non-struct class.\n"
    if (scalar keys %{$fields} > 0 &&
        !$superclass->isStructuredClass());

  my $methods = class_methods($body, $fields, $prefix, 0, $fullNameStr, 0);
  class_attributes ($attribNames, $body, $fields, $prefix, $methods);

  my $class = LL::Class->new([keys %{$fields}], $prefix, $methods, $superclass, 1,
                             0, ${$name});

  return $class;
}


sub builtin_class_ext {
  my ($class, $body) = @_;

  $class->checkClass();
  $body->checkLoL(" in _class_ext.");
  my $name = $class->{name};

  {
    my $docstring = $body->stripDocString();
    $name = LL::Name::Normalize($name) if $name;
    addClassDocString($name, 1, $docstring) if ($name && $docstring);
  }

  my $methods = class_methods($body, {}, '', 1, $name, 1);
  $class->addMethods ($methods);

  # Add the mdoc statements to the set of docstrings.
  class_ext_docs ($class, $body);

  LL::Class->refreshAllBuiltinClassMethodCaches();

  return $class;
}


sub builtin_new {
  my ($class, @args) = @_;

  $class->checkClass(" in procedure 'new'.");

  die "'new' only works on Struct-derived classes.\n"
    unless $class->isStructuredClass();

  my $obj = LL::Struct->new($class);

  my $init = $class->{methodCache}->{_init};    # avoid lookup() to skip dnu
  if ($init) {
    $init->($obj, @args);
  }

  return $obj;
}




sub basicLookup {
  my ($name) = @_;

  $name->checkSymbol(" in 'defined' or 'lookup'");

  my $nm = ${$name};

  return unless LL::Name::IsLegallyAccessible($nm);

  $nm = LL::Name::Normalize(${$name});
  return unless exists($LL::Main::Globals->{$nm});

  return $LL::Main::Globals->{$nm};
}


sub builtin_definedfn {
  my ($name) = @_;
  checkNargs('defined', \@_, 1);

  return boolObj(!! basicLookup($name));
}

sub builtin_lookup {
  my ($name) = @_;
  checkNargs('lookup', \@_, 1);

  my $val = basicLookup($name);
  fail "Undefined symbol '${$name}'\n" unless $val;

  return $val;
}

sub builtin_docstring_keys {
  my ($tag, $package) = @_;
  checkNargs ('_::docstring_keys', \@_, 2);

  my @keys = keys %DocStringHash;

  if (!$tag->isNil() ) {
    $tag->checkSymbol(" in _::docstring_keys");
    $tag = ${$tag};

    fail "Invalid tag type: '$tag'\n"
      unless $tag =~ /^(class|proc|method|attrib|macro|mproc|package)$/;

    @keys = grep { $DocStringHash{$_}->[0] eq $tag } @keys;
#    @keys = map { s/\:+$//; $_ } @keys;    # Field
  }

  if (!$package->isNil()) {
    $package->checkSymbol(" in _::docstring_keys");
    my $ns = ${$package};
    $ns =~ s/::$//;

    @keys = grep { LL::Name::IsMemberOf($DocStringHash{$_}->[1], $ns) } @keys;
  }

  @keys = sort { $a cmp $b } @keys;

  @keys = map { LL::Symbol->new($_) } @keys;
  return LL::List->new(\@keys);}


sub builtin_docstring_get {
  my ($key) = @_;

  fail ("Expecting Symbol or String, got @{[ref($key)]}\n")
    unless ($key->isSymbol() || $key->isString());

  my $ds = $DocStringHash{${$key}};
  return NIL unless defined($ds);

  my $type = LL::Symbol->new($ds->[0]);

  my $result;
  given ($ds->[0]) {
    when (/^m?proc$/) {
      $result = [$type,
                 LL::String->new($ds->[1]),
                 boolObj($ds->[2]),
                 map { LL::String->new($_) } @{$ds}[3..4] ];
    }

    when ('class') {
      $result = [$type,
                 LL::String->new($ds->[1]),
                 boolObj($ds->[2]),
                 LL::String->new($ds->[3]),
                ];
    }

    when ('method') {
      $result = [$type,
                 LL::String->new($ds->[1]),
                 boolObj($ds->[2]),
                 map { LL::String->new($_) } @{$ds}[3..$#{$ds}]
                ];
    }

    when ('attrib') {
      $result = [$type,                     # Tag
                 LL::String->new($ds->[1]), # Name
                 boolObj($ds->[2]),         # Builtin
                 LL::String->new($ds->[3]), # Class name
                 LL::Symbol->new($ds->[4]), # access mode
                 LL::String->new($ds->[5]), # attrib name
                 LL::String->new($ds->[6]), # docstring
                ];
    }

    when ('macro') {
      $result = [$type,                     # Tag
                 LL::String->new($ds->[1]), # Name
                 boolObj($ds->[2]),         # Builtin,
                 LL::String->new($ds->[3]), # Arguments
                 LL::String->new($ds->[4])  # Docstring
                ];
    }

    when ('package') {
      $result = [$type,                     # Tag
                 LL::String->new($ds->[1]), # Name
                 LL::String->new($ds->[2])  # Docstring
                ];
    }

    default {die "Corrupt docstring entry for '${$key}'\n"}
  }

  return LL::List->new($result);
}


sub builtin_subify {
  my ($expr, $args) = @_;

  my @args = ();

  if ($args->isNumber()) {
    push @args, ${$args};
  } else {
    $args->checkList();
    for my $elem (@{$args}) {
      $elem->checkSymbol(" in 'subify'.");
      push @args, $elem;
    }
  }

  my $result = subify($expr, @args);
  return $result if $result != $expr;
  return $expr;
}


sub builtin_subifyOrDelay {
  my ($expr) = @_;

  return subifyOrDelay($expr);
}



