#!/usr/bin/perl

# Perl version of factor.dk for speed comparison

use strict;
use warnings;


sub isInt {
  my ($n) = @_;
  return $n == int($n);
}

sub idb {
  my ($value, $divisor) = @_;

  return isInt($value/$divisor);
}

sub firstFactorOf {
  my ($num) = @_;

  for my $factor (2 .. int ($num / 2) + 1) {
    if (idb($num, $factor)) {
      return $factor;
    }
  }

  return $num;
}

sub allFactors {
  my ($num) = @_;

  if ($num < 4) {
    return [1, $num];
  }

  my $result = [1];

  while ($num > 1) {
    my $ff = firstFactorOf($num);
    push @{$result}, $ff;
    $num /= $ff;
  }

  return $result;
}


sub factor {
  my ($num) = @_;

  for my $f (@{ allFactors($num) }) {
    print $f, " ";
  }
  print "\n";
}


sub main {
  if (scalar @ARGV != 1) {
    die "Usage: factor.pl <integer>\n";
  }

  my $num = $ARGV[0] + 0;

  if ($num < 1) {
    die "Argument must be greater than 0.n";
  }

  if (!isInt($num)) {
    die "Argument '$num' is not an integer.\n";
  }

  if ($num == 1) {
    print "1";
    exit (0);
  }

  if ($num < 4) {
    print "1 $num\n";
    exit(0);
  }

  factor($num);

}


main();
