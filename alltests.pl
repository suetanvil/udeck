#!/usr/bin/perl

# Run all Deck regression tests and compare their results with
# expected results.

use strict;
use warnings;

use File::Basename;
use File::Temp qq{tempfile};


{
  chdir("tests") or die "Unable to chdir to 'tests'\n";

  my $upto = shift || "";
  $upto = basename($upto) if $upto;
  die "No test named '$upto'\n" unless !$upto || -f $upto;

  my @tests = glob('test*.dk');
  my @failures = glob('fail*.dk');
  my @sh = glob('*.sh');

  for my $test (@tests, @sh, @failures) {
	if ($upto) {
	  next unless $upto eq $test;
	  $upto = "";
	}

	if ($upto) {
	  print "Skipping $test\n";
	  next;
	}
	
	runtest($test);
  }
}

sub runtest {
  my ($file) = @_;

  doRun($file);
  doRun($file, 1) if $file =~ /^fail/;
}

sub doRun {
  my ($file, $bt) = @_;

  my $cmd = '../udeck.pl --flush' . ($bt ? " --backtrace" : "");
  $cmd = "bash" if $file =~ /\.sh$/;

  # Test if we're expecting failure
  my $fail = ($cmd =~ /^fail/);

  # Compute the result file name
  my $resultFileName = $file;
  $resultFileName =~ s/\.(dk|sh)$//;
  $resultFileName .= ".bt" if $bt;
  $resultFileName .= ".txt";

  # And skip if the file is missing
  if (! -f $resultFileName) {
	print "Skipping $file: No results file.\n" unless $bt;
	return;
  }

  print "$file", ($bt) ? " (backtrace)" : "", "\n";

  # Run the test and exit if it did not exit in the expected way
  my $result = `$cmd $file 2>&1`;
  if ($? >> 16) {
	die "$file failed unexpectedly.\n" unless $fail;
  } else {
	die "$file succeeded unexpectedly.\n" if $fail;
  }

  # Compare the results.  If different, run diff on them
  my $expected = slurp($resultFileName);

  return if $expected eq $result;

  my ($fh, $filename) = tempfile();
  print {$fh} $result;
  close ($fh);

  print `diff $filename $resultFileName`;
  unlink($filename);
  exit(1);

}

sub slurp {
  my ($filename) = @_;
  local $/ = undef;

  open my $fh, "<$filename"
	or die "Unable to open '$filename' for reading.\n";
  my $result = <$fh>;
  close ($fh);

  return $result;
}
