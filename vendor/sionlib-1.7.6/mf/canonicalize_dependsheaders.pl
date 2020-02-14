#!/usr/bin/perl -w
#****************************************************************************
#*  Scalasca Makefile dependency canonicalization tool                     **
#****************************************************************************
#*  Copyright (c) 2010-2019                                                **
#*  Forschungszentrum Juelich GmbH, Juelich Supercomputing Centre          **
#*                                                                         **
#*  Contact:                                                               **
#*      Markus Geimer <m.geimer@fz-juelich.de>                             **
#****************************************************************************


use strict;


#--- Global variables -------------------------------------------------------

my $target;              # Current target(s)
my $depend;              # Dependencies of current target(s)
my $alldepend;           # Dependencies of all target(s)
my %rules;               # Hash of rules $target -> $depend


#--- Functions --------------------------------------------------------------

# Discards all but one of identical elements from the array being passed
# in. The remaining entries are returned.
sub uniq {
  my $item;
  my @uniq;
  my %seen = ();

  foreach $item (@_) {
    push(@uniq, $item) unless $seen{$item}++;
  }

  return @uniq;
}


#--- Main program -----------------------------------------------------------

# Process GCC dependency output (read from stdin)
$alldepend = "";
$depend = "";
while (<STDIN>) {
  # Remove trailing newline
  chomp;

  # Concatenate multi-line dependencies
  if (s/\\$//) {
    $depend .= $_;
    next;
  }
  $depend .= $_;

  #--- Now we have a one dependency in a single line ---

  # Extract target
  $target = $depend;
  $target =~ s/([^:]+:)(.*)/$1/;

  # Extract dependencies
  $depend =~ s/([^:]+:)(.*)/$2/;

  # Replace multiple whitespaces by a single space
  $depend =~ s/\s+/ /g;

  $alldepend .= $depend;

  # Store rule
  $rules{$target} = $depend;
  $depend = "";
}


  # Sort dependencies alphabetically, remove duplicates
foreach (uniq(sort(split(/ /, $alldepend)))) {
    print $_,"\n";
}

exit 0
