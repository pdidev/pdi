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

#  print STDERR "$target $depend\n";

  # remove dependency to mpi.h
  $depend =~ s/\/.*\/mpi.h//gs;

  # remove dependency to openmpi
  $depend =~ s/\/usr\/lib\/mpi\/[^\s]*\s//gs;

  # remove dependency to openmpi
  $depend =~ s/\/usr\/lib64\/mpi\/[^\s]*\s//gs;

  # Replace multiple whitespaces by a single space
  $depend =~ s/\s+/ /g;


  # Sort dependencies alphabetically, remove duplicates
  $depend = join(" \\\n\t", uniq(sort(split(/ /, $depend))));


  # Store rule
  $rules{$target} = $depend;
  $depend = "";

}


# Generate canonicalized output
foreach (sort(keys(%rules))) {
  # Print rule
  print "$_$rules{$_}\n";
}
exit 0
