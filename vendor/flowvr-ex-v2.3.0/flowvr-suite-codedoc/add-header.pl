#!/usr/bin/perl

if ($#ARGV != 1 &&  $#ARGV != 2)
  {
    print "Usage:perl add-header.pl copyrightfile  dir [contact]
       Recursively change the header of all .h, .ccp  and .cxx in dir using content of copyrightfile
       Add contact as contact person if no copyright header present in the file\n";
    exit;
  }


$copyright_begin='/******* COPYRIGHT ************************************************';
$copyright_end  ='*****************************************************************';


open FCOPYRIGHT,$ARGV[0] or die "can\'t open copyright file $ARGV[0]";
shift @ARGV;
@copyright=<FCOPYRIGHT>;
close FCOPYRIGHT;

$dir=$ARGV[0]; 
shift @ARGV;

if ($#ARGV == 1) 
  {
    $contact=$ARGV[0];
  }


use File::Find;
find(\&wanted, $dir);

sub wanted
  {
#    print "call wanted on $_";
    $sourcefile = $_;
    $fullpathsourcefile = $File::Find::name;
    if ( $sourcefile =~ /.+\.(h|cpp|c|cxx)$/ )
      {
	print " Update header  of:  $fullpathsourcefile\n";
	
	open MYFILE,$sourcefile or die "can\'t open file $fullpathsourcefile\n";
	
	@lines = <MYFILE>;
	close MYFILE;
	
	# overwrite the file

	open MYFILE,">".$sourcefile or die "can\'t open file $fullpathsourcefile\n";


	@before=();
	@in=();
	@after=();
	
	$state=0;

	foreach $l (@lines) {
	  if (substr($l,-1,1) ne "\n") {
	    $l=$l."\n";
	  }
	  
	  if ($state==0) {
	    if ($l =~ /(\s*\Q$copyright_begin\E)$/ ) {
	      $state=1;
	      #push(@in,$l);
	    }
	    else {
	      push(@before,$l);
	    }
	  }
	  elsif ($state==1) {
	    if ($l =~ /(\s*\Q$copyright_end\E)$/ ) {
	      $state=2;

	    }
	    else {
	      push(@in,$l);
	    }
	  }
	  else {
	    push(@after,$l);
	  }
	}
	
	if ($state==0) {
	  print MYFILE  @copyright;
	  print MYFILE "*                                                                 *\n";
	  print MYFILE "* File: $fullpathsourcefile".substr("                                                          ",length($fullpathsourcefile))."*\n";
	  print MYFILE "*                                                                 *\n";
	  print MYFILE "* Contacts: $contact".substr("                                                      ",length($contact))."*\n";
	  print MYFILE "*                                                                 *\n";
	  print MYFILE "******************************************************************/\n";
	  print MYFILE @before;
	}
	elsif ($state==1) {
	  print MYFILE @before;
	  print MYFILE @copyright;
	  print MYFILE @in;
	}
	else {
	  print MYFILE @before;
	  print MYFILE @copyright;
	  print MYFILE @after;
	}

	close MYFILE;
	
      }
  }
  
