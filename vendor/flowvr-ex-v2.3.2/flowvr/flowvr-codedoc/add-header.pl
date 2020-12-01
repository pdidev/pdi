#!/usr/bin/perl

$copyright_begin='/******* COPYRIGHT ************************************************'."\n";
$copyright_end  ='*******************************************************************'."\n";

#$contact="20/09/2005 Clement Menier <clement.menier@inrialpes.fr>";

open FCOPYRIGHT,$ARGV[0] or die "can\'t open copyright file $ARGV[0]";
shift @ARGV;
@copyright=<FCOPYRIGHT>;
close FCOPYRIGHT;

$fname=$ARGV[0];
shift @ARGV;

@lines=<STDIN>;
#print "lines:\n"; print @lines;

@before=();
@in=();
@after=();

$state=0;

foreach $l (@lines) {
    if (substr($l,-1,1) ne "\n") {
	$l=$l."\n";
    }

    if ($state==0) {
	if ($l eq $copyright_begin) {
	    $state=1;
	    #push(@in,$l);
	}
	else {
	    push(@before,$l);
	}
    }
    elsif ($state==1) {
	if ($l eq $copyright_end) {
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
    print @copyright;
    print "*                                                                 *\n";
    print "* File: $fname".substr("                                                          ",length($fname))."*\n";
    print "*                                                                 *\n";
    print "* Contacts: $contact".substr("                                                      ",length($contact))."*\n";
    foreach $c (@ARGV) {
	print "*  $c".substr("                                                               ",length($c))."*\n";
    }
    print "*                                                                 *\n";
    print "******************************************************************/\n";
    print @before;
}
elsif ($state==1) {
    print @before;
    print @copyright;
    print @in;
}
else {
    print @before;
    print @copyright;
    print @after;
}
