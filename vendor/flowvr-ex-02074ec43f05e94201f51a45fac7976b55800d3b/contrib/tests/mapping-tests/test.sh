#! /bin/sh

# TO run test
USAGE="test  [testname]  [csvfile (use -L for localhost)] [example] -gen"

gen=0;
case $# in
	0)	echo $USAGE
		exit 2 ;;
	1)	echo $USAGE
		exit 2 ;;
	2)	echo $USAGE
		exit 2 ;;
	3)	csvfile=$2
                testname=$1 
                example=$3;;
	4)      csvfile=$2
                testname=$1
                example=$3;
                if test $4 == "-gen"; then
                    gen=1 ;
                fi ;;
esac;


. bin/primes-config.sh ;
if test $csvfile = '-L'; then 
    flowvr Primes -Pprimes:example=$example -L -p $testname 2> $testname.err;
else
    flowvr Primes -Pprimes:example=$example -H $csvfile -p $testname 2> $testname.err;
fi
if test  -s $testname.err; then
    exit 2;
fi
if test $gen = '1'; then 
    grep "host"  $testname.net.xml > $testname.hosts.net.xml ;
    grep "host"  $testname.adl.out.xml > $testname.hosts.adl.out.xml ;
else
    grep "host"  $testname.net.xml > tmp.hosts.net.xml ;
    grep "host"  $testname.adl.out.xml > tmp.hosts.adl.out.xml;
    if test  ! -r $testname.hosts.net.xml  -a ! -r  $testname.hosts.adl.out.xml ; then
        exit 2;
    fi
    diff -u   tmp.hosts.net.xml  $testname.hosts.net.xml > diff.hosts.net.xml ;
    diff -u    tmp.hosts.adl.out.xml $testname.hosts.adl.out.xml > diff.hosts.adl.out.xml ;
    if  test -s diff.hosts.net.xml || test -s diff.hosts.adl.out.xml; then
        echo "ERROR for $testname";
        exit 2;
    fi
fi
exit 0;
