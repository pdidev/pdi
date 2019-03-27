#!/bin/sh
# determine what compiler is behind "cc" (and presumably "CC" and "ftn") wrapper

CC=`which cc 2>/dev/null`
COMP=
if [ -z "$CC" ]; then
  echo "No 'cc' located!"
  exit
fi

case "$CC" in
  /opt/cray/craype/* | /opt/cray/xt*)
      echo "$CC is probably a Cray XT compiler wrapper" ;;
  *)
      echo "Warning: $CC doesn't seem to be a Cray XT compiler wrapper!" ;;
esac

if [ `$CC -V 2>&1 | grep -c ^Cray` -gt 0 ]; then
  COMP=cce
  COMPVER=`$CC -V 2>&1 | grep ^Cray | cut -d\  -f 5`
  echo "Got CCE ${COMPVER} compiler"
elif [ `$CC -V 2>&1 | grep -c ^Intel` -gt 0 ]; then
  COMP=intel
  COMPVER=`$CC -V 2>&1 | grep ^Intel | sed -e 's/^.*Version //' | cut -d\  -f 1`
  echo "Got Intel ${COMPVER} compiler"
elif [ `$CC -V 2>&1 | grep -c ^pg` -gt 0 ]; then
  COMP=pgi
  COMPVER=`$CC -V 2>&1 | grep ^pg | cut -d\  -f 2`
  echo "Got PGI ${COMPVER} compiler"
elif [ `$CC -v dummy.c 2>&1 | grep -c ^PathScale` -gt 0 ]; then
  COMP=pathscale
  COMPVER=`$CC -v dummy.c 2>&1 | grep ^PathScale | sed -e 's/^.*Version //'`
  echo "Got PathScale $COMPVER compiler"
# NB: since PathScale includes GCC, test for GCC last
elif [ `$CC -v dummy.c 2>&1 | grep -c ^gcc` -gt 0 ]; then
  COMP=gnu
  COMPVER=`$CC -dumpversion`
  echo "Got GNU $COMPVER compiler"
else
  echo "'$CC' compiler not identified!"
fi
