#!/bin/sh

COMPVER="?.?"

case ${COMP} in
  pgi)  COMPVER=`pgcc -V 2>&1 | grep "pgcc" | cut -d\  -f 2`
       ;;
  intel) COMPVER=`icc -V 2>&1 | grep ^Intel | sed -e 's/^.*Version //' \
         | cut -d\  -f 1`
       ;;
  path) COMPVER=`pathcc -v 2>&1 | grep ^PathScale | sed -e 's/^.*Version //'`
       ;;
  ibm) CVER=`xlc -qversion 2>&1 | grep "IBM" | sed -e 's/^.*V//' \
         | cut -d\  -f 1`
       FVER=`xlf -qversion 2>&1 | grep "IBM" | sed -e 's/^.*V//' \
         | cut -d\  -f 1`
       COMPVER="${CVER}/${FVER}"
       ;;
  cce) COMPVER=`craycc -V 2>&1 | grep ^Cray | cut -d\  -f 5`
       ;;
  sun) CVER=`suncc  -V 2>&1 | grep 'Sun' | sed -e 's/^.*Sun C //' \
         | cut -d\  -f 1`
       FVER=`sunf90 -V 2>&1 | grep 'Sun' | sed -e 's/^.*Sun Fortran 95 //' \
         | cut -d\  -f 1`
       COMPVER="${CVER}/${FVER}"
       ;;
  gnu) COMPVER=`gcc -dumpfullversion -dumpversion`
       #COMPVER=`gcc -v 2>&1 | grep -i 'Version [0-9]' \
       #  | sed -e 's/^.*ersion \(.\..\..\).*$/\1/'`
       ;;
  *)   ErrorAndExit "Unknown compiler '${COMP}'"
       ;;
esac
