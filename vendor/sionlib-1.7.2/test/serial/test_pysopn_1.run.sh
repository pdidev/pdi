#!/bin/bash

#set -x

TESTNAME=pysopn_1

SIONPYPATH=../../../pythoninterface

export PYTHONPATH=$SIONPYPATH:$PYTHONPATH

PYTHONVERSION=3
PYCMD=python${PYTHONVERSION}

TESTCMD="$PYCMD ../test_${TESTNAME}.py"

if (! [ -d run_test_${TESTNAME} ] ) 
then
    mkdir run_test_${TESTNAME}
fi

(

  cd run_test_${TESTNAME}
  
  export SION_DEBUG=test_${TESTNAME}
  export SION_DEBUG_MASK=1023
  export SION_DEBUG_RANK1=0
  export SION_DEBUG_RANK2=1
  export SION_ERROR_MSG_RANK=0

  $TESTCMD 1> stdout.log 2> stderr_t.log

  grep -v 'Writing debug output' stderr_t.log > stderr.log
  
  if diff stderr.log ../test_${TESTNAME}_stderr.log > /dev/null
  then
      if diff stdout.log ../test_${TESTNAME}_stdout.log > /dev/null
      then
	  echo "     test_${TESTNAME}              ... OK"
      else
	  echo "     test_${TESTNAME}              ... FAILED (diff in stdout)"
	  diff -u stdout.log ../test_${TESTNAME}_stdout.log
      fi
  else
      echo "     test_${TESTNAME}              ... FAILED (diff in stderr)"
      diff -u stderr.log ../test_${TESTNAME}_stderr.log
  fi

)
