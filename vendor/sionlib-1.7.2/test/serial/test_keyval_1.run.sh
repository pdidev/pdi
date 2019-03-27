#!/bin/bash

#set -x

TESTNAME=keyval_1

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

  if [ "${COVERAGE}" = "kcov" ] && type kcov 2>&1 > /dev/null
  then
      kcov --include-pattern=sion_ kcov_output ../test_${TESTNAME} 1> stdout.log 2> stderr_t.log
  else
      ../test_${TESTNAME} 1> stdout.log 2> stderr_t.log
  fi

  grep -v 'Writing debug output' stderr_t.log > stderr.log

  cat keysA_rank0000_in.log keysA_rank0001_in.log keysA_rank0002_in.log keysA_rank0003_in.log | sort -n -k 5  > keysA_in.log 
  cat keysA_rank0000_out.log keysA_rank0001_out.log keysA_rank0002_out.log keysA_rank0003_out.log | sort -n -k 5 > keysA_out.log 

  cat keysC_rank0000_in.log keysC_rank0001_in.log keysC_rank0002_in.log keysC_rank0003_in.log | sort -n -k 5  > keysC_in.log 
  cat keysC_rank0000_out.log keysC_rank0001_out.log keysC_rank0002_out.log keysC_rank0003_out.log | sort -n -k 5 > keysC_out.log 


  if diff keysC_in.log keysC_out.log > /dev/null
  then
      if diff keysA_in.log keysA_out.log > /dev/null
      then
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
      else
	  echo "     test_${TESTNAME}              ... FAILED (diff in keylistA)"
	  diff -u keysA_in.log keysA_out.log
      fi
  else
      echo "     test_${TESTNAME}              ... FAILED (diff in keylistC)"
      diff -u keysC_in.log keysC_out.log
  fi


)
