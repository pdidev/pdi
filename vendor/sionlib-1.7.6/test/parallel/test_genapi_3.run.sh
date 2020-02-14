#!/bin/bash

#set -x

# defaults
MPIRUN="mpirun"
MPIRUNEXECPARM=""

# include definitions
. ./mpirun.defs

TESTNAME=genapi_3


if (! [ -d run_test_${TESTNAME} ] ) 
then
    mkdir run_test_${TESTNAME}
fi

SIONDUMP=../siondump

(

  cd run_test_${TESTNAME}
  
  export SION_DEBUG=test_${TESTNAME}
  export SION_DEBUG_MASK=1023
  export SION_DEBUG_RANK1=0
  export SION_DEBUG_RANK2=1
  export SION_ERROR_MSG_RANK=0
  export SION_DEBUG_SILENT=1
  export OMP_NUM_THREADS=1

  if [ "${COVERAGE}" = "kcov" ] && type kcov 2>&1 > /dev/null
  then
      ${MPIRUN} sh -c "kcov --include-pattern=sion_ kcov_output_\$\$ ../test_${TESTNAME}" ${MPIRUNEXECPARM} 1> stdout.log 2> stderr.log
      kcov --merge kcov_output kcov_output_* 2>&1 > /dev/null
  else
      ${MPIRUN} ../test_${TESTNAME} ${MPIRUNEXECPARM} 1> stdout.log 2> stderr.log
  fi

  grep 'on rank 0' stdout.log > stdout_0.log
  grep 'on rank 0' stderr.log > stderr_0.log

  cat keys_rank0000_in.log keys_rank0001_in.log keys_rank0002_in.log keys_rank0003_in.log | sort -n -k 5 > keys_in.log 
  cat keys_rank0000_out.log keys_rank0001_out.log keys_rank0002_out.log keys_rank0003_out.log | sort -n -k 5 > keys_out.log 

  if diff keys_in.log keys_out.log > /dev/null
  then
      if diff stderr_0.log ../test_${TESTNAME}_stderr_0.log > /dev/null
      then
	  if diff stdout_0.log ../test_${TESTNAME}_stdout_0.log > /dev/null
	  then
	      echo "     test_${TESTNAME}              ... OK"
	  else
	      echo "     test_${TESTNAME}              ... FAILED (diff in stdout)"
	      diff -u stdout_0.log ../test_${TESTNAME}_stdout_0.log
	  fi
      else
	  echo "     test_${TESTNAME}              ... FAILED (diff in stderr)"
	  diff -u stderr_0.log ../test_${TESTNAME}_stderr_0.log
      fi
  else
      echo "     test_${TESTNAME}              ... FAILED (diff in keylist)"
      diff -u keys_in.log keys_out.log
  fi

)
