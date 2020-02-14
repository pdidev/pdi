#!/bin/bash

#set -x

# defaults
MPIRUN="mpirun"
MPIRUNEXECPARM=""

# include definitions
. ./mpirun.defs

TESTNAME=smplef_1

if (! [ -d run_test_${TESTNAME} ] ) 
then
    mkdir run_test_${TESTNAME}
fi

SIONDUMP=../siondump
(

  cd run_test_${TESTNAME}
  
  export SION_DEBUG=test_${TESTNAME}
  export SION_DEBUG_MASK=2047
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


  ${SIONDUMP} -a -l test_sionfile.sion 1>> siondump_stdout.log 2>> siondump_stderr.log

  grep 'on rank      0' stdout.log | grep -v 'resources: utime' > stdout_0.log 
  grep 'on rank      0' stderr.log | grep -v 'COMM_WORLD selected' | grep -v 'Writing debug output' | grep -v "ORTE_ERROR_LOG: File read failure in file util/universe_setup_file_io.c" > stderr_0.log 

  grep 'siondump:' siondump_stdout.log | grep -v 'endianness' | grep -v 'SIONlib version' >> stdout_0.log
  grep 'Task ' siondump_stdout.log >> stdout_0.log


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

)
