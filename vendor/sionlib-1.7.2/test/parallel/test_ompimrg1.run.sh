#!/bin/bash

#set -x

# defaults
MPIRUN="mpirun"
MPIRUNEXECPARM=""

# include definitions
. ./mpirun.defs

TESTNAME=ompimrg1
rm -rf run_test_${TESTNAME}
if (! [ -d run_test_${TESTNAME} ] ) 
then
    mkdir run_test_${TESTNAME}
fi

SIONDUMP=../siondump

(

  cd run_test_${TESTNAME}
  
   
  export SION_DEBUG_MASK=4095
#  export SION_DEBUG_RANK1=0
#  export SION_DEBUG_RANK2=3
  export SION_ERROR_MSG_RANK=0
  export SION_DEBUG_SILENT=1
  export SION_COLLDEBUG=3
  
  # TEST 1

  export SION_DEBUG=test1_${TESTNAME}
  export OMP_NUM_THREADS=4
  export SION_COLLSIZE=2
  
  if [ "${COVERAGE}" = "kcov" ] && type kcov 2>&1 > /dev/null
  then
      ${MPIRUN} sh -c "kcov --include-pattern=sion_ kcov_output_\$\$ ../test_${TESTNAME}" ${MPIRUNEXECPARM} 1> stdout.log 2> stderr.log
      kcov --merge kcov_output kcov_output_* 2>&1 > /dev/null
  else
      ${MPIRUN} ../test_${TESTNAME} ${MPIRUNEXECPARM} 1> stdout.log 2> stderr.log
  fi
  
  grep 'on MPI rank 0, OMP thread 0:' stdout.log | grep -v 'sid           =' > stdout_0.log
  grep 'on MPI rank 0, OMP thread 0:' stderr.log > stderr_0.log

  export SION_DEBUG=siondump1_${TESTNAME}

  ${SIONDUMP} -a -m -l testB.out 1>> stdout_siondump.log 2>> stderr_siondump.log

  cat stdout_siondump.log | grep -v 'endianness' | grep -v 'SIONlib version' | grep -v 'Writing debug output' >> stdout_0.log
  cat stderr_siondump.log | grep -v 'Writing debug output' >> stdout_0.log


  # TEST 2

  export SION_DEBUG=test2_${TESTNAME}
  export OMP_NUM_THREADS=2
  export SION_COLLSIZE=4

  ${MPIRUN} ../test_${TESTNAME}  ${MPIRUNEXECPARM}  1> stdout_2.log 2> stderr_2.log
  
  grep 'on MPI rank 0, OMP thread 0:' stdout_2.log | grep -v 'sid           =' >> stdout_0.log
  grep 'on MPI rank 0, OMP thread 0:' stderr_2.log >> stderr_0.log

  export SION_DEBUG=siondump2_${TESTNAME}
  ${SIONDUMP} -a -m -l testB.out 1>> stdout_siondump_2.log 2>> stderr_siondump_2.log

  cat stdout_siondump_2.log | grep -v 'endianness' | grep -v 'SIONlib version' | grep -v 'Writing debug output' >> stdout_0.log
  cat stderr_siondump_2.log | grep -v 'Writing debug output' >> stdout_0.log


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
