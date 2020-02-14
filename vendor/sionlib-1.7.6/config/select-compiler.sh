#!/bin/sh

# -- Uncomment for stand-alone use
#CFGecho () {
#  echo "$1"
#}

NCPS=0
CPS=""
COMP=""

GNU=`which gcc 2> /dev/null`
if [ -n "${GNU}" ]
then
  NCPS=`expr ${NCPS} + 1`
  COMP=gnu
  if [ -z "${CPS}" ]; then CPS="${COMP}"; else CPS="${CPS}|${COMP}"; fi
  CFGecho "INFO: Found ${GNU}"
fi

INTEL=`which icc 2> /dev/null`
if [ -n "${INTEL}" ]
then
  NCPS=`expr ${NCPS} + 1`
  COMP=intel
  if [ -z "${CPS}" ]; then CPS="${COMP}"; else CPS="${CPS}|${COMP}"; fi
  CFGecho "INFO: Found ${INTEL}"
fi

PATHCC=`which pathcc 2> /dev/null`
if [ -n "${PATHCC}" ]
then
  NCPS=`expr ${NCPS} + 1`
  COMP=path
  if [ -z "${CPS}" ]; then CPS="${COMP}"; else CPS="${CPS}|${COMP}"; fi
  CFGecho "INFO: Found ${PATHCC}"
fi

PGI=`which pgcc 2> /dev/null`
if [ -n "${PGI}" ]
then
  NCPS=`expr ${NCPS} + 1`
  COMP=pgi
  if [ -z "${CPS}" ]; then CPS="${COMP}"; else CPS="${CPS}|${COMP}"; fi
  CFGecho "INFO: Found ${PGI}"
fi

IBM=`which xlc 2> /dev/null`
if [ -n "${IBM}" ]
then
  NCPS=`expr ${NCPS} + 1`
  COMP=ibm
  if [ -z "${CPS}" ]; then CPS="${COMP}"; else CPS="${CPS}|${COMP}"; fi
  CFGecho "INFO: Found ${IBM}"
fi

SUN=`which suncc 2> /dev/null`
if [ -n "${SUN}" ]
then
  NCPS=`expr ${NCPS} + 1`
  COMP=sun
  if [ -z "${CPS}" ]; then CPS="${COMP}"; else CPS="${CPS}|${COMP}"; fi
  CFGecho "INFO: Found ${SUN}"
fi

if [ "${NCPS}" -eq 2 ]
then
  CFGecho "INFO: Using ${COMP} compiler; select GNU compiler using --compiler=gnu"
elif [ "${NCPS}" -gt 2 ]
then
  CFGecho ""
  CFGecho "INFO: Found ${NCPS} compilers"
  CFGecho "Select compiler using --compiler=${CPS}"
  exit 1
fi
CFGecho ""
