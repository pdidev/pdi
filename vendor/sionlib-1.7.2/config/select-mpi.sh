#!/bin/sh

# -- Uncomment for stand-alone use
#CFGecho () {
#  echo "$1"
#}

NMPIS=0
MPIS=""
MPI=""

MPIICC=`which mpiicc 2> /dev/null`
if [ -n "${MPIICC}" ]
then
  R_MPIICC=`readlink -f ${MPIICC}`
  if [ -n "${R_MPIICC}" ] ; then MPICC=${R_MPIICC}; fi
  MBINDIR=`dirname ${MPIICC}`
  MINCDIR=`echo ${MBINDIR} | sed -e 's/bin/include/'`
  if [ -f ${MINCDIR}/mpi.h ]
  then
    IMPIVER=`grep ^# ${MINCDIR}/mpi.h 2> /dev/null | \
             grep MPI_VERSION | head -1 | awk '{print $NF}'`
    if [ -z "${IMPIVER}" ]; then IMPIVER=-42; fi
    if [ ${IMPIVER} -eq 1 ]
    then
      NMPIS=`expr ${NMPIS} + 1`
      MPI=intel
      CFGecho "INFO: Found Intel MPI 1 ${MPIICC}"
    elif [ ${IMPIVER} -eq 2 ]
    then
      NMPIS=`expr ${NMPIS} + 1`
      MPI=intel2
      CFGecho "INFO: Found Intel MPI 2 ${MPIICC}"
    elif [ ${IMPIVER} -eq 3 ]
    then
      NMPIS=`expr ${NMPIS} + 1`
      MPI=intel3
      CFGecho "INFO: Found Intel MPI 3 ${MPIICC}"
    else
      CFGecho "ERROR: Cannot determine Intel MPI version"
      CFGecho "Select MPI using --mpi=intel|intel2|intel3"
      exit 1
    fi
  fi
  if [ -z "${MPIS}" ]; then MPIS="${MPI}"; else MPIS="${MPIS}|${MPI}"; fi
fi

MPCC=`which mpcc 2> /dev/null`
if [ -n "${MPCC}" ]
then
  R_MPCC=`readlink -f ${MPCC}`
  if [ -n "${R_MPCC}" ] ; then MPICC=${R_MPCC}; fi
  MBINDIR=`dirname ${MPCC}`
  if [ `uname -m` = "x86_64" ]
  then
    NMPIS=`expr ${NMPIS} + 1`
    MPI=intelpoe
    CFGecho "INFO: Found Intel POE ${MBINDIR}"
  else
    NMPIS=`expr ${NMPIS} + 1`
    MPI=ibmpoe
    CFGecho "INFO: Found IBM POE ${MBINDIR}"
  fi
  if [ -z "${MPIS}" ]; then MPIS="${MPI}"; else MPIS="${MPIS}|${MPI}"; fi
fi

MPIRC=`which rail-config 2> /dev/null`
if [ -f /etc/sgi-release -a -n "${MPIRC}" ]
then
  R_MPIRC=`readlink -f ${MPIRC}`
  if [ -n "${R_MPIRC}" ] ; then MPICC=${R_MPIRC}; fi
  MBINDIR=`dirname ${MPIRC}`
  NMPIS=`expr ${NMPIS} + 1`
  MPI=sgimpt
  CFGecho "INFO: Found SGI MPT ${MBINDIR}"
  if [ -z "${MPIS}" ]; then MPIS="${MPI}"; else MPIS="${MPIS}|${MPI}"; fi
fi

MPICC=`which mpicc 2> /dev/null`
if [ -n "${MPICC}" ]
then
  FMPI=""
  R_MPICC=`readlink -f ${MPICC}`
  if [ -n "${R_MPICC}" ] ; then MPICC=${R_MPICC}; fi
  MBINDIR=`dirname ${MPICC}`
  MPIROOTDIR1=`dirname ${MBINDIR}`

  echo "#include <mpi.h>" > conftest.c
  mpicc -E conftest.c | grep '/mpi.h"' | head -1 > mpiconf.txt
  MINCDIR=`cat mpiconf.txt | sed -e 's#^.* "##' -e 's#/mpi.h".*##'`
  if [ -n "${MINCDIR}" ]
  then
    MPIROOTDIR2=`dirname ${MINCDIR}`
    R_MPIROOTDIR2=`readlink -f ${MPIROOTDIR2}`
    if [ -n "${R_MPIROOTDIR2}" ] ; then MPIROOTDIR2=${R_MPIROOTDIR2}; fi
    rm -f conftest.c mpiconf.txt
    if [ "${MPIROOTDIR1}" = "${MPIROOTDIR2}" ]
    then
      MPIROOTDIR2=""
    fi
  fi

  for mr in ${MPIROOTDIR1} ${MPIROOTDIR2}
  do
    MLIBDIR="${mr}/lib"
    MLIB64DIR="${mr}/lib64"
    MBINDIR="${mr}/bin"
    if [ -f ${MLIBDIR}/libmpich.a -o -f ${MLIBDIR}/libmpich.so -o \
         -f ${MLIB64DIR}/libmpich.a -o -f ${MLIB64DIR}/libmpich.so ]
    then
      if [ -f ${MBINDIR}/mpibull2-version ]
      then
        NMPIS=`expr ${NMPIS} + 1`
        FMPI=mpibull2
        CFGecho "INFO: Found Bull MPICH2 ${MPICC}"
      elif [ -f ${MBINDIR}/mpichversion ]
      then
        MPICHVER=`grep ^# ${MINCDIR}/mpi.h 2> /dev/null | \
                  grep MPI_VERSION | awk '{print $NF}'`
        if [ -z "${MPICHVER}" ]; then MPICHVER=-42; fi
        if [ ${MPICHVER} -eq 3 ]
	then
          NMPIS=`expr ${NMPIS} + 1`
          FMPI=mpich3
          CFGecho "INFO: Found MPICH3 ${MPICC}"
	else
          NMPIS=`expr ${NMPIS} + 1`
          FMPI=mpich
          CFGecho "INFO: Found MPICH1 ${MPICC}"
	fi
      elif [ -f ${MBINDIR}/mpich2version ]
      then
        NMPIS=`expr ${NMPIS} + 1`
        FMPI=mpich2
        CFGecho "INFO: Found MPICH2 ${MPICC}"
      elif [ ! -f ${MBINDIR}/mpiicc ]
      then
        if [ -f ${MLIBDIR}/libmpich.a ]
        then
          ML=${MLIBDIR}/libmpich.a
        elif [ -f ${MLIBDIR}/libmpich.so ]
        then
          ML=${MLIBDIR}/libmpich.so
        elif [ -f ${MLIB64DIR}/libmpich.a ]
        then
          ML=${MLIB64DIR}/libmpich.a
        elif [ -f ${MLIB64DIR}/libmpich.so ]
        then
          ML=${MLIB64DIR}/libmpich.so
        else
          CFGecho "ERROR: Cannot determine MPICH version"
          exit 1
        fi
        if nm ${ML} | grep -q MPI_Win
        then
          NMPIS=`expr ${NMPIS} + 1`
          FMPI=mpich2
          CFGecho "INFO: Found MPICH2 ${MPICC}"
        else
          NMPIS=`expr ${NMPIS} + 1`
          FMPI=mpich
          CFGecho "INFO: Found MPICH1 ${MPICC}"
        fi
      fi
    elif [ -f ${MLIBDIR}/liblam.a -o -f ${MLIBDIR}/liblam.so -o \
           -f ${MLIB64DIR}/liblam.a -o -f ${MLIB64DIR}/liblam.so ]
    then
      NMPIS=`expr ${NMPIS} + 1`
      FMPI=lam
      CFGecho "INFO: Found LAM ${MPICC}"
    elif [ -d ${MLIBDIR}/openmpi -a \( -f ${MLIBDIR}/libmpi.a -o -f ${MLIBDIR}/libmpi.so \) -o \
           -d ${MLIB64DIR}/openmpi -a \( -f ${MLIB64DIR}/libmpi.a -o -f ${MLIB64DIR}/libmpi.so \) ]
    then
      NMPIS=`expr ${NMPIS} + 1`
      FMPI=openmpi
      CFGecho "INFO: Found Open MPI ${MPICC}"
    elif [ -d ${MLIBDIR}/bullxmpi -a \( -f ${MLIBDIR}/libmpi.a -o -f ${MLIBDIR}/libmpi.so \) -o \
           -d ${MLIB64DIR}/bullxmpi -a \( -f ${MLIB64DIR}/libmpi.a -o -f ${MLIB64DIR}/libmpi.so \) ]
    then
      NMPIS=`expr ${NMPIS} + 1`
      FMPI=bullxmpi
      CFGecho "INFO: Found bullx MPI ${MPICC}"
    elif [ -f ${MLIBDIR}/linux_ia32/libpcmpi.a -o -f ${MLIBDIR}/linux_ia32/libpcmpi.so \
         -o -f ${MLIBDIR}/linux_amd64/libpcmpi.a -o -f ${MLIBDIR}/linux_amd64/libpcmpi.so ]
    then
      NMPIS=`expr ${NMPIS} + 1`
      FMPI=platform
      CFGecho "INFO: Found Platform Computing MPI ${MPICC}"
    elif [ -f ${MLIBDIR}/linux_ia32/libhpmpi.a -o -f ${MLIBDIR}/linux_ia32/libhpmpi.so \
         -o -f ${MLIBDIR}/linux_amd64/libhpmpi.a -o -f ${MLIBDIR}/linux_amd64/libhpmpi.so \
         -o -f ${MLIBDIR}/linux_ia64/libhpmpi.a -o -f ${MLIBDIR}/linux_ia64/libhpmpi.so ]
    then
      NMPIS=`expr ${NMPIS} + 1`
      FMPI=hp
      CFGecho "INFO: Found HP MPI ${MPICC}"
    elif [ -f ${MBINDIR}/mpimon -a \( -f ${MLIBDIR}/libmpi.a -o -f ${MLIBDIR}/libmpi.so -o \
           -f ${MLIB64DIR}/libmpi.a -o -f ${MLIB64DIR}/libmpi.so \) ]
    then
      NMPIS=`expr ${NMPIS} + 1`
      FMPI=scali
      CFGecho "INFO: Found SCALI MPI ${MPICC}"
    fi
    if [ "${FMPI}" ]
    then
      if [ -z "${MPIS}" ]; then MPIS="${FMPI}"; else MPIS="${MPIS}|${FMPI}"; fi
      MPI="${FMPI}"
      break
    fi
  done
fi

if [ "${NMPIS}" -eq 0 ]
then
  if [ -n "${MPICC}" ]
  then
    CFGecho "ERROR: mpicc found but cannot determine MPI library"
    CFGecho "Select MPI using --mpi=mpich|mpich2|mpich3|mpibull2|lam|openmpi|bullxmpi|intel|intel2|intel3|hp|scali"
  else
    CFGecho "ERROR: Cannot detect MPI library"
    CFGecho "Make sure mpicc, mpcc or mpiicc is in your PATH and rerun configure"
    CFGecho "or specify --disable-mpi to build without MPI measurement support"
  fi
  exit 1
elif [ "${NMPIS}" -gt 1 ]
then
  CFGecho ""
  CFGecho "INFO: Found ${NMPIS} MPI installations"
  CFGecho "Select MPI using --mpi=${MPIS}"
  exit 1
fi
CFGecho ""
