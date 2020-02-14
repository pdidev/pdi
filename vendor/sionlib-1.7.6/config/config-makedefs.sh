#!/bin/sh

# -- Uncomment for stand-alone use
#CFGecho () {
#  echo "$1"
#}

#ErrorAndExit () {
#  echo "ERROR: $1"
#  exit 1
#}

CFGecho "Configuring ${CF}"

if [ -n "${PREFIX}" ]
then
  CFGecho "- Setting PREFIX=${PREFIX}"
  sed -e 's,^PREFIX *= .*$,PREFIX = '${PREFIX}',' ${CF} > sed.out \
      && mv sed.out ${CF} || exit 1
fi

if [ -n "${SWCLOCK}" ]
then
  CFGecho "- Enabling switch clock"
  sed -e 's,^PLATCC   = xlc$,& -DUSE_SWITCH_CLOCK,' \
      -e 's,^UTILLIB  =$,& -lswclock,' \
      ${CF} > sed.out && mv sed.out ${CF} || exit 1
fi

if [ -n "${MMTIMER}" ]
then
  CFGecho "- Enabling multimedia timer"
  sed -e 's,^PLATCC   = icc$,& -DUSE_MMTIMER,' \
      ${CF} > sed.out && mv sed.out ${CF} || exit 1
fi

if [ -n "${COMP}" ]
then
  CFGecho "- Configuring for ${COMP} v${COMPVER} compilers"

  # Disable building hybrid MPI/OpenMP analysis components on platforms
  # which are known to cause trouble
  if [ "${COMP}" = "gnu" ]
  then
    case "${COMPVER}" in
      4.2.*)
        HYBRID=""
        sed -e 's,^OMPCXX,#OMPCXX,' \
            -e 's,^HYBCXX,#HYBCXX,' \
            ${CF} > sed.out && mv sed.out ${CF} || exit 1
        ;;
    esac
  fi

  # Enable new IBM compiler instrumentation interface for XLC/XLF > 11/13
  if [ "${COMP}" = "ibm" ]
  then
    XLC_MAJOR=`echo ${CVER} | sed -e 's/\..*$//'`
    XLF_MAJOR=`echo ${FVER} | sed -e 's/\..*$//'`

    if [ ${XLC_MAJOR} -ge 11 ]
    then
      if [ ${XLF_MAJOR} -lt 13 ]
      then
        ErrorAndExit "Unsupported combination of compiler versions!"
      fi
      sed -e 's/^CINSTFLAG.*$/CINSTFLAG = -qfunctrace/' \
          ${CF} > sed.out && mv sed.out ${CF} || exit 1
    fi
  fi
fi

if [ -n "${GFORTRAN}" ]
then
  CFGecho "- Using gfortran instead of g77"
  sed -e 's,^F77.*$,F77      = gfortran,' \
      -e 's,^F90\s.*$,F90      = gfortran,' \
      ${CF} > sed.out && mv sed.out ${CF} || exit 1
fi

if [ -n "${MPI}" ]
then
  if [ ! -f "../mf/mpi-${MPI}.def" ]
  then
    ErrorAndExit "Unknown MPI '${MPI}'"
  fi
  CFGecho "- Configuring ${MPI} MPI library"
  sed -e '/^MPI_CONFIG_TARGETS/r '../mf/mpi-${MPI}.def -e '/^MPI/d' \
      -e '/^[FP]MPI/d' -e '/^#MPI/d' -e '/^#[FP]MPI/d' ${CF} > sed.out \
      && mv sed.out ${CF} || exit 1

  # Check whether RMA is supported and enabled
  # TODO: Guard parts in scout with more fine grained defines
  if [ ! -z "`echo ${EXCL_MPI_DEFS} | grep RMA`" -o ! -z "${DISABLE_MPI_RMA_SUPPORT}" ]; then
      CFGecho "- Disabling MPI-2 RMA analysis"
      sed -e 's,-DHAS_MPI2_1SIDED,,' \
          ${CF} > sed.out && mv sed.out ${CF} || exit 1
  fi
fi

if [ -n "${MSA}" ]
then
  CFGecho "- Configuring with optimized collectives for MSA type: ${MSA}"

  case ${MSA} in
    hostname-regex)
      sed -e 's,^SION_MSA\s*=.*$,SION_MSA = -D_SION_MSA_HOSTNAME_REGEX,' \
          ${CF} > sed.out && mv sed.out ${CF} || exit 1
      ;;
    deep-est-sdv)
      sed -e 's,^SION_MSA\s*=.*$,SION_MSA = -D_SION_MSA_DEEP_EST_SDV,' \
          ${CF} > sed.out && mv sed.out ${CF} || exit 1
      ;;
    test)
      sed -e 's,^SION_MSA\s*=.*$,SION_MSA = -D_SION_MSA_TEST,' \
          ${CF} > sed.out && mv sed.out ${CF} || exit 1
      ;;
  esac
fi

if [ -n "${CUDA}" ]
then
  CFGecho "- Configuring with CUDA support: ${CUDA}"

  sed -e 's,^SION_CUDA\s*=.*$,SION_CUDA = -D_SION_CUDA,' \
      -e 's,^CUDA_LIBRARIES\s*=.*$,CUDA_LIBRARIES = -lcudart,' \
      ${CF} > sed.out && mv sed.out ${CF} || exit 1

  case ${CUDA} in
    system)
      ;;
    *)
      sed -e "s,^CUDA_INCLUDE_PATHS\\s*.*\$,CUDA_INCLUDE_PATHS = -I${CUDA}/include," \
          ${CF} > sed.out && mv sed.out ${CF} || exit 1

      if [ -d ${CUDA}/lib64 ]
      then
        sed -e "s,^CUDA_LIBRARY_PATHS\\s*.*\$,CUDA_LIBRARY_PATHS = -L${CUDA}/lib64," \
            ${CF} > sed.out && mv sed.out ${CF} || exit 1
      else
        sed -e "s,^CUDA_LIBRARY_PATHS\\s*.*\$,CUDA_LIBRARY_PATHS = -L${CUDA}/lib," \
            ${CF} > sed.out && mv sed.out ${CF} || exit 1
      fi
      ;;
  esac
fi

if [ -n "${SIONFWD}" ]
then
  CFGecho "- Configuring with SIONfwd I/O forwarding: ${SIONFWD}"

  sed -e 's,^SION_SIONFWD\s*=.*$,SION_SIONFWD = -D_SION_SIONFWD,' \
      ${CF} > sed.out && mv sed.out ${CF} || exit 1

  case ${SIONFWD} in
    system)
      if ! pkg-config --exists sionfwd-client
      then
        CFGecho "SIONfwd not found!"
        exit 1
      fi
      SIONFWD_CFLAGS=`pkg-config --cflags sionfwd-client`
      SIONFWD_LIBS=`pkg-config --static --libs sionfwd-client`
      ;;
    *)
      if [ -d ${SIONFWD}/lib64/pkgconfig ]
      then
        SIONFWD_PKG_CONFIG_PATH=${SIONFWD}/lib64/pkgconfig
      else
        SIONFWD_PKG_CONFIG_PATH=${SIONFWD}/lib/pkgconfig
      fi

      if ! env PKG_CONFIG_PATH="${SIONFWD_PKG_CONFIG_PATH}" pkg-config --exists sionfwd-client
      then
        CFGecho "SIONfwd not found!"
        exit 1
      fi
      SIONFWD_CFLAGS=`env PKG_CONFIG_PATH="${SIONFWD_PKG_CONFIG_PATH}" pkg-config --cflags sionfwd-client`
      SIONFWD_LIBS=`env PKG_CONFIG_PATH="${SIONFWD_PKG_CONFIG_PATH}" pkg-config --static --libs sionfwd-client`
      ;;
  esac

  sed -e "s#^SIONFWD_CFLAGS.*\$#SIONFWD_CFLAGS = ${SIONFWD_CFLAGS}#" \
      -e "s#^SIONFWD_LIBS.*\$#SIONFWD_LIBS = ${SIONFWD_LIBS}#" \
      ${CF} > sed.out && mv sed.out ${CF} || exit 1
fi

if [ "${CF}" != "Makefile.defs.fe" ]
then
  if [ -n "${BINUTILSDIR}" ]; then
    CFGecho "- Configuring Binutils (using ${BINUTILSDIR})"
    sed -e "s|^BINUTILS.*$|BINUTILS     = ${BINUTILSDIR}|" ${CF} > sed.out \
        && mv sed.out ${CF} || exit 1
  fi
fi

if [ -n "${SZLIB}" ]
then
  CFGecho "- Disabling trace compression"
  sed -e 's,^SZLIB,#SZLIB,' ${CF} > sed.out &&  mv sed.out ${CF} || exit 1
fi

if [ -n "${SIONLIB}" ]
then
  echo "- Enabling sionlib for trace file I/O"
  sed -e 's,^#SIONLIB,SIONLIB,' \
      -e 's,^SIONLIB_CONFIG.*$,SIONLIB_CONFIG      = '${SIONLIBCONFIG}',' \
       ${CF} > sed.out &&  mv sed.out ${CF} || exit 1
fi

if [ -n "${SIONDEBUG}" ]
then
    if [ "${SIONDEBUG}" = "yes" ]
    then
	echo "- Enabling DEBUG for sionlib"
	sed -e 's,^SION_DEBUG.*$,SION_DEBUG      = -DSION_DEBUG,' \
	    ${CF} > sed.out &&  mv sed.out ${CF} || exit 1
    fi
fi

if [ -n "${COVERAGE}" ]
then
    echo "- Collecting test suite coverage data with ${COVERAGE}"
    sed -e "s,^COVERAGE.*\$,COVERAGE = ${COVERAGE}," \
        ${CF} > sed.out && mv sed.out ${CF} || exit 1
fi

if [ -n "${PTHREADSUPPORT}" ]
then
    if [ "${PTHREADSUPPORT}" = "no" ]
    then
	echo "- Disable Pthread support for sionlib"
	sed -e 's,^SION_PTHREADS.*$,SION_PTHREADS      = ,' \
	    -e 's,^PTHREADSUPPORT.*$,PTHREADSUPPORT      = no,' \
	    ${CF} > sed.out &&  mv sed.out ${CF} || exit 1
    fi
fi

if [ -n "${ENABLE_HINTS}" ]
then
    if [ -n "${ENABLE_HINTS_GPFS}" -a "${PREC}" = "64" ]
    then
	echo "- Enabling GPFS Hints for sionlib"
	sed -e 's,^HINTSDEF\s*=,HINTSDEF      = -DSION_HINTS_GPFS,' \
            -e 's,^HINTSINC\s*=,HINTSINC      = \$(GPFS_HINTS_INC),' \
            -e 's,^HINTSLIB\s*=,HINTSLIB      = \$(GPFS_HINTS_LIB),' \
	    ${CF} > sed.out &&  mv sed.out ${CF} || exit 1
    fi
    if [ -n "${ENABLE_HINTS_LINUX}" ]
    then
	echo "- Enabling LINUX Hints for sionlib"
	sed -e 's,^HINTSDEF\s*=,HINTSDEF      = -DSION_HINTS_LINUX,' \
            -e 's,^HINTSINC\s*=,HINTSINC      = \$(LINUX_HINTS_INC),' \
            -e 's,^HINTSLIB\s*=,HINTSLIB      = \$(LINUX_HINTS_LIB),' \
	    ${CF} > sed.out &&  mv sed.out ${CF} || exit 1
    fi
fi

if [ -n "${DISABLE}" ]
then
  CFGecho "- Disabling ${DISABLE} support"
  case ${DISABLE} in
    mpi) sed -e 's,^MPIENABLE\s*=.*$,MPIENABLE      = 0,' \
	     -e 's,^HYBENABLE\s*=.*$,HYBENABLE      = 0,' \
                ${CF} > sed.out && mv sed.out ${CF} || exit 1
        ;;
    omp) sed -e 's,^OMPENABLE\s*=.*$,OMPENABLE      = 0,' \
	     -e 's,^HYBENABLE\s*=.*$,HYBENABLE      = 0,' \
                ${CF} > sed.out && mv sed.out ${CF} || exit 1
        ;;
    ompi) sed -e 's,^MPIENABLE\s*=.*$,MPIENABLE      = 0,' \
	      -e 's,^HYBENABLE\s*=.*$,HYBENABLE      = 0,' \
	      -e 's,^OMPENABLE\s*=.*$,OMPENABLE      = 0,' \
                 ${CF} > sed.out && mv sed.out ${CF} || exit 1
        ;;
  esac
fi

if [ -n "${NOFOR}" ]
then
    CFGecho "- Disabling Fortran support"
    sed -e 's,^FORTRANENABLE.*$,FORTRANENABLE = 0,' \
 ${CF} > sed.out &&  mv sed.out ${CF} || exit 1
fi

if [ -n "${PYVER}" ]
then
    CFGecho "- Enabling Python support"
    sed -e 's,^PYTHONENABLE.*$,PYTHONENABLE = 1,' \
 ${CF} > sed.out &&  mv sed.out ${CF} || exit 1
fi

if [ -n "${NOCXX}" ]
then
    CFGecho "- Disabling CXX support"
    sed -e 's,^CXXENABLE.*$,CXXENABLE = 0,' \
 ${CF} > sed.out &&  mv sed.out ${CF} || exit 1
fi

if [ -n "${NOPARUTIL}" ]
then
    CFGecho "- Disabling parutils compilation"
    sed -e 's,^PARUTILENABLE.*$,PARUTILENABLE = 0,' \
 ${CF} > sed.out &&  mv sed.out ${CF} || exit 1
fi

# Fix CFGKEY and CONFIG variables
if [ "${CF}" != "Makefile.defs.be" ]
then
    sed -e 's,^CFGKEY.*$,CFGKEY = '${CFG}',' \
        ${CF} > sed.out && mv sed.out ${CF} || exit 1
    sed -e 's,^CONFIG.*$,CONFIG = '"${CFGMSG}"',' \
        ${CF} > sed.out && mv sed.out ${CF} || exit 1
fi

if [ -n "${PREC}" ]
then
  if [ -n "${DEFPREC}" ]
  then
    CFGecho "- Using default ${PREC}-bit compilation mode"
  else
    CFGecho "- Setting ${PREC}-bit compilation mode"
  fi
  sed -e 's,^PREC .*$,PREC   = '${PREC}',' ${CF} \
      > sed.out &&  mv sed.out ${CF} || exit 1
fi
if [ "${CF}" = "Makefile.defs" -a "${MF}" = "Makefile.32-64" ]
then
  sed -e 's,^PREC .*$,DEFAULTPREC   = '${PREC}',' ${CF} \
      > sed.out &&  mv sed.out ${CF} || exit 1
fi

if [ "${CF}" != "Makefile.defs.be" ]
then
  # wxWidgets is not needed on backend
  if [ -n "${WX}" ]
  then
    CFGecho "- Using wxWidgets version ${WX}"
    sed -e 's,^#WX,WX,' ${CF} > sed.out &&  mv sed.out ${CF} || exit 1
  fi

  # Qt is not needed on backend
  if [ -n "${QT}" ]
  then
    CFGecho "- Using Qt version ${QT}"
    sed -e 's,^#QT,QT,' \
        -e 's,^QT_QMAKE \(.*\)=.*$,QT_QMAKE \1= '${QMAKE}',' \
        ${CF} > sed.out &&  mv sed.out ${CF} || exit 1
  fi
fi

# For combined 32/64-bit builds, compile Qt-based GUI with precision
# of Qt library
if [ "${CF}" = "Makefile.defs" -a "${MF}" = "Makefile.32-64" -a \
     -n "${QT}" -a "${QTPREC}" = "64" ]
then
  sed -e 's,^\(CUBE_BUILD_TARGETS64.*\)$,\1 \$(QT_BUILD_TARGETS),' \
      -e 's,^\(CUBE_INSTALL_TARGETS64.*\)$,\1 \$(QT_INSTALL_TARGETS),' \
    ${CF} > sed.out &&  mv sed.out ${CF} || exit 1
else
  sed -e 's,^\(CUBE_BUILD_TARGETS .*\)$,\1 \$(QT_BUILD_TARGETS),' \
      -e 's,^\(CUBE_INSTALL_TARGETS .*\)$,\1 \$(QT_INSTALL_TARGETS),' \
    ${CF} > sed.out &&  mv sed.out ${CF} || exit 1
fi
