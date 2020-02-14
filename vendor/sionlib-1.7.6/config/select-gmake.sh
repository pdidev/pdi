#!/bin/sh

# -- Uncomment for stand-alone use
#CFGecho () {
#  echo "$1"
#}

#ErrorAndExit () {
#  echo "ERROR: $1"
#  exit 1
#}

MK=""

MKT=`which gmake 2> /dev/null`
if [ -n "${MKT}" -a "`echo ${MKT} | awk '{print $1, $2}'`" != "no gmake" ]
then
  MKV=`${MKT} --version 2> /dev/null`
  if echo ${MKV} | grep 'GNU Make' > /dev/null
  then 
    MK=`basename ${MKT}`
  fi
fi

if [ -z "${MK}" ]
then
  MKT=`which gnumake 2> /dev/null`
  if [ -n "${MKT}" -a "`echo ${MKT} | awk '{print $1, $2}'`" != "no gnumake" ]
  then
    MKV=`${MKT} --version 2> /dev/null`
    if echo ${MKV} | grep 'GNU Make' > /dev/null
    then 
      MK=`basename ${MKT}`
    fi
  fi
fi

if [ -z "${MK}" ]
then
  MKT=`which make 2> /dev/null`
  if [ -n "${MKT}" -a "`echo ${MKT} | awk '{print $1, $2}'`" != "no make" ]
  then
    MKV=`${MKT} --version 2> /dev/null`
    if echo ${MKV} | grep 'GNU Make' > /dev/null
    then 
      MK=`basename ${MKT}`
    fi
  fi
fi

if [ -z "${MK}" ]
then
  ErrorAndExit "${PROJECT} requires GNU make; no GNU make found"
else
  CFGecho "INFO: ${PROJECT} requires GNU make for this configuration"
  CFGecho "INFO: Found ${MK}"
  CFGecho ""
fi
