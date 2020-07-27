#!/bin/sh

if [ "$#" -ne 4 ]; then
	echo "Error, expected 4 arguments:\n    $0 <PDI_EXAMPLE_DIR> <PDI_example_C> <flowvr_visu> <FLOWVR_PREFIX>"
	exit
fi

PDI_EXAMPLE_DIR="$1"
PDI_example_C="$2"
flowvr_visu="$3"
export FLOWVR_PREFIX="$4"

# Copy of unrelocatable flowvr-config.sh
export PATH="${FLOWVR_PREFIX}/share/flowvr/modules/bin:${FLOWVR_PREFIX}/bin${PATH:+:${PATH}}"
export FLOWVR_DATA_PATH="${FLOWVR_PREFIX}/share/flowvr-render/data${FLOWVR_DATA_PATH:+:${FLOWVR_DATA_PATH}}"
export LD_LIBRARY_PATH="${FLOWVR_PREFIX}/lib${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}"
export DYLD_LIBRARY_PATH="${FLOWVR_PREFIX}/lib${DYLD_LIBRARY_PATH:+:${DYLD_LIBRARY_PATH}}"
export PYTHONPATH="${FLOWVR_PREFIX}/lib/flowvr/python:${PYTHONPATH}"

export FLOWVR_PID_LOG_DIR="./.flowvr"

flowvrd > flowvrd.log &
DAEMON_PID=$!

$1/flowvr.py $1 $2 $3
flowvr -a flowvr > flowvr.log
FLOWVR_STATUS=$?

kill $DAEMON_PID

# in case flowvr daemon didn't unbind the socket
service network restart

exit $FLOWVR_STATUS
