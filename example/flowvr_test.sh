#!/bin/sh

if [ -z "${PDI_STAGING}" ]; then
	#guess the PDI staging location
	export PDI_STAGING=$(readlink -f ../../../../staging/)
fi

export PATH="${PDI_STAGING}/bin:${PATH}"
export LD_LIBRARY_PATH="${PDI_STAGING}/lib/:${LD_LIBRARY_PATH}"

export FLOWVR_PREFIX="${PDI_STAGING}"
export PATH="${PDI_STAGING}/share/flowvr/modules/bin${PATH:+:${PATH}}"
export FLOWVR_DATA_PATH="${PDI_STAGING}/share/flowvr-render/data${FLOWVR_DATA_PATH:+:${FLOWVR_DATA_PATH}}"
export DYLD_LIBRARY_PATH="${PDI_STAGING}/lib${DYLD_LIBRARY_PATH:+:${DYLD_LIBRARY_PATH}}"
export FLOWVR_PID_LOG_DIR="./.flowvr"
export FlowVR_DIR="${PDI_STAGING}/share/flowvr/cmake"
export PYTHONPATH=${PDI_STAGING}/lib/flowvr/python/:${PYTHONPATH}
export PYTHONPATH=${PDI_STAGING}/lib/python2.7/dist-packages/:${PYTHONPATH}

# needed to force flowvr to not use `--bind-to hwthread' mpirun flag
echo "rank 0=localhost slot=0:0" >> rankfile.txt
echo "rank 1=localhost slot=0:0" >> rankfile.txt
echo "rank 2=localhost slot=0:0" >> rankfile.txt
echo "rank 3=localhost slot=0:0" >> rankfile.txt

flowvrd > /dev/null &
DAEMON_PID=$!
#echo "DAEMON PID = $DAEMON_PID"

python2.7 $1/flowvr.py $1 $2 $3
flowvr -a flowvr > /dev/null

FLOWVR_STATUS=$?
# echo "FLOWVR_STATUS = $FLOWVR_STATUS"

kill $DAEMON_PID
rm rankfile.txt

exit $FLOWVR_STATUS