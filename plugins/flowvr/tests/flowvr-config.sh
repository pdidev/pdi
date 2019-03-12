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