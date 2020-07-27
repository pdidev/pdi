#!/bin/sh

export FLOWVR_PREFIX="$(dirname "$(echo "@FLOWVR_INCLUDE_DIR@" | sed 's/.*;//')")"

export PATH="${FLOWVR_PREFIX}/share/flowvr/modules/bin:\
${FLOWVR_PREFIX}/bin\
${PATH:+:${PATH}}"

export FLOWVR_DATA_PATH="\
${FLOWVR_PREFIX}/share/flowvr-render/data\
${FLOWVR_DATA_PATH:+:${FLOWVR_DATA_PATH}}\
"

export LD_LIBRARY_PATH="${FLOWVR_PREFIX}/lib\
${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}"

export DYLD_LIBRARY_PATH="${FLOWVR_PREFIX}/lib\
${DYLD_LIBRARY_PATH:+:${DYLD_LIBRARY_PATH}}"

export FLOWVR_PID_LOG_DIR="@CMAKE_CURRENT_BINARY_DIR@/flowvr-dir"

export PYTHONPATH="${FLOWVR_PREFIX}/lib/flowvr/python:${PYTHONPATH}"
