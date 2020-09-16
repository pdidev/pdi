#!/bin/bash
cmake -DDIST_PROFILE=Devel -DUSE_DEFAULT=EMBEDDED -DUSE_SIONlib=SYSTEM . && \
make -j
if [ -z "${DISABLE_CTEST}" ]; then
    ctest --output-on-failure
fi
