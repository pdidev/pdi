#!/bin/bash
cmake -DDIST_PROFILE=Devel -DUSE_DEFAULT=SYSTEM -DUSE_Bpp=SYSTEM . && \
make -j
if [ -z "${DISABLE_CTEST}" ]; then
    ctest --output-on-failure
fi
