#!/bin/sh

# this is a filter-and-test for 2 machines (can be extended to more machines, but the host-mapping
# hast to be adapted for that).

# PRECONDITIONS:
# 1) add "./lib" to your LD_LIBRARY_PATH
# 2) REPLACE 'malecite' with the hostname of your flowvr launch console
# 3) edit the filtertest.csv ; at least enter two hostnames in the first line where flowvr-daemons are running

flowvr -x \
       -b malecite \
       -H filterandtest.csv \
       --complib lib/libfilterandtest.comp.so \
       -Q config/filterandtest.prm \
       FilterAndTest
