#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
##
#
# Tools HDF5 Makefile(.in)
#

include $(top_srcdir)/config/commence.am

if BUILD_TESTS_CONDITIONAL
   TESTSERIAL_DIR =test
else
   TESTSERIAL_DIR=
endif

CONFIG=ordered

# All subdirectories
SUBDIRS=lib src $(TESTSERIAL_DIR)

# Test with just the native connector, with a single pass-through connector
# and with a doubly-stacked pass-through.
VOL_LIST = native "pass_through under_vol=0;under_info={}" \
           "pass_through under_vol=505;under_info={under_vol=0;under_info={}}"

include $(top_srcdir)/config/conclude.am
