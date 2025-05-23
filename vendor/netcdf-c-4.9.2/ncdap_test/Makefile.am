## This is a automake file, part of Unidata's netCDF package.
# Copyright 2018v, see the COPYRIGHT file for more information.

# This file builds and runs DAP tests.

# Put together AM_CPPFLAGS and AM_LDFLAGS.
include $(top_srcdir)/lib_flags.am

# Un comment to use a more verbose test driver
#SH_LOG_DRIVER = $(SHELL) $(top_srcdir)/test-driver-verbose
#LOG_DRIVER = $(SHELL) $(top_srcdir)/test-driver-verbose
#TEST_LOG_DRIVER = $(SHELL) $(top_srcdir)/test-driver-verbose
#TESTS_ENVIRONMENT = export SETX=1;

# Note which tests depend on other tests. Necessary for make -j check.
TEST_EXTENSIONS = .sh

LDADD = ${top_builddir}/liblib/libnetcdf.la
AM_CPPFLAGS += -I$(top_srcdir)/liblib
AM_CPPFLAGS += -DTOPSRCDIR=${abs_top_srcdir}
AM_CPPFLAGS += -DTOPBINDIR=${abs_top_builddir}

# Set up the tests; do the .sh first, then .c
check_PROGRAMS =
TESTS =

t_dap3a_SOURCES = t_dap3a.c t_srcdir.h
test_cvt3_SOURCES = test_cvt.c t_srcdir.h
test_vara_SOURCES = test_vara.c t_srcdir.h

if ENABLE_DAP
check_PROGRAMS += t_dap3a test_cvt3 test_vara
TESTS += t_dap3a test_cvt3 test_vara
if BUILD_UTILITIES
TESTS += tst_ncdap3.sh
endif

# remote tests are optional
# because the server may be down or inaccessible

if ENABLE_DAP_REMOTE_TESTS
noinst_PROGRAMS = findtestserver pingurl
findtestserver_SOURCES = findtestserver.c
pingurl_SOURCES = pingurl.c

if BUILD_UTILITIES
TESTS += tst_ber.sh tst_remote3.sh tst_formatx.sh testurl.sh tst_fillmismatch.sh tst_zero_len_var.sh tst_encode.sh
endif

TESTS += test_partvar

if ENABLE_DAP_LONG_TESTS
 TESTS += tst_longremote3.sh
 tst_longremote3.log: tst_remote3.log
if ENABLE_EXTERNAL_SERVER_TESTS
 test_manyurls_SOURCES = test_manyurls.c manyurls.h
 check_PROGRAMS += test_manyurls
 test_manyurls.log: tst_longremote3.log
 TESTS += test_manyurls
endif
endif

test_partvar_SOURCES = test_partvar.c

t_misc_SOURCES = t_misc.c

#TESTS += t_ncf330
TESTS += t_misc

test_nstride_cached_SOURCES = test_nstride_cached.c
TESTS += test_nstride_cached
check_PROGRAMS += test_nstride_cached
test_varm3_SOURCES = test_varm3.c
TESTS += test_varm3
check_PROGRAMS += test_varm3

check_PROGRAMS += test_partvar
check_PROGRAMS += t_misc
check_PROGRAMS += t_ncf330

if ENABLE_DAP_AUTH_TESTS
TESTS += testauth.sh
endif

endif #ENABLE_DAP_REMOTE_TESTS

endif #ENABLE_DAP

# Need to add subdirs
SUBDIRS = testdata3 expected3 expectremote3

EXTRA_DIST = tst_ncdap3.sh  \
             tst_remote3.sh \
             tst_longremote3.sh \
	     tst_zero_len_var.sh \
             tst_filelists.sh tst_urls.sh tst_utils.sh \
	     t_dap.c CMakeLists.txt tst_formatx.sh testauth.sh testurl.sh \
	     t_ncf330.c tst_ber.sh tst_fillmismatch.sh tst_encode.sh \
	     findtestserver.c.in

CLEANFILES = test_varm3 test_cvt3 file_results/* remote_results/* datadds* t_dap3a test_nstride_cached *.exe tmp*.txt
# This should only be left behind if using parallel io
CLEANFILES += tmp_*

DISTCLEANFILES = findtestserver.c

# This rule are used if someone wants to rebuild t_dap3a.c
# Otherwise never invoked, but records how to do it.
t_dap3a.c: t_dap.c
	echo "#define NETCDF3ONLY" > ./t_dap3a.c
	cat t_dap.c >> t_dap3a.c

# One last thing
BUILT_SOURCES = .dodsrc

.dodsrc:
	echo "#DODSRC" >.dodsrc
	echo "HTTP.READ.BUFFERSIZE=max" >>.dodsrc
	echo "HTTP.KEEPALIVE=60/60" >>.dodsrc

clean-local: clean-local-check

.PHONY: clean-local-check

clean-local-check:
	-rm -rf results
	-rm -f .dodsrc

# If valgrind is present, add valgrind targets.
@VALGRIND_CHECK_RULES@
