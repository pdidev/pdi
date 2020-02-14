#------------------------------------------------------------------------------
# General Settings
#------------------------------------------------------------------------------

TOPDIR = ../..
SRCDIR = ../../../../src

include $(TOPDIR)/Makefile.defs
include $(TOPDIR)/mf/common.defs

CFLAGS += $(SION_DEBUG) -g
CFLAGS += -I. -I../../lib -I../../parlib
CFLAGS += $(SION_PLATFORM)
LDFLAGS += -L../../lib -l$(SION_LIBNAME_SER) -l$(SION_LIBNAME_COM) -l$(SION_LIBNAME_COM_LOCK_NONE) $(HINTSLIB) $(CACHELIB) $(CUDA_LIBRARY_PATHS) $(CUDA_LIBRARIES) $(SIONFWD_LIBS)
CXXLDFLAGS = -L../../cppinterface -l$(SION_LIBNAME_CXX_SER) $(LDFLAGS)
CXXFLAGS = $(CXXFLAGS) -I../../cppinterface
FFLAGS +=  -g
FFLAGS += -I. -I../../fortraninterface
FFLAGS += -I. -I../../lib -I../../parlib
F90FLAGS +=  -g
F90FLAGS += -I. -I../../fortraninterface
F90FLAGS += -I. -I../../lib -I../../parlib

LDFLAGS_F77 = -L../../lib -L../../fortraninterface
LDFLAGS_F77 += -l$(SION_LIBNAME_F77_SER)
LDFLAGS_F77 += -l$(SION_LIBNAME_SER) -l$(SION_LIBNAME_COM)
LDFLAGS_F77 += -l$(SION_LIBNAME_COM_LOCK_NONE) $(HINTSLIB) $(CACHELIB)
LDFLAGS_F77 += $(CUDA_LIBRARY_PATHS) $(CUDA_LIBRARIES) $(SIONFWD_LIBS)

LDFLAGS_F90 = -I../../fortraninterface -L../../lib -L../../fortraninterface
LDFLAGS_F90 += -l$(SION_LIBNAME_F90_SER)
LDFLAGS_F90 += -l$(SION_LIBNAME_SER) -l$(SION_LIBNAME_COM)
LDFLAGS_F90 += -l$(SION_LIBNAME_COM_LOCK_NONE) $(HINTSLIB) $(CACHELIB)
LDFLAGS_F90 += $(CUDA_LIBRARY_PATHS) $(CUDA_LIBRARIES) $(SIONFWD_LIBS)

.SUFFIXES: .f .c .f90 .F .h. .com .o .cpp .hpp

TESTS = test_seropn_1.c test_mandel_1.c test_keyval_1.c test_ktab___1.c \
	test_ktab___2.c test_kmngr__1.c test_seek___1.c \
	test_flags__1.c test_2read__1.c test_file___1.c test_file___2.c \
	test_file___3.c test_file___4.c test_swap___1.c

CTESTS = $(TESTS:%.c=%)
CTESTSo = $(TESTS:%.c=%.o)
CTESTSdirs = $(TESTS:%.c=run_%)
CTESTSscripts = $(TESTS:%.c=%.run.sh)
CTESTSstdout = $(TESTS:%.c=%_stdout.log)
CTESTSstderr = $(TESTS:%.c=%_stderr.log)
CTESTSscriptsE = $(TESTS:%.c=%.exec.sh)
CTESTSscriptsC = $(TESTS:%.c=%.comp.sh)

ifeq ($(FORTRANENABLE),1)
TESTSF77 = test_fsropn_1.f
TESTSF90 = test_Fsropn_1.f90
endif

F77TESTS = $(TESTSF77:%.f=%)
F77TESTSo = $(TESTSF77:%.f=%.o)
F77TESTSdirs = $(TESTSF77:%.f=run_%)
F77TESTSscripts = $(TESTSF77:%.f=%.run.sh)
F77TESTSstdout = $(TESTSF77:%.f=%_stdout.log)
F77TESTSstderr = $(TESTSF77:%.f=%_stderr.log)
F77TESTSscriptsE = $(TESTSF77:%.f=%.exec.sh)
F77TESTSscriptsC = $(TESTSF77:%.f=%.comp.sh)

F90TESTS = $(TESTSF90:%.f90=%)
F90TESTSo = $(TESTSF90:%.f90=%.o)
F90TESTSdirs = $(TESTSF90:%.f90=run_%)
F90TESTSscripts = $(TESTSF90:%.f90=%.run.sh)
F90TESTSstdout = $(TESTSF90:%.f90=%_stdout.log)
F90TESTSstderr = $(TESTSF90:%.f90=%_stderr.log)
F90TESTSscriptsE = $(TESTSF90:%.f90=%.exec.sh)
F90TESTSscriptsC = $(TESTSF90:%.f90=%.comp.sh)

ifeq ($(CXXENABLE),1)
#TESTCXX = test_cxsopn_1.cpp
endif

CXXTESTS = $(TESTCXX:%.cpp=%)
CXXTESTSo = $(TESTCXX:%.cpp=%.o)
CXXTESTSdirs = $(TESTCXX:%.cpp=run_%)
CXXTESTSscripts = $(TESTCXX:%.cpp=%.run.sh)
CXXTESTSstdout = $(TESTCXX:%.cpp=%_stdout.log)
CXXTESTSstderr = $(TESTCXX:%.cpp=%_stderr.log)
CXXTESTSscriptsE = $(TESTCXX:%.cpp=%.exec.sh)
CXXTESTSscriptsC = $(TESTCXX:%.cpp=%.comp.sh)

ifeq ($(PYTHONENABLE),1)
TESTSPY = test_pysopn_1.py
endif

PYTESTS = $(TESTSPY)
PYTESTSdirs = $(TESTSPY:%.py=run_%)
PYTESTSscripts = $(TESTSPY:%.py=%.run.sh)
PYTESTSstdout = $(TESTSPY:%.py=%_stdout.log)
PYTESTSstderr = $(TESTSPY:%.py=%_stderr.log)
PYTESTSscriptsE = $(TESTSPY:%.py=%.exec.sh)

TESTSGEN = test_genapi_1.c

GENTESTS = $(TESTSGEN:%.c=%)
GENTESTSo = $(TESTSGEN:%.c=%.o)
GENTESTSdirs = $(TESTSGEN:%.c=run_%)
GENTESTSscripts = $(TESTSGEN:%.c=%.run.sh)
GENTESTSstdout = $(TESTSGEN:%.c=%_stdout.log)
GENTESTSstderr = $(TESTSGEN:%.c=%_stderr.log)
GENTESTSscriptsE = $(TESTSGEN:%.c=%.exec.sh)
GENTESTSscriptsC = $(TESTSGEN:%.c=%.comp.sh)

ifdef BGPREC
PRECFL=--$(BGPREC)
else
PRECFL=--$(PREC)
endif

ifeq ($(SYSKIND),fe)
SYSKINDFL=--fe
endif

ifeq ($(SYSKIND),be)
SYSKINDFL=--be
endif


all: $(CTESTS) $(F77TESTS) $(F90TESTS) $(CXXTESTS) $(PYTESTS) $(GENTESTS)

$(CTESTS): $(CTESTSo) ../../lib/lib$(SION_LIBNAME_SER).a
	$(CC) $(CFLAGS) -o $@  $@.o $(LDFLAGS)

$(F77TESTS): $(F77TESTSo) ../../lib/lib$(SION_LIBNAME_SER).a ../../fortraninterface/lib$(SION_LIBNAME_F77_SER).a
	$(F77) $(FFLAGS) -o $@  $@.o $(LDFLAGS_F77)
$(GENTESTS): $(GENTESTSo) ../../parlib/lib$(SION_LIBNAME_GEN).a
	$(CC) $(CFLAGS) -o $@  $@.o -L../../parlib -l$(SION_LIBNAME_GEN) $(LDFLAGS)

$(F90TESTS): $(F90TESTSo) ../../lib/lib$(SION_LIBNAME_SER).a ../../fortraninterface/lib$(SION_LIBNAME_F90_SER).a
	$(F90) $(F90FLAGS) -o $@  $@.o $(LDFLAGS_F90)

$(CXXTESTS): $(CXXTESTSo) ../../lib/lib$(SION_LIBNAME_SER).a ../../cppinterface/lib$(SION_LIBNAME_CXX_SER).a
	$(CXX) $(CXXFLAGS) -o $@  $@.o $(CXXLDFLAGS)



test: cleantests $(CTESTSscripts) $(F77TESTSscripts) $(F90TESTSscripts) $(CXXTESTSscripts) $(PYTESTSscripts) $(GENTESTSscripts) kcov_output

kcov_output:
	if [ "${COVERAGE}" = "kcov" ] && type kcov 2>&1 > /dev/null; then kcov --merge kcov_output run_test_*/kcov_output; fi

$(CTESTSscripts): $(CTESTS) Makefile siondump
	env COVERAGE=$(COVERAGE) sh ./$@

$(F77TESTSscripts): $(F77TESTS) Makefile siondump
	env COVERAGE=$(COVERAGE) sh ./$@

$(F90TESTSscripts): $(F90TESTS) Makefile siondump
	env COVERAGE=$(COVERAGE) sh ./$@

$(CXXTESTSscripts): $(CXXTESTS) Makefile siondump
	env COVERAGE=$(COVERAGE) sh ./$@

$(PYTESTSscripts): $(PYTESTS) Makefile siondump
	env COVERAGE=$(COVERAGE) sh ./$@

$(GENTESTSscripts): $(GENTESTS) Makefile siondump
	env COVERAGE=$(COVERAGE) sh ./$@


$(CTESTSscriptsC): $(CTESTS) Makefile siondump
	echo 'echo "compile $@.c ..."' >> _compall.sh
	echo '$(CC) -c `sionconfig --cflags --c --ser $(PRECFL) $(SYSKINDFL)` $@.c'      >> _compall.sh
	echo '$(CC) -o $@ $@.o `sionconfig --libs --c --ser $(PRECFL) $(SYSKINDFL)`'     >> _compall.sh

$(F77TESTSscriptsC): $(F77TESTS) Makefile siondump
	echo 'echo "compile $@.f ..."' >> _compall.sh
	echo '$(F77) -c `sionconfig --cflags --f77 --ser $(PRECFL) $(SYSKINDFL)` $@.f'   >> _compall.sh
	echo '$(F77) -o $@ $@.o `sionconfig --libs --f77 --ser $(PRECFL) $(SYSKINDFL)`'  >> _compall.sh

$(F90TESTSscriptsC): $(F90TESTS) Makefile siondump
	echo 'echo "compile $@.f90 ..."' >> _compall.sh
	echo '$(F90) -c `sionconfig --cflags --f90 --ser $(PRECFL) $(SYSKINDFL)` $@.f90' >> _compall.sh
	echo '$(F90) -o $@ $@.o `sionconfig --libs --f90 --ser $(PRECFL) $(SYSKINDFL)`'  >> _compall.sh

$(CXXTESTSscriptsC): $(CXXTESTS) Makefile siondump
	echo 'echo "compile $@.cpp ..."' >> _compall.sh
	echo '$(CXX) -c `sionconfig --cflags --cxx --ser $(PRECFL) $(SYSKINDFL)` $@.cpp' >> _compall.sh
	echo '$(CXX) -o $@ $@.o `sionconfig --libs --cxx --ser $(PRECFL) $(SYSKINDFL)`'  >> _compall.sh

$(GENTESTSscriptsC): $(GENTESTS) Makefile siondump
	echo 'echo "compile $@.c ..."' >> _compall.sh
	echo '$(CC) -c `sionconfig --cflags --c --gen $(PRECFL) $(SYSKINDFL)` $@.c'      >> _compall.sh
	echo '$(CC) -o $@ $@.o `sionconfig --libs --c --gen $(PRECFL) $(SYSKINDFL)`'     >> _compall.sh



$(CTESTSscriptsE): $(CTESTS) Makefile siondump
	echo "sh ./$@" >> _runall.sh

$(F77TESTSscriptsE): $(F77TESTS) Makefile siondump
	echo "sh ./$@" >> _runall.sh

$(F90TESTSscriptsE): $(F90TESTS) Makefile siondump
	echo "sh ./$@" >> _runall.sh

$(CXXTESTSscriptsE): $(CXXTESTS) Makefile siondump
	echo "sh ./$@" >> _runall.sh

$(PYTESTSscriptsE): $(PYTESTS) Makefile siondump
	echo "sh ./$@" >> _runall.sh

$(GENTESTSscriptsE): $(GENTESTS) Makefile siondump
	echo "sh ./$@" >> _runall.sh



%.o: %.c Makefile
	$(CC)  -c $(CFLAGS) $(SIONINC) $<
%.o: %.f Makefile
	$(F77) $(FFLAGS) -c  $(SIONINCF) $<
%.o: %.f90 Makefile
	$(F90) $(FFLAGS) -c $(SIONINCF) $<
%.o: %.cpp Makefile
	$(CXX) -c $(CXXFLAGS) $<

siondump: ../../../siondump
	cp ../../../siondump ./siondump

__compall.sh:
	echo "#!/bin/bash" > ./$@
	echo "set -e" > ./$@
	echo "# compile all serial tests" >> ./$@
	echo 'PATH=@INSTDIR@/bin:$$PATH' >> ./$@

_compall.sh: __compall.sh
	cd $(PREFIX)
	AINSTDIR=`(cd $(PREFIX) && pwd)`; \
	sed -e "s!@ARCH@!$(ARCH)!" \
		-e "s!@INSTDIR@!$$AINSTDIR!" \
		-e "s!@LIBNAME@!$(SION_LIBNAME_PAR)!" \
		-e "s!@LIBSERNAME@!$(SION_LIBNAME_SER)!" \
		-e "s!@HINTSLIB@!$(HINTSLIB)!" \
		__compall.sh > _compall.sh
	rm __compall.sh

_runall.sh:
	echo "set -e" > ./$@
	echo "# run all serial tests" > ./$@

packtest: _compall.sh $(CTESTSscriptsC) $(F77TESTSscriptsC) $(F90TESTSscriptsC) $(CXXTESTSscriptsC) $(GENTESTSscriptsC)
packtest: _runall.sh $(CTESTSscriptsE) $(F77TESTSscriptsE) $(F90TESTSscriptsE) $(CXXTESTSscriptsE) $(PYTESTSscriptsE)  $(GENTESTSscriptsE)
packtest: siondump
	-rm -rf serialtests
	-mkdir serialtests

	sed -e 's,.comp.sh,,g' _compall.sh >serialtests/compall.sh
	rm _compall.sh
	chmod u+x serialtests/compall.sh

	sed -e 's,.exec.sh,.run.sh,g' _runall.sh >serialtests/runall.sh
	rm _runall.sh
	chmod u+x serialtests/runall.sh

	cp -p siondump ./serialtests/siondump
	cp -p test_Fsropn_1.sion test_mandel_1.sion serialtests/
	cp -p $(SRCDIR)/lib/sion_keyvalue_keymngr.h serialtests/
	cp -p $(SRCDIR)/lib/sion_keyvalue_table.h serialtests/
	cp -p $(SRCDIR)/lib/sion_filedesc.h serialtests/
	cp -p $(SRCDIR)/lib/sion_file.h serialtests/
	cp -p $(SRCDIR)/lib/sion_flags.h serialtests/
	cp -p $(CTESTSscripts)   $(TESTS)    $(CTESTSstdout)   $(CTESTSstderr) serialtests/
ifeq ($(FORTRANENABLE),1)
	cp -p $(F77TESTSscripts) $(TESTSF77) $(F77TESTSstdout)   $(F77TESTSstderr) serialtests/
	cp -p $(F90TESTSscripts) $(TESTSF90) $(F90TESTSstdout)   $(F90TESTSstderr) serialtests/
endif
ifeq ($(CXXENABLE),1)
#	cp -p $(CXXTESTSscripts) $(TESTSCXX) $(CXXTESTSstdout) $(CXXTESTSstderr) serialtests/
endif
ifeq ($(PYTHONENABLE),1)
	cp -p $(TESTSPY) $(PYTESTSstdout) $(PYTESTSstderr) serialtests/
	( for script in $(PYTESTSscripts) ; do \
	sed -e "s,SIONPYPATH=../../../pythoninterface,SIONPYPATH=`readlink -f $(PREFIX)`/lib,g" \
	  $$script > serialtests/$$script ; \
	done )
endif
	cp -p $(GENTESTSscripts) $(TESTSGEN) $(GENTESTSstdout) $(GENTESTSstderr) serialtests/
	tar czf serialtests.tar.gz serialtests

installtests: install
install: packtest
	$(MKDIR) -m 755 $(TESTDIR)
	echo "$(INSTALL) -c -m 644 serialtests.tar.gz $(TESTDIR)" ;
	eval "$(INSTALL) -c -m 644 serialtests.tar.gz $(TESTDIR)" ;
	( cd $(TESTDIR); tar xzf serialtests.tar.gz)

checkinst: checkinstallation
checkinstallation: packtest
	( cd serialtests; sh compall.sh)
	( cd serialtests; sh runall.sh)

.PHONY: clean
clean:
	@echo Clean
	-rm -rf $(CTESTSdirs) $(F77TESTSdirs) $(F90TESTSdirs) $(CXXTESTSdirs) $(PYTESTSdirs) $(GENTESTSdirs)
	-rm -f $(CTESTS) $(CTESTSo) $(F77TESTS) $(F90TESTS) $(CXXTESTS) $(PYTESTS) $(GENTESTS) $(F77TESTSo) $(F90TESTSo) $(CXXTESTSo) $(GENTESTSo)
	-rm -f siondump
	-rm -f serialtests.tar.gz
	-rm -r serialtests
	-rm -rf kcov_output

cleantests:
	@echo Clean test directories
	-rm -rf $(CTESTSdirs) $(F77TESTSdirs) $(F90TESTSdirs) $(CXXTESTSdirs) $(PYTESTSdirs)
	-rm -rf kcov_output
