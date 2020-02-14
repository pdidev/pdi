#------------------------------------------------------------------------------
# General Settings
#------------------------------------------------------------------------------
#	- cp ../../parlib/opari* .

TOPDIR = ../..

include $(TOPDIR)/Makefile.defs
include $(TOPDIR)/mf/common.defs

CFLAGS_MPI  += $(CFLAGS) -I. -I../../lib -I../../parlib $(SION_DEBUG) $(SION_PLATFORM)  
CFLAGS_MPI  += $(MPIMFLAG)
LDFLAGS_MPI += $(LDFLAGS) -L../../parlib -l$(SION_LIBNAME_MPI) -L../../parlib -l$(SION_LIBNAME_GEN) -L../../lib -l$(SION_LIBNAME_SER) -l$(SION_LIBNAME_COM) -l$(SION_LIBNAME_COM_LOCK_PTHREADS) $(HINTSLIB) $(CACHELIB) $(MPILIB) $(CUDA_LIBRARY_PATHS) $(CUDA_LIBRARIES) $(SIONFWD_LIBS)

CFLAGS_MPI_GEN  += $(CFLAGS) -I. -I../../lib -I../../parlib $(SION_DEBUG) $(SION_PLATFORM)  
CFLAGS_MPI_GEN  += $(MPIMFLAG)
LDFLAGS_MPI_GEN += $(LDFLAGS) -L../../parlib -l$(SION_LIBNAME_GEN) -L../../lib -l$(SION_LIBNAME_COM) -l$(SION_LIBNAME_COM_LOCK_PTHREADS) $(HINTSLIB) $(CACHELIB) $(MPILIB) $(CUDA_LIBRARY_PATHS) $(CUDA_LIBRARIES) $(SIONFWD_LIBS)

CXXFLAGS_MPI   = $(CXXFLAGS) -I. -I../../lib -I../../parlib -I../../cppinterface $(SION_DEBUG) $(SION_PLATFORM)
CXXFLAGS_MPI  += $(MPIMFLAG)
CXXLDFLAGS_MPI = -L../../cppinterface -l$(SION_LIBNAME_CXX_MPI) -l$(SION_LIBNAME_CXX_SER) $(LDFLAGS_MPI)

CFLAGS_HYB  += $(CFLAGS) -I. -I../../lib -I../../parlib $(SION_DEBUG) $(SION_PLATFORM) 
CFLAGS_HYB  += $(MPIMFLAG) $(HYBMFLAG) $(HYBFLAG)
LDFLAGS_HYB += $(LDFLAGS) -L../../parlib -l$(SION_LIBNAME_OMPI) -L../../parlib -l$(SION_LIBNAME_GEN) -L../../lib -l$(SION_LIBNAME_COM) -l$(SION_LIBNAME_COM_LOCK_PTHREADS) $(HINTSLIB) $(CACHELIB) $(MPILIB) $(CUDA_LIBRARY_PATHS) $(CUDA_LIBRARIES) $(SIONFWD_LIBS)

CFLAGS_OMP  += $(CFLAGS) -I. -I../../lib -I../../parlib $(SION_DEBUG) $(SION_PLATFORM) 
CFLAGS_OMP  += $(OMPMFLAG) $(OMPFLAG) 
LDFLAGS_OMP += $(LDFLAGS) -L../../parlib -l$(SION_LIBNAME_OMP) -L../../parlib -l$(SION_LIBNAME_GEN) -L../../lib -l$(SION_LIBNAME_COM) -l$(SION_LIBNAME_COM_LOCK_PTHREADS) $(SION_SHM_FLAGS) $(HINTSLIB) $(CACHELIB) $(CUDA_LIBRARY_PATHS) $(CUDA_LIBRARIES) $(SIONFWD_LIBS)
CFLAGS_OMP  += $(SION_PTHREADS) 

F77FLAGS_MPI    = $(FFLAGS) -I../../fortraninterface
F77FLAGS_OMP    = $(FFLAGS) $(OMPFLAG)  -I../../fortraninterface
F77FLAGS_HYB    = $(FFLAGS) $(HYBFLAG) -I../../fortraninterface
LDFLAGS_F77 = ../../fortraninterface/lib$(SION_LIBNAME_F77_MPI).a ../../fortraninterface/lib$(SION_LIBNAME_F77_SER).a  -l$(SION_LIBNAME_COM) -l$(SION_LIBNAME_COM_LOCK_PTHREADS) -l$(SION_LIBNAME_SER) $(HINTSLIB) $(CACHELIB) $(MPILIB) $(CUDA_LIBRARY_PATHS) $(CUDA_LIBRARIES) $(SIONFWD_LIBS)

F90FLAGS_MPI  = $(FFLAGS) -I../../fortraninterface
F90FLAGS_OMP  = $(FFLAGS) $(OMPFLAG) -I../../fortraninterface
F90FLAGS_HYB  = $(FFLAGS) $(HYBFLAG) -I../../fortraninterface
LDFLAGS_F90 = ../../fortraninterface/lib$(SION_LIBNAME_F90_MPI).a ../../fortraninterface/lib$(SION_LIBNAME_F90_SER).a  -l$(SION_LIBNAME_COM) -l$(SION_LIBNAME_COM_LOCK_PTHREADS) -l$(SION_LIBNAME_SER) $(HINTSLIB) $(CACHELIB) $(MPILIB) $(CUDA_LIBRARY_PATHS) $(CUDA_LIBRARIES) $(SIONFWD_LIBS)

.SUFFIXES: .f .c .f90 .F .h. .com .o .cpp .hpp

# C++ Tests
ifeq ($(CXXENABLE),1)
ifeq ($(MPIENABLE),1)
     TESTS_MPI_x         = test_cxxwr__1.cpp
     TESTS_MPI_x_o       = $(TESTS_MPI_x:%.cpp=%.o)
     TESTS_MPI_x_exe     = $(TESTS_MPI_x:%.cpp=%)	
     TESTS_MPI_x_dir     = $(TESTS_MPI_x:%.cpp=run_%)
     TESTS_MPI_x_scripts = $(TESTS_MPI_x:%.cpp=%.run.sh)
     TESTS_MPI_x_scriptsC= $(TESTS_MPI_x:%.cpp=%.comp.sh)
     TESTS_MPI_x_scriptsE= $(TESTS_MPI_x:%.cpp=%.exec.sh)
     TESTS_MPI_x_stdout  = $(TESTS_MPI_x:%.cpp=%_stdout_0.log)
     TESTS_MPI_x_stderr  = $(TESTS_MPI_x:%.cpp=%_stderr_0.log)
     TESTS_SRCS         += $(TESTS_MPI_x)
     TESTS_EXES         += $(TESTS_MPI_x_exe)
     TESTS_DIRS         += $(TESTS_MPI_x_dir)
     TESTS_SCRIPTS      += $(TESTS_MPI_x_scripts)
     TESTS_SCRIPTSC     += $(TESTS_MPI_x_scriptsC)
     TESTS_SCRIPTSE     += $(TESTS_MPI_x_scriptsE)
     TESTS_LOGS         += $(TESTS_MPI_x_stdout)
     TESTS_LOGS         += $(TESTS_MPI_x_stderr)
endif
endif

# Generic Tests
ifeq ($(MPIENABLE),1)
  TESTS_MPI_g         = test_genapi_1.c test_genapi_2.c test_genapi_3.c test_genapi_4.c
  TESTS_MPI_g_o       = $(TESTS_MPI_g:%.c=%.o)
  TESTS_MPI_g_exe     = $(TESTS_MPI_g:%.c=%)
  TESTS_MPI_g_dir     = $(TESTS_MPI_g:%.c=run_%)
  TESTS_MPI_g_scripts = $(TESTS_MPI_g:%.c=%.run.sh)
  TESTS_MPI_g_scriptsC = $(TESTS_MPI_g:%.c=%.comp.sh)
  TESTS_MPI_g_scriptsE = $(TESTS_MPI_g:%.c=%.exec.sh)
  TESTS_MPI_g_stdout  = $(TESTS_MPI_g:%.c=%_stdout_0.log)
  TESTS_MPI_g_stderr  = $(TESTS_MPI_g:%.c=%_stderr_0.log)
  TESTS_SRCS         += $(TESTS_MPI_g)
  TESTS_EXES         += $(TESTS_MPI_g_exe)
  TESTS_DIRS         += $(TESTS_MPI_g_dir)
  TESTS_SCRIPTS      += $(TESTS_MPI_g_scripts)
  TESTS_SCRIPTSC     += $(TESTS_MPI_g_scriptsC)
  TESTS_SCRIPTSE     += $(TESTS_MPI_g_scriptsE)
  TESTS_LOGS         += $(TESTS_MPI_g_stdout)
  TESTS_LOGS         += $(TESTS_MPI_g_stderr)
endif

# C Tests
ifeq ($(MPIENABLE),1)
# test_dup____1.c
  TESTS_MPI_c         = test_param__1.c test_simple_2.c test_get_info.c test_seek___1.c  test_multi__2.c test_cache__1.c test_buffer_1.c 
  TESTS_MPI_c        += test_coll___1.c test_hints__1.c test_reinit_1.c test_mapped_1.c 
  TESTS_MPI_c        += test_keyval_1.c test_keyval_2.c
ifneq ($(strip $(SION_SIONFWD)),)
  TESTS_MPI_c        += test_sfwd___1.c test_sfwd___2.c
endif
  TESTS_MPI_c_o       = $(TESTS_MPI_c:%.c=%.o)
  TESTS_MPI_c_exe     = $(TESTS_MPI_c:%.c=%)
  TESTS_MPI_c_dir     = $(TESTS_MPI_c:%.c=run_%)
  TESTS_MPI_c_scripts = $(TESTS_MPI_c:%.c=%.run.sh)
  TESTS_MPI_c_scriptsC = $(TESTS_MPI_c:%.c=%.comp.sh)
  TESTS_MPI_c_scriptsE = $(TESTS_MPI_c:%.c=%.exec.sh)
  TESTS_MPI_c_stdout  = $(TESTS_MPI_c:%.c=%_stdout_0.log)
  TESTS_MPI_c_stderr  = $(TESTS_MPI_c:%.c=%_stderr_0.log)
  TESTS_SRCS         += $(TESTS_MPI_c)
  TESTS_EXES         += $(TESTS_MPI_c_exe)
  TESTS_DIRS         += $(TESTS_MPI_c_dir)
  TESTS_SCRIPTS      += $(TESTS_MPI_c_scripts)
  TESTS_SCRIPTSC     += $(TESTS_MPI_c_scriptsC)
  TESTS_SCRIPTSE     += $(TESTS_MPI_c_scriptsE)
  TESTS_LOGS         += $(TESTS_MPI_c_stdout)
  TESTS_LOGS         += $(TESTS_MPI_c_stderr)
endif

# MPI Multi file tests which uses also serial interface
ifeq ($(MPIENABLE),1)
  TESTS_MPI_cs         = test_simple_1.c test_multi__1.c test_multi__3.c test_buddy__1.c test_buddy__2.c test_fitest_1.c test_fitest_2.c
  TESTS_MPI_cs_o       = $(TESTS_MPI_cs:%.c=%.o)
  TESTS_MPI_cs_exe     = $(TESTS_MPI_cs:%.c=%)
  TESTS_MPI_cs_dir     = $(TESTS_MPI_cs:%.c=run_%)
  TESTS_MPI_cs_scripts = $(TESTS_MPI_cs:%.c=%.run.sh)
  TESTS_MPI_cs_scriptsC = $(TESTS_MPI_cs:%.c=%.comp.sh)
  TESTS_MPI_cs_scriptsE = $(TESTS_MPI_cs:%.c=%.exec.sh)
  TESTS_MPI_cs_stdout  = $(TESTS_MPI_cs:%.c=%_stdout_0.log)
  TESTS_MPI_cs_stderr  = $(TESTS_MPI_cs:%.c=%_stderr_0.log)
  TESTS_SRCS         += $(TESTS_MPI_cs)
  TESTS_EXES         += $(TESTS_MPI_cs_exe)
  TESTS_DIRS         += $(TESTS_MPI_cs_dir)
  TESTS_SCRIPTS      += $(TESTS_MPI_cs_scripts)
  TESTS_SCRIPTSC     += $(TESTS_MPI_cs_scriptsC)
  TESTS_SCRIPTSE     += $(TESTS_MPI_cs_scriptsE)
  TESTS_LOGS         += $(TESTS_MPI_cs_stdout)
  TESTS_LOGS         += $(TESTS_MPI_cs_stderr)
endif

ifeq ($(OMPENABLE),1)
ifneq ($(PLAT),bgp)
  TESTS_OMP_c         = test_omp____1.c test_lock___1.c test_ompcol_1.c
  TESTS_OMP_c_o       = $(TESTS_OMP_c:%.c=%.o)
  TESTS_OMP_c_exe     = $(TESTS_OMP_c:%.c=%)
  TESTS_OMP_c_dir     = $(TESTS_OMP_c:%.c=run_%)
  TESTS_OMP_c_scripts = $(TESTS_OMP_c:%.c=%.run.sh)
  TESTS_OMP_c_scriptsC = $(TESTS_OMP_c:%.c=%.comp.sh)
  TESTS_OMP_c_scriptsE = $(TESTS_OMP_c:%.c=%.exec.sh)
  TESTS_OMP_c_stdout = $(TESTS_OMP_c:%.c=%_stdout_0.log)
  TESTS_OMP_c_stderr = $(TESTS_OMP_c:%.c=%_stderr_0.log)
  TESTS_SRCS         += $(TESTS_OMP_c)
  TESTS_EXES         += $(TESTS_OMP_c_exe)
  TESTS_DIRS         += $(TESTS_OMP_c_dir)
  TESTS_SCRIPTS      += $(TESTS_OMP_c_scripts)
  TESTS_SCRIPTSC     += $(TESTS_OMP_c_scriptsC)
  TESTS_SCRIPTSE     += $(TESTS_OMP_c_scriptsE)
  TESTS_LOGS         += $(TESTS_OMP_c_stdout)
  TESTS_LOGS         += $(TESTS_OMP_c_stderr)
endif
endif
ifeq ($(HYBENABLE),1)
ifneq ($(PLAT),bgp)
  TESTS_HYB_c         = test_ompi___1.c test_ompicol1.c test_ompicol2.c test_ompimrg1.c
  TESTS_HYB_c_o       = $(TESTS_HYB_c:%.c=%.o)
  TESTS_HYB_c_exe     = $(TESTS_HYB_c:%.c=%)
  TESTS_HYB_c_dir     = $(TESTS_HYB_c:%.c=run_%)
  TESTS_HYB_c_scripts = $(TESTS_HYB_c:%.c=%.run.sh)
  TESTS_HYB_c_scriptsC = $(TESTS_HYB_c:%.c=%.comp.sh)
  TESTS_HYB_c_scriptsE = $(TESTS_HYB_c:%.c=%.exec.sh)
  TESTS_HYB_c_stdout = $(TESTS_HYB_c:%.c=%_stdout_0.log)
  TESTS_HYB_c_stderr = $(TESTS_HYB_c:%.c=%_stderr_0.log)
  TESTS_SRCS         += $(TESTS_HYB_c)
  TESTS_EXES         += $(TESTS_HYB_c_exe)
  TESTS_DIRS         += $(TESTS_HYB_c_dir)
  TESTS_SCRIPTS      += $(TESTS_HYB_c_scripts)
  TESTS_SCRIPTSC     += $(TESTS_HYB_c_scriptsC)
  TESTS_SCRIPTSE     += $(TESTS_HYB_c_scriptsE)
  TESTS_LOGS         += $(TESTS_HYB_c_stdout)
  TESTS_LOGS         += $(TESTS_HYB_c_stderr)
endif
endif


# Fortran Tests
ifeq ($(FORTRANENABLE),1)
ifeq ($(MPIENABLE),1)
# F77 Tests
  TESTS_MPI_f          = test_smplef_1.f test_mchnkf_1.f test_collf__1.f
  TESTS_MPI_f_o        = $(TESTS_MPI_f:%.f=%.o)
  TESTS_MPI_f_exe      = $(TESTS_MPI_f:%.f=%)
  TESTS_MPI_f_dir      = $(TESTS_MPI_f:%.f=run_%)
  TESTS_MPI_f_scripts  = $(TESTS_MPI_f:%.f=%.run.sh)
  TESTS_MPI_f_scriptsC = $(TESTS_MPI_f:%.f=%.comp.sh)
  TESTS_MPI_f_scriptsE = $(TESTS_MPI_f:%.f=%.exec.sh)
  TESTS_MPI_f_stdout   = $(TESTS_MPI_f:%.f=%_stdout_0.log)
  TESTS_MPI_f_stderr   = $(TESTS_MPI_f:%.f=%_stderr_0.log)
  TESTS_SRCS          += $(TESTS_MPI_f)
  TESTS_EXES          += $(TESTS_MPI_f_exe)
  TESTS_DIRS          += $(TESTS_MPI_f_dir)
  TESTS_SCRIPTS       += $(TESTS_MPI_f_scripts)
  TESTS_SCRIPTSC      += $(TESTS_MPI_f_scriptsC)
  TESTS_SCRIPTSE      += $(TESTS_MPI_f_scriptsE)
  TESTS_LOGS          += $(TESTS_MPI_f_stdout)
  TESTS_LOGS          += $(TESTS_MPI_f_stderr)
#  F90 Tests
  TESTS_MPI_F          = test_Fmpi___1.f90
  TESTS_MPI_F_o        = $(TESTS_MPI_F:%.f90=%.o)
  TESTS_MPI_F_exe      = $(TESTS_MPI_F:%.f90=%)
  TESTS_MPI_F_dir      = $(TESTS_MPI_F:%.f90=run_%)
  TESTS_MPI_F_scripts  = $(TESTS_MPI_F:%.f90=%.run.sh)
  TESTS_MPI_F_scriptsC = $(TESTS_MPI_F:%.f90=%.comp.sh)
  TESTS_MPI_F_scriptsE = $(TESTS_MPI_F:%.f90=%.exec.sh)
  TESTS_MPI_F_stdout   = $(TESTS_MPI_F:%.f90=%_stdout_0.log)
  TESTS_MPI_F_stderr   = $(TESTS_MPI_F:%.f90=%_stderr_0.log)
  TESTS_SRCS          += $(TESTS_MPI_F)
  TESTS_EXES          += $(TESTS_MPI_F_exe)
  TESTS_DIRS          += $(TESTS_MPI_F_dir)
  TESTS_SCRIPTS       += $(TESTS_MPI_F_scripts)
  TESTS_SCRIPTSC      += $(TESTS_MPI_F_scriptsC)
  TESTS_SCRIPTSE      += $(TESTS_MPI_F_scriptsE)
  TESTS_LOGS          += $(TESTS_MPI_F_stdout)
  TESTS_LOGS          += $(TESTS_MPI_F_stderr)
endif 
ifeq ($(OMPENABLE),1)
ifneq ($(PLAT),bgp)
# F77 Tests
  TESTS_OMP_f          = test_fomp___1.f
  TESTS_OMP_f_o        = $(TESTS_OMP_f:%.f=%.o)
  TESTS_OMP_f_exe      = $(TESTS_OMP_f:%.f=%)
  TESTS_OMP_f_dir      = $(TESTS_OMP_f:%.f=run_%)
  TESTS_OMP_f_scripts  = $(TESTS_OMP_f:%.f=%.run.sh)
  TESTS_OMP_f_scriptsC = $(TESTS_OMP_f:%.f=%.comp.sh)
  TESTS_OMP_f_scriptsE = $(TESTS_OMP_f:%.f=%.exec.sh)
  TESTS_OMP_f_stdout   = $(TESTS_OMP_f:%.f=%_stdout_0.log)
  TESTS_OMP_f_stderr   = $(TESTS_OMP_f:%.f=%_stderr_0.log)
  TESTS_SRCS          += $(TESTS_OMP_f)
  TESTS_EXES          += $(TESTS_OMP_f_exe)
  TESTS_DIRS          += $(TESTS_OMP_f_dir)
  TESTS_SCRIPTS       += $(TESTS_OMP_f_scripts)
  TESTS_SCRIPTSC      += $(TESTS_OMP_f_scriptsC)
  TESTS_SCRIPTSE      += $(TESTS_OMP_f_scriptsE)
  TESTS_LOGS          += $(TESTS_OMP_f_stdout)
  TESTS_LOGS          += $(TESTS_OMP_f_stderr)
  TESTSF              += 
# F90 Tests
  TESTS_OMP_F          = test_Fomp___1.f90
  TESTS_OMP_F_o        = $(TESTS_OMP_F:%.f90=%.o)
  TESTS_OMP_F_exe      = $(TESTS_OMP_F:%.f90=%)
  TESTS_OMP_F_dir      = $(TESTS_OMP_F:%.f90=run_%)
  TESTS_OMP_F_scripts  = $(TESTS_OMP_F:%.f90=%.run.sh)
  TESTS_OMP_F_scriptsC = $(TESTS_OMP_F:%.f90=%.comp.sh)
  TESTS_OMP_F_scriptsE = $(TESTS_OMP_F:%.f90=%.exec.sh)
  TESTS_OMP_F_stdout   = $(TESTS_OMP_F:%.f90=%_stdout_0.log)
  TESTS_OMP_F_stderr   = $(TESTS_OMP_F:%.f90=%_stderr_0.log)
  TESTS_SRCS          += $(TESTS_OMP_F)
  TESTS_EXES          += $(TESTS_OMP_F_exe)
  TESTS_DIRS          += $(TESTS_OMP_F_dir)
  TESTS_SCRIPTS       += $(TESTS_OMP_F_scripts)
  TESTS_SCRIPTSC      += $(TESTS_OMP_F_scriptsC)
  TESTS_SCRIPTSE      += $(TESTS_OMP_F_scriptsE)
  TESTS_LOGS          += $(TESTS_OMP_F_stdout)
  TESTS_LOGS          += $(TESTS_OMP_F_stderr)
  TESTSF              += 
endif
endif
ifeq ($(HYBENABLE),1)
ifneq ($(PLAT),bgp)
# F77 Tests
  TESTS_HYB_f          = test_ompif__1.f 
  TESTS_HYB_f_o        = $(TESTS_HYB_f:%.f=%.o)
  TESTS_HYB_f_exe      = $(TESTS_HYB_f:%.f=%)
  TESTS_HYB_f_dir      = $(TESTS_HYB_f:%.f=run_%)
  TESTS_HYB_f_scripts  = $(TESTS_HYB_f:%.f=%.run.sh)
  TESTS_HYB_f_scriptsC = $(TESTS_HYB_f:%.f=%.comp.sh)
  TESTS_HYB_f_scriptsE = $(TESTS_HYB_f:%.f=%.exec.sh)
  TESTS_HYB_f_stdout   = $(TESTS_HYB_f:%.f=%_stdout_0.log)
  TESTS_HYB_f_stderr   = $(TESTS_HYB_f:%.f=%_stderr_0.log)
  TESTS_SRCS          += $(TESTS_HYB_f)
  TESTS_EXES          += $(TESTS_HYB_f_exe)
  TESTS_DIRS          += $(TESTS_HYB_f_dir)
  TESTS_SCRIPTS       += $(TESTS_HYB_f_scripts)
  TESTS_SCRIPTSC      += $(TESTS_HYB_f_scriptsC)
  TESTS_SCRIPTSE      += $(TESTS_HYB_f_scriptsE)
  TESTS_LOGS          += $(TESTS_HYB_f_stdout)
  TESTS_LOGS          += $(TESTS_HYB_f_stderr)
  TESTSF              += 
# F90 Tests
  TESTS_HYB_F          = test_ompiF__1.f90 
  TESTS_HYB_F_o        = $(TESTS_HYB_F:%.f90=%.o)
  TESTS_HYB_F_exe      = $(TESTS_HYB_F:%.f90=%)
  TESTS_HYB_F_dir      = $(TESTS_HYB_F:%.f90=run_%)
  TESTS_HYB_F_scripts  = $(TESTS_HYB_F:%.f90=%.run.sh)
  TESTS_HYB_F_scriptsC = $(TESTS_HYB_F:%.f90=%.comp.sh)
  TESTS_HYB_F_scriptsE = $(TESTS_HYB_F:%.f90=%.exec.sh)
  TESTS_HYB_F_stdout   = $(TESTS_HYB_F:%.f90=%_stdout_0.log)
  TESTS_HYB_F_stderr   = $(TESTS_HYB_F:%.f90=%_stderr_0.log)
  TESTS_SRCS          += $(TESTS_HYB_F)
  TESTS_EXES          += $(TESTS_HYB_F_exe)
  TESTS_DIRS          += $(TESTS_HYB_F_dir)
  TESTS_SCRIPTS       += $(TESTS_HYB_F_scripts)
  TESTS_SCRIPTSC      += $(TESTS_HYB_F_scriptsC)
  TESTS_SCRIPTSE      += $(TESTS_HYB_F_scriptsE)
  TESTS_LOGS          += $(TESTS_HYB_F_stdout)
  TESTS_LOGS          += $(TESTS_HYB_F_stderr)
  TESTSF              += 
endif
endif
endif

# F77 Tests
F77TESTS = $(TESTSF:%.f=%)
F77TESTSo = $(TESTSF:%.f=%.o)
F77TESTSdirs = $(TESTSF:%.f=run_%)
F77TESTSscripts = $(TESTSF:%.f=%.run.sh)
# F90 Tests
F90TESTS = $(TESTSF:%.f90=%)
F90TESTSo = $(TESTSF:%.f90=%.o)
F90TESTSdirs = $(TESTSF:%.f90=run_%)
F90TESTSscripts = $(TESTSF:%.f90=%.run.sh)

ifeq ($(PYTHONENABLE),1)
ifeq ($(MPIENABLE),1)
  TESTS_MPI_py          = test_pympi__1.py
  TESTS_MPI_py_exe      = $(TESTS_MPI_py)
  TESTS_MPI_py_dir      = $(TESTS_MPI_py:%.py=run_%)
  TESTS_MPI_py_scripts  = $(TESTS_MPI_py:%.py=%.run.sh)
  TESTS_MPI_py_scriptsE = $(TESTS_MPI_py:%.py=%.exec.sh)
  TESTS_MPI_py_stdout   = $(TESTS_MPI_py:%.py=%_stdout_0.log)
  TESTS_MPI_py_stderr   = $(TESTS_MPI_py:%.py=%_stderr_0.log)
  TESTS_SRCS           += $(TESTS_MPI_py)
  TESTS_EXES           += $(TESTS_MPI_py_exe)
  TESTS_DIRS           += $(TESTS_MPI_py_dir)
  TESTS_SCRIPTS        += $(TESTS_MPI_py_scripts)
  TESTS_SCRIPTSE       += $(TESTS_MPI_py_scriptsE)
  TESTS_LOGS           += $(TESTS_MPI_py_stdout)
  TESTS_LOGS           += $(TESTS_MPI_py_stderr)
endif
endif

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

all: mpirun.defs $(TESTS_EXES)

ifeq ($(CXXENABLE),1)
TESTS_EXES_CXX_A = ../../cppinterface/lib$(SION_LIBNAME_CXX_SER).a ../../cppinterface/lib$(SION_LIBNAME_CXX_MPI).a
endif

$(TESTS_EXES): ../../lib/lib$(SION_LIBNAME_COM).a ../../parlib/lib$(SION_LIBNAME_GEN).a ../../lib/lib$(SION_LIBNAME_SER).a ../../parlib/lib$(SION_LIBNAME_MPI).a $(TESTS_EXES_CXX_A)

# MPI
$(TESTS_MPI_x_o): %.o : %.cpp
	$(MPICXX) $(CXXFLAGS_MPI) -c $< 
$(TESTS_MPI_x_exe): % : %.o
	$(MPICXX) $(CXXFLAGS_MPI) -o $@ $< $(CXXLDFLAGS_MPI)
$(TESTS_MPI_x_scriptsC): %.comp.sh : % 
	echo 'echo "compile $@.cpp ..."' >> _compall.sh
	echo '$(MPICXX) $(CXXFLAGS) -c `sionconfig --cflags --cxx --mpi --threadsafe $(PRECFL) $(SYSKINDFL)` $@.cpp' >> _compall.sh
	echo '$(MPICXX) $(LDFLAGS) -o $@ $@.o `sionconfig --libs --cxx --mpi --threadsafe $(PRECFL) $(SYSKINDFL)`' >> _compall.sh

$(TESTS_MPI_c_o): %.o : %.c
	$(MPICC) $(CFLAGS_MPI) -c $< 
$(TESTS_MPI_c_exe): % : %.o
	$(MPICC) $(CFLAGS_MPI) -o $@ $< $(LDFLAGS_MPI)
$(TESTS_MPI_c_scriptsC): %.comp.sh : % 
	echo 'echo "compile $@.c ..."' >> _compall.sh
	echo '$(MPICC) $(CFLAGS)  -c `sionconfig --cflags --c --mpi --threadsafe $(PRECFL) $(SYSKINDFL)` $@.c' >> _compall.sh
	echo '$(MPICC) $(LDFLAGS) -o $@ $@.o `sionconfig --libs --c --mpi --threadsafe $(PRECFL) $(SYSKINDFL)`' >> _compall.sh

$(TESTS_MPI_f_o): %.o : %.f
	$(MPIF77) $(F77FLAGS_MPI) -c $< 
$(TESTS_MPI_f_exe): % : %.o
	$(MPIF77) $(F77FLAGS_MPI) -o $@ $< $(LDFLAGS_F77) $(LDFLAGS_MPI)
$(TESTS_MPI_f_scriptsC): %.comp.sh : % 
	echo 'echo "compile $@.f ..."' >> _compall.sh
	echo '$(MPIF77) $(FFLAGS)  -c `sionconfig --cflags --f77 --mpi --threadsafe $(PRECFL) $(SYSKINDFL)` $@.f' >> _compall.sh
	echo '$(MPIF77) $(LDFLAGS)  -o $@ $@.o `sionconfig --libs --f77 --mpi --threadsafe $(PRECFL) $(SYSKINDFL)`' >> _compall.sh

$(TESTS_MPI_F_o): %.o : %.f90
	$(MPIF90) $(F90FLAGS_MPI) -c $< 
$(TESTS_MPI_F_exe): % : %.o
	$(MPIF90) $(F90FLAGS_MPI) -o $@ $< $(LDFLAGS_F90) $(LDFLAGS_MPI)
$(TESTS_MPI_F_scriptsC): %.comp.sh : % 
	echo 'echo "compile $@.f90 ..."' >> _compall.sh
	echo '$(MPIF90) $(FFLAGS)  -c `sionconfig --cflags --f90 --mpi --threadsafe $(PRECFL) $(SYSKINDFL)` $@.f90' >> _compall.sh
	echo '$(MPIF90) $(LDFLAGS) -o $@ $@.o `sionconfig --libs --f90 --mpi --threadsafe $(PRECFL) $(SYSKINDFL)`' >> _compall.sh

$(TESTS_MPI_cs_o): %.o : %.c
	$(MPICC) $(CFLAGS_MPI) -c $< 
$(TESTS_MPI_cs_exe): % : %.o
	$(MPICC) $(CFLAGS_MPI) -o $@ $< $(LDFLAGS_MPI)
$(TESTS_MPI_cs_scriptsC): %.comp.sh : % 
	echo 'echo "compile $@.c ..."' >> _compall.sh
	echo '$(MPICC) $(CFLAGS)  -c `sionconfig --cflags --c --mpi --threadsafe $(PRECFL) $(SYSKINDFL)` `sionconfig --cflags --c --ser $(PRECFL) $(SYSKINDFL)` $@.c' >> _compall.sh
	echo '$(MPICC) $(LDFLAGS) -o $@ $@.o `sionconfig --libs --c --mpi --threadsafe $(PRECFL) $(SYSKINDFL)` `sionconfig --libs --c --ser $(PRECFL) $(SYSKINDFL)`' >> _compall.sh

$(TESTS_MPI_g_o): %.o : %.c
	$(MPICC) $(CFLAGS_MPI_GEN) -c $< 
$(TESTS_MPI_g_exe): % : %.o
	$(MPICC) $(CFLAGS_MPI_GEN) -o $@ $< $(LDFLAGS_MPI_GEN)
$(TESTS_MPI_g_scriptsC): %.comp.sh : % 
	echo 'echo "compile $@.c ..."' >> _compall.sh
	echo '$(MPICC) $(CFLAGS)  -c `sionconfig --cflags --c --gen --threadsafe $(PRECFL) $(SYSKINDFL)` `sionconfig --cflags --c --mpi --threadsafe $(PRECFL) $(SYSKINDFL)` $@.c' >> _compall.sh
	echo '$(MPICC) $(LDFLAGS) -o $@ $@.o `sionconfig --libs --c --gen --threadsafe $(PRECFL) $(SYSKINDFL)`' >> _compall.sh


# OMP
$(TESTS_OMP_c_o): %.o : %.c
	$(CC) $(CFLAGS_OMP) -c $< 
$(TESTS_OMP_c_exe): % : %.o
	$(CC) $(CFLAGS_OMP) -o $@ $< $(LDFLAGS_OMP)
$(TESTS_OMP_c_scriptsC): %.comp.sh : % 
	echo 'echo "compile $@.c ..."' >> _compall.sh
	echo '$(CC) $(CFLAGS)  $(OMPFLAG) $(SION_DEBUG) -c `sionconfig --cflags --c --omp --threadsafe $(PRECFL) $(SYSKINDFL)` $@.c' >> _compall.sh
	echo '$(CC) $(LDFLAGS) $(OMPFLAG) $(SION_DEBUG) -o $@ $@.o `sionconfig --libs --c --omp --threadsafe $(PRECFL) $(SYSKINDFL)`' >> _compall.sh

$(TESTS_OMP_f_o): %.o : %.f
	$(F77) $(F77FLAGS_OMP) -c $< 
$(TESTS_OMP_f_exe): % : %.o
	$(F77) $(F77FLAGS_OMP) -o $@ $< $(LDFLAGS_F77) $(LDFLAGS_OMP)
$(TESTS_OMP_f_scriptsC): %.comp.sh : % 
	echo 'echo "compile $@.f ..."' >> _compall.sh
	echo '$(F77) $(FFLAGS) $(OMPFLAG) -c `sionconfig --cflags --f77 --omp --threadsafe $(PRECFL) $(SYSKINDFL)` $@.f' >> _compall.sh
	echo '$(F77) $(OMPFLAG) $(LDFLAGS) -o $@ $@.o `sionconfig --libs --f77 --omp --threadsafe $(PRECFL) $(SYSKINDFL)`' >> _compall.sh

$(TESTS_OMP_F_o): %.o : %.f90
	$(F90) $(F90FLAGS_OMP) -c $< 
$(TESTS_OMP_F_exe): % : %.o
	$(F90) $(F90FLAGS_OMP) -o $@ $< $(LDFLAGS_F90) $(LDFLAGS_OMP)
$(TESTS_OMP_F_scriptsC): %.comp.sh : % 
	echo 'echo "compile $@.f ..."' >> _compall.sh
	echo '$(F90) $(FFLAGS) $(OMPFLAG) -c `sionconfig --cflags --f90 --omp --threadsafe $(PRECFL) $(SYSKINDFL)` $@.f90' >> _compall.sh
	echo '$(F90) $(OMPFLAG) $(LDFLAGS) -o $@ $@.o `sionconfig --libs --f90 --omp --threadsafe $(PRECFL) $(SYSKINDFL)`' >> _compall.sh

# HYB
$(TESTS_HYB_c_o): %.o : %.c
	$(MPICC) $(CFLAGS_HYB) -c $< 
$(TESTS_HYB_c_exe): % : %.o
	$(MPICC) $(CFLAGS_HYB) -o $@ $< $(LDFLAGS_HYB)
$(TESTS_HYB_c_scriptsC): %.comp.sh : % 
	echo 'echo "compile $@.c ..."' >> _compall.sh
	echo '$(MPICC) $(HYBFLAG) -c `sionconfig --cflags --c --ompi --threadsafe $(PRECFL) $(SYSKINDFL)` $@.c' >> _compall.sh
	echo '$(MPICC) $(HYBFLAG) $(LDFLAGS) -o $@ $@.o `sionconfig --libs --c --ompi --threadsafe $(PRECFL) $(SYSKINDFL)`' >> _compall.sh

$(TESTS_HYB_f_o): %.o : %.f
	$(MPIF77) $(F77FLAGS_HYB) -c $< 
$(TESTS_HYB_f_exe): % : %.o
	$(MPIF77) $(F77FLAGS_HYB) -o $@ $< $(LDFLAGS_F77) $(LDFLAGS_HYB)
$(TESTS_HYB_f_scriptsC): %.comp.sh : % 
	echo 'echo "compile $@.f ..."' >> _compall.sh
	echo '$(MPIF77) $(FFLAGS) $(HYBFLAG)  -c `sionconfig --cflags --f77 --ompi --threadsafe $(PRECFL) $(SYSKINDFL)` $@.f' >> _compall.sh
	echo '$(MPIF77) $(HYBFLAG) $(LDFLAGS) -o $@ $@.o `sionconfig --libs --f77 --ompi --threadsafe  $(PRECFL) $(SYSKINDFL)`' >> _compall.sh

$(TESTS_HYB_F_o): %.o : %.f90
	$(MPIF90) $(F90FLAGS_HYB) -c $< 
$(TESTS_HYB_F_exe): % : %.o
	$(MPIF90) $(F90FLAGS_HYB) -o $@ $< $(LDFLAGS_F90) $(LDFLAGS_HYB)
$(TESTS_HYB_F_scriptsC): %.comp.sh : % 
	echo 'echo "compile $@.f90 ..."' >> _compall.sh
	echo '$(MPIF90) $(FFLAGS) $(HYBFLAG)  -c `sionconfig --cflags --f90 --ompi --threadsafe $(PRECFL) $(SYSKINDFL)` $@.f90' >> _compall.sh
	echo '$(MPIF90) $(HYBFLAG) $(LDFLAGS) -o $@ $@.o `sionconfig --libs --f90 --ompi --threadsafe  $(PRECFL) $(SYSKINDFL)`' >> _compall.sh


test: cleantest $(TESTS_SCRIPTS) kcov_output

kcov_output:
	if [ "${COVERAGE}" = "kcov" ] && type kcov 2>&1 > /dev/null; then kcov --merge kcov_output run_test_*/kcov_output; fi

# this silences errors due to python files keeping their extension
test_pympi__1:

$(TESTS_SCRIPTS): Makefile siondump mpirun.defs
$(TESTS_SCRIPTS): %.run.sh : % 
	env COVERAGE=$(COVERAGE) bash ./$@

$(TESTS_SCRIPTSE): %.exec.sh : % 
	echo "bash ./$@" >> _runall.sh


mpirun.defs: mpirun.defs.in
	sed -e "s!@MPITESTRUN@!$(MPITESTRUN)!" mpirun.defs.in | sed -e "s!@MPITESTEXECPARM@!$(MPITESTEXECPARM)!"  > ./mpirun.defs

siondump: ../../../siondump
	cp ../../../siondump ./siondump

__compall.sh: 
	echo "#!/bin/bash" > ./$@
	echo "set -e" > ./$@
	echo "# compile all parallel tests" >> ./$@
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
	echo "# run all parallel tests" > ./$@

packtest: _compall.sh $(TESTS_SCRIPTSC)
packtest: _runall.sh  $(TESTS_SCRIPTSE)
packtest: siondump mpirun.defs
	- rm -rf paralleltests
	- mkdir paralleltests

	sed -e 's,.comp.sh,,g' _compall.sh >paralleltests/compall.sh
	rm _compall.sh 		 
	chmod u+x paralleltests/compall.sh

	sed -e 's,.exec.sh,.run.sh,g' _runall.sh >paralleltests/runall.sh
	rm _runall.sh		
	chmod u+x paralleltests/runall.sh

	cp -p siondump ./paralleltests/siondump
	cp -p mpirun.defs ./paralleltests/mpirun.defs
	cp -p test_genapi_INC_MPI_api.c $(TESTS_SRCS) $(TESTS_SCRIPTS) $(TESTS_LOGS)  paralleltests/
ifeq ($(PYTHONENABLE),1)
	( for script in $(TESTS_MPI_py_scripts) ; do \
	sed -e "s,SIONPYPATH=../../../pythoninterface,SIONPYPATH=`readlink -f $(PREFIX)`/lib,g" \
	  $$script > paralleltests/$$script ; \
	done )
endif
	tar czf paralleltests.tar.gz paralleltests

installtests: install
install: packtest
	$(MKDIR) -m 755 $(TESTDIR)
	echo "$(INSTALL) -c -m 644 paralleltests.tar.gz $(TESTDIR)" ; 
	eval "$(INSTALL) -c -m 644 paralleltests.tar.gz $(TESTDIR)" ; 
	( cd $(TESTDIR); tar xzf paralleltests.tar.gz)

checkinst: checkinstallation
checkinstallation: packtest
	$(MKDIR) -m 755 $(TESTDIR)
	( cd paralleltests; sh compall.sh)
	( cd paralleltests; sh runall.sh)

.PHONY: clean
clean:
	@echo Clean  
	-rm -rf $(TESTS_DIRS) 
	-rm -f $(TESTS_EXES)
	-rm -f $(TESTS_MPI_x_o)
	-rm -f $(TESTS_MPI_c_o)
	-rm -f $(TESTS_MPI_cs_o)
	-rm -f $(TESTS_MPI_g_o)
	-rm -f $(TESTS_MPI_f_o)
	-rm -f $(TESTS_MPI_F_o)
	-rm -f $(TESTS_OMP_c_o)
#	-rm -f $(TESTS_OMP_f_o)
	-rm -f $(TESTS_HYB_c_o)
	-rm -f $(TESTS_HYB_f_o)
	-rm -f mpirun.defs
	-rm -f  ../../paralleltests.tar.gz
	-rm -rf kcov_output

cleantest:
	@echo Clean test directories
	-rm -rf $(TESTS_DIRS)
	-rm -rf kcov_output

