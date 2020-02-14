#/****************************************************************************
#**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
#*****************************************************************************
#**  Copyright (c) 2008-2019                                                **
#**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
#**                                                                         **
#**  See the file COPYRIGHT in the package base directory for details       **
#****************************************************************************/
#------------------------------------------------------------------------------
# General Settings
#------------------------------------------------------------------------------

TOPDIR = ..

include $(TOPDIR)/Makefile.defs
include $(TOPDIR)/mf/common.defs
include $(TOPDIR)/mf/Makefile.gitrev

CFLAGS += $(SION_DEBUG) $(SION_PTHREADS)
CFLAGS += -I. -I../lib -I../parlib 
CFLAGS += $(SION_PLATFORM) 
CFLAGS += $(SION_CUDA) $(CUDA_INCLUDE_PATHS)

CMPIFLAGS = $(MPIMFLAG)
COMPFLAGS = $(OMPMFLAG) $(OMPFLAG)
CHYBFLAGS = $(HYBMFLAG) $(HYBFLAG)

CMPILDFLAGS = $(LDFLAGS) -L../parlib -l$(SION_LIBNAME_MPI) -L../lib -l$(SION_LIBNAME_GEN) -L../lib -l$(SION_LIBNAME_COM) -l$(SION_LIBNAME_COM_LOCK_PTHREADS) $(HINTSLIB) $(CACHELIB) $(CUDA_LIBRARY_PATHS) $(CUDA_LIBRARIES) $(SIONFWD_LIBS)

COMPLDFLAGS = $(LDFLAGS) -L../parlib -l$(SION_LIBNAME_OMP)  -L../lib -l$(SION_LIBNAME_COM) -l$(SION_LIBNAME_COM_LOCK_PTHREADS) $(HINTSLIB) $(CACHELIB) $(OMPFLAG) $(CUDA_LIBRARY_PATHS) $(CUDA_LIBRARIES) $(SIONFWD_LIBS)
CHYBLDFLAGS = $(LDFLAGS) -L../parlib -l$(SION_LIBNAME_OMPI) -L../lib -l$(SION_LIBNAME_GEN) -L../lib -l$(SION_LIBNAME_COM) -l$(SION_LIBNAME_COM_LOCK_PTHREADS) $(HINTSLIB) $(CACHELIB) $(OMPFLAG) $(CUDA_LIBRARY_PATHS) $(CUDA_LIBRARIES) $(SIONFWD_LIBS)

FMPILDFLAGS = $(LDFLAGS) -L../parlib -l$(SION_LIBNAME_MPI) -L../lib -l$(SION_LIBNAME_GEN) -L../lib -l$(SION_LIBNAME_SER) -l$(SION_LIBNAME_COM) -l$(SION_LIBNAME_COM_LOCK_PTHREADS) $(HINTSLIB) $(CACHELIB) $(CUDA_LIBRARY_PATHS) $(CUDA_LIBRARIES) $(SIONFWD_LIBS)
FOMPLDFLAGS = $(COMPLDFLAGS)  $(OMPFLAG)
FHYBLDFLAGS = $(COMPILDFLAGS) $(OMPFLAG)

CFLAGS += -DGIT_REV='"$(GIT_REV)"'

.SUFFIXES:  .cpp .c .f

USER_HDRS = 
USER_LIBS = 

ifeq ($(MPIENABLE),1)
  USER_UTILS += partest
endif
ifeq ($(HYBENABLE),1)
  USER_UTILS += ompi_partest
endif
ifeq ($(FORTRANENABLE),1)
  ifeq ($(MPIENABLE),1)
    USER_UTILS += fpartest
  endif
endif

PARTEST_OBJS = partest.o \
partest_split_comm.o \
partest_sionfile.o \
partest_mpiio.o \
partest_tasklocalfile.o \
partest_util.o \
partest_opts.o

PARTEST_OMPI_OBJS = ompi_partest.o \
	ompi_partest_sionfile.o \
	omp_partest_sionfile.o \
	partest_opts.o

FPARTEST_OBJS = fpartest.o

all: $(USER_UTILS)

partest: $(PARTEST_OBJS) ../parlib/lib$(SION_LIBNAME_MPI).a  ../parlib/lib$(SION_LIBNAME_GEN).a  ../lib/lib$(SION_LIBNAME_SER).a 
	$(MPICC) -o $@ $(PARTEST_OBJS) -L../lib -l$(SION_LIBNAME_SER) $(CMPILDFLAGS) $(MPILIB)

ompi_partest: $(PARTEST_OMPI_OBJS) ../parlib/lib$(SION_LIBNAME_OMPI).a ../parlib/lib$(SION_LIBNAME_OMP).a ../lib/lib$(SION_LIBNAME_SER).a 
	$(HYBCC) -o $@ $(PARTEST_OMPI_OBJS) $(CHYBLDFLAGS) -L../parlib -l$(SION_LIBNAME_OMP) -L../lib -l$(SION_LIBNAME_SER)  $(MPILIB)

ompi_partest.o : ompi_partest.c
	$(HYBCC) $(CHYBFLAGS) $(COMPFLAGS) $(CFLAGS) -c ompi_partest.c

ompi_partest_sionfile.o : ompi_partest_sionfile.c
	$(MPICC) $(CHYBFLAGS) $(CFLAGS) -c ompi_partest_sionfile.c

omp_partest_sionfile.o : omp_partest_sionfile.c
	$(HYBCC) $(COMPFLAGS) $(CHYBFLAGS) $(CFLAGS) -c omp_partest_sionfile.c

partest.o : partest.c
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) -c partest.c

partest_split_comm.o : partest_split_comm.c
	$(PLATCC) $(CMPIFLAGS) $(CFLAGS) -c partest_split_comm.c

partest_sionfile.o : partest_sionfile.c
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) -c partest_sionfile.c

partest_mpiio.o : partest_mpiio.c
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) -c partest_mpiio.c

partest_util.o : partest_util.c
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) -c partest_util.c

partest_tasklocalfile.o : partest_tasklocalfile.c
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) -c partest_tasklocalfile.c

partest_opts.o : partest_opts.c
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) -c partest_opts.c

fpartest.o : fpartest.F90
	$(MPIF90) $(FFLAGS) -c fpartest.F90

fpartest: fpartest.o  ../parlib/lib$(SION_LIBNAME_MPI).a  ../parlib/lib$(SION_LIBNAME_GEN).a ../lib/lib$(SION_LIBNAME_SER).a ../fortraninterface/lib$(SION_LIBNAME_F77_MPI).a ../fortraninterface/lib$(SION_LIBNAME_F77_SER).a
	$(MPIF90) -o $@ fpartest.o  -L../fortraninterface -l$(SION_LIBNAME_F77_MPI) -l$(SION_LIBNAME_F77_SER) $(FMPILDFLAGS) $(MPILIB) 


install: all
	$(MKDIR) -m 755 $(INCDIR)
	@list='$(USER_HDRS)'; for p in $$list; do \
		echo "$(INSTALL) -c -m 644 $$p $(INCDIR)" ; \
		eval "$(INSTALL) -c -m 644 $$p $(INCDIR)" ; \
	done
	$(MKDIR) -m 755 $(LIBDIR)
	@list='$(USER_LIBS)'; for p in $$list; do \
		echo "$(INSTALL) -c -m 644 $$p $(LIBDIR)" ; \
		eval "$(INSTALL) -c -m 644 $$p $(LIBDIR)" ; \
	done
	$(MKDIR) -m 755 $(BINDIR)
	@list='$(USER_UTILS)'; for p in $$list; do \
		echo "$(INSTALL) -c -m 755 $$p $(BINDIR)" ; \
		eval "$(INSTALL) -c -m 755 $$p $(BINDIR)" ; \
	done

depend:
	@echo "Generating/updating dependencies"
	@echo "#------------------------------------------------------------------------------" >Makefile.dep
	@echo "# Dependencies" >>Makefile.dep
	@echo "# (automatically generated using 'make depend')" >>Makefile.dep
	@echo "#------------------------------------------------------------------------------" >>Makefile.dep
	@echo >>Makefile.dep
	@$(MPICC) $(CFLAGS) $(FORTRANNAMES) -MM \
		$(patsubst %.o, %.c, $(sort $(filter %.o, $(PARTEST_OBJS) $(PARTEST_OMPI_OBJS) ))) \
	   > depend.tmp
	@cat depend.tmp \
	 | $(TOPDIR)/mf/canonicalize_depends.pl >>Makefile.dep
	@rm -f depend.tmp


dependheader:
	@echo "Generating/updating dependencies to system header"
	@echo "#------------------------------------------------------------------------------" >Header.dep
	@$(MPICC) $(CFLAGS) $(FORTRANNAMES) -M \
		$(patsubst %.o, %.c, $(sort $(filter %.o, $(PARTEST_OBJS) $(PARTEST_OMPI_OBJS)))) \
	    > header.tmp
	@cat header.tmp \
	 | $(TOPDIR)/mf/canonicalize_dependsheaders.pl | grep '^\/'  >>Header.dep
	@rm -f header.tmp

clean:
	rm -f *.o lib*.a $(USER_UTILS)

#------------------------------------------------------------------------------
# Dependencies
#------------------------------------------------------------------------------

-include Makefile.dep
