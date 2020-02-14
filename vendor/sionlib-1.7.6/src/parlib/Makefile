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

CFLAGS += $(SION_DEBUG) $(SION_PTHREADS)
CFLAGS += -I. -I../lib
CFLAGS += $(SION_PLATFORM) 
CFLAGS += $(SION_MSA)
CFLAGS += $(SION_CUDA) $(CUDA_INCLUDE_PATHS)

CMPIFLAGS = $(MPIMFLAG)
COMPFLAGS = $(OMPMFLAG) $(OMPFLAG)
CHYBFLAGS = $(HYBMFLAG) $(HYBFLAG)

.SUFFIXES:  .cpp .c .f

USER_HDRS = sion_datatypes_mpi.h sion_mpi.h sion_ompi.h sion_omp.h sion_generic.h 
USER_LIBS = lib$(SION_LIBNAME_GEN).a

SION_OBJS_GEN = sion_generic.o \
	sion_generic_internal.o \
	sion_generic_mapped.o \
	sion_generic_collective.o \
	sion_generic_collective_merge.o \
	sion_generic_buddy.o \
	sion_generic_apidesc.o 

SION_OBJS_INTERNAL = sion_lock.o 

ifeq ($(OMPENABLE),1)
  SION_OBJS_OMP = sion_omp_gen.o \
	          sion_omp_cb_gen.o \
	          sion_omp_internal_gen.o 
  USER_HDRS += sion_omp.h 
  USER_LIBS += lib$(SION_LIBNAME_OMP).a 
endif

ifeq ($(MPIENABLE),1)
  SION_OBJS_MPI = sion_mpi_gen.o \
	          sion_mpi_deprecated.o \
	          sion_mpi_cb_gen.o \
	          sion_mpi_coll_cb_gen.o \
	          sion_mpi_internal_gen.o \
	          sion_mpi_util.o  \
		  sion_mpi_file_check.o
  USER_HDRS += sion_datatypes_mpi.h sion_mpi.h 
  USER_LIBS += lib$(SION_LIBNAME_MPI).a 
endif
ifeq ($(HYBENABLE),1)
  SION_OBJS_HYB = sion_ompi_gen.o \
                  sion_ompi_cb_gen.o \
	          sion_ompi_coll_cb_gen.o \
	          sion_ompi_util.o \
                  sion_ompi_internal_gen.o 
  USER_HDRS += sion_datatypes_mpi.h sion_ompi.h
  USER_LIBS += lib$(SION_LIBNAME_OMPI).a 
endif   

ifeq ($(HYBENABLE),1)
  LOCKCC    =$(HYBCC)
  LOCKCFLAGS=$(CHYBFLAGS)
else 
ifeq ($(OMPENABLE),1)
  LOCKCC    =$(OMPCC)
  LOCKCFLAGS=$(COMPFLAGS)
else
  LOCKCC    =$(CC)
  LOCKCFLAGS=$(CFLAGS)
endif
endif

SION_OBJS = $(SION_OBJS_GEN) $(SION_OBJS_INTERNAL) $(SION_OBJS_OMP) $(SION_OBJS_MPI) $(SION_OBJS_HYB)

all: $(USER_LIBS)

lib$(SION_LIBNAME_GEN).a : $(SION_OBJS_GEN) $(SION_OBJS_INTERNAL)
	$(C_AR) $(AFLAG) -rcs $@ $(SION_OBJS_GEN) $(SION_OBJS_INTERNAL)
	@echo "*** $@ built"

lib$(SION_LIBNAME_OMP).a : $(SION_OBJS_OMP) $(SION_OBJS_INTERNAL)
	$(C_AR) $(AFLAG) -rcs $@ $(SION_OBJS_OMP) $(SION_OBJS_INTERNAL)
	@echo "*** $@ built"

lib$(SION_LIBNAME_MPI).a : $(SION_OBJS_MPI) $(SION_OBJS_INTERNAL)
	$(C_AR) $(AFLAG) -rcs $@ $(SION_OBJS_MPI) $(SION_OBJS_INTERNAL)
	@echo "*** $@ built"

lib$(SION_LIBNAME_OMPI).a : $(SION_OBJS_HYB) $(SION_OBJS_INTERNAL)
	$(C_AR) $(AFLAG) -rcs $@ $(SION_OBJS_HYB) $(SION_OBJS_INTERNAL)
	@echo "*** $@ built"

sion_generic.o : sion_generic.c
	$(CC) $(CFLAGS) -c sion_generic.c

sion_generic_internal.o : sion_generic_internal.c
	$(CC) $(CFLAGS) -c sion_generic_internal.c

sion_generic_apidesc.o : sion_generic_apidesc.c
	$(CC) $(CFLAGS) -c sion_generic_apidesc.c

sion_generic_mapped.o : sion_generic_mapped.c
	$(CC) $(CFLAGS) -c sion_generic_mapped.c

sion_generic_collective.o : sion_generic_collective.c
	$(CC) $(CFLAGS) -c sion_generic_collective.c

sion_generic_collective_merge.o : sion_generic_collective_merge.c
	$(CC) $(CFLAGS) -c sion_generic_collective_merge.c

sion_generic_buddy.o : sion_generic_buddy.c
	$(CC) $(CFLAGS) -c sion_generic_buddy.c

sion_lock.o : sion_lock.c
	$(LOCKCC) $(LOCKCFLAGS) $(CFLAGS) -c sion_lock.c


sion_ompi_gen.o : sion_ompi_gen.c
	$(HYBCC) $(CHYBFLAGS) $(CFLAGS) -c sion_ompi_gen.c

sion_ompi_cb_gen.o : sion_ompi_cb_gen.c
	$(HYBCC) $(CHYBFLAGS) $(CFLAGS) -c sion_ompi_cb_gen.c

sion_ompi_coll_cb_gen.o : sion_ompi_coll_cb_gen.c
	$(HYBCC) $(CHYBFLAGS) $(CFLAGS) -c sion_ompi_coll_cb_gen.c

sion_ompi.o : sion_ompi.c
	$(HYBCC) $(CHYBFLAGS) $(CFLAGS) -c sion_ompi.c

sion_ompi_cb.o : sion_ompi_cb.c
	$(HYBCC) $(CHYBFLAGS) $(CFLAGS) -c sion_ompi_cb.c

sion_ompi_internal_gen.o : sion_ompi_internal_gen.c
	$(HYBCC) $(CHYBFLAGS)  $(CFLAGS) -c sion_ompi_internal_gen.c


sion_omp_gen.o : sion_omp_gen.c
	$(OMPCC) $(COMPFLAGS) $(CFLAGS) -c sion_omp_gen.c

sion_omp_cb_gen.o : sion_omp_cb_gen.c
	$(OMPCC) $(COMPFLAGS) $(CFLAGS) -c sion_omp_cb_gen.c

sion_omp_internal_gen.o : sion_omp_internal_gen.c
	$(OMPCC) $(COMPFLAGS) $(CFLAGS) -c sion_omp_internal_gen.c


sion_mpi_gen.o : sion_mpi_gen.c
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) -c sion_mpi_gen.c

sion_mpi_cb_gen.o : sion_mpi_cb_gen.c
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) -c sion_mpi_cb_gen.c

sion_mpi_coll_cb_gen.o : sion_mpi_coll_cb_gen.c
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) -c sion_mpi_coll_cb_gen.c

sion_mpi_deprecated.o : sion_mpi_deprecated.c
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) -c sion_mpi_deprecated.c

sion_mpi_file_check.o : sion_mpi_file_check.c
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) -c sion_mpi_file_check.c


sion_mpi.o : sion_mpi.c
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) -c sion_mpi.c

sion_mpi_cb.o : sion_mpi_cb.c
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) -c sion_mpi_cb.c

sion_mpi_coll.o : sion_mpi_coll.c
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) -c sion_mpi_coll.c

sion_mpi_internal_gen.o : sion_mpi_internal_gen.c
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) -c sion_mpi_internal_gen.c


depend:
	@echo "Generating/updating dependencies"
	@echo "#------------------------------------------------------------------------------" >Makefile.dep
	@echo "# Dependencies" >>Makefile.dep
	@echo "# (automatically generated using 'make depend')" >>Makefile.dep
	@echo "#------------------------------------------------------------------------------" >>Makefile.dep
	@echo >>Makefile.dep
	@$(MPICC) $(CFLAGS) $(CMPIFLAGS) $(COMPFLAGS) $(CHYBFLAGS) $(FORTRANNAMES) -MM \
		$(patsubst %.o, %.c, $(sort $(filter %.o, $(SION_OBJS)))) \
	   > depend.tmp
	@cat depend.tmp \
	 | $(TOPDIR)/mf/canonicalize_depends.pl >>Makefile.dep
	@rm -f depend.tmp


dependheader:
	@echo "Generating/updating dependencies to system header"
	@echo "#------------------------------------------------------------------------------" >Header.dep
	@$(MPICC) $(CFLAGS) $(FORTRANNAMES) -M \
		$(patsubst %.o, %.c, $(sort $(filter %.o, $(SION_OBJS)))) \
	    > header.tmp
	@cat header.tmp \
	 | $(TOPDIR)/mf/canonicalize_dependsheaders.pl | grep '^\/'  >>Header.dep
	@rm -f header.tmp

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

clean:
	rm -f *.o lib*.a opari*

#------------------------------------------------------------------------------
# Dependencies
#------------------------------------------------------------------------------

-include Makefile.dep
