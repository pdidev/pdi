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

F77FLAGS = $(FFLAGS)
F77FLAGS_CPP = $(SION_DEBUG) $(SION_PTHREADS)
F77FLAGS_CPP += -I. -I../lib -I../parlib
F77FLAGS_CPP += $(SION_PLATFORM)

F90FLAGS += $(SION_DEBUG) $(SION_PTHREADS)
F90FLAGS_CPP = -I. -I../lib -I../parlib
F90FLAGS_CPP += $(SION_PLATFORM)


CFLAGS += -I. -I../lib -I../parlib
CFLAGS += $(SION_DEBUG) 
CMPIFLAGS = $(MPIMFLAG)
COMPFLAGS = $(OMPMFLAG) $(OMPFLAG)
CHYBFLAGS = $(HYBMFLAG) $(HYBFLAG)

.SUFFIXES:  .cpp

SION_HDRS = ../parlib/sion_datatypes_mpi.h ../parlib/sion_mpi.h ../parlib/sion_ompi.h ../parlib/sion_omp.h ../lib/sion.h

ifeq ($(FORTRANENABLE),1)

USER_HDRS = sion_f77.H sion_f77.h 
USER_MODS = sion_f90.mod
USER_LIBS = lib$(SION_LIBNAME_F77_SER).a lib$(SION_LIBNAME_F77_MPI).a lib$(SION_LIBNAME_F90_SER).a lib$(SION_LIBNAME_F90_MPI).a
else
USER_HDRS = 
USER_MODS =
USER_LIBS = 
endif


SION_OBJS_F77_SER = sion_f77.o sion_fortran.o sion_fortran_writeread_f77.o
SION_OBJS_F77_MPI =
SION_OBJS_F90_SER = sion_f90.o sion_fortran.o sion_fortran_writeread_f90.o
SION_OBJS_F90_MPI =

ifeq ($(OMPENABLE),1)
  SION_OBJS_F77_SER += sion_f77_omp.o sion_fortran_omp.o
  SION_OBJS_F90_SER += sion_f90_omp.o sion_fortran_omp.o
  USER_MODS += sion_f90_omp.mod
endif

ifeq ($(MPIENABLE),1)
  SION_OBJS_F77_MPI += sion_f77_mpi.o sion_fortran_mpi.o sion_fortran_writeread_mpi_f77.o
  SION_OBJS_F90_MPI += sion_f90_mpi.o sion_fortran_mpi.o sion_fortran_writeread_mpi_f90.o
  USER_MODS += sion_f90_mpi.mod
endif

ifeq ($(HYBENABLE),1)
  SION_OBJS_F77_MPI += sion_f77_ompi.o sion_fortran_ompi.o
  SION_OBJS_F90_MPI += sion_f90_ompi.o sion_fortran_ompi.o
  USER_MODS += sion_f90_ompi.mod
endif   

SION_DEPEND_OBJS=sion_fortran.o sion_fortran_omp.o sion_fortran_mpi.o sion_fortran_ompi.o sion_fortran_writeread_f77.o sion_fortran_writeread_f90.o  sion_fortran_writeread_mpi_f77.o sion_fortran_writeread_mpi_f90.o 

all: $(USER_LIBS) $(USER_MODS)


lib$(SION_LIBNAME_F77_SER).a : $(SION_OBJS_F77_SER)
	$(C_AR) $(AFLAG) -rcs $@ $(SION_OBJS_F77_SER)
	@echo "*** $@ built"

lib$(SION_LIBNAME_F77_MPI).a : $(SION_OBJS_F77_MPI)
	$(C_AR) $(AFLAG) -rcs $@ $(SION_OBJS_F77_MPI)
	@echo "*** $@ built"

lib$(SION_LIBNAME_F90_SER).a : $(SION_OBJS_F90_SER)
	$(C_AR) $(AFLAG) -rcs $@ $(SION_OBJS_F90_SER)
	@echo "*** $@ built"

lib$(SION_LIBNAME_F90_MPI).a : $(SION_OBJS_F90_MPI)
	$(C_AR) $(AFLAG) -rcs $@ $(SION_OBJS_F90_MPI)
	@echo "*** $@ built"

# F77 interface
sion_f77.h: sion_f77.H
	$(CPP) $(F77FLAGS) $(F77FLAGS_CPP) -P -E -I ../lib sion_f77.H > sion_f77.h
sion_f77.o : sion_f77.f sion_f77.h $(SION_HDRS)
	$(F77) $(F77FLAGS) -c sion_f77.f
sion_f77_mpi.o : sion_f77_mpi.f sion_f77.H $(SION_HDRS)
	$(MPIF77) $(F77FLAGS) -c sion_f77_mpi.f
sion_f77_ompi.o : sion_f77_ompi.f sion_f77.H $(SION_HDRS)
	$(MPIF77) $(F77FLAGS) -c sion_f77_ompi.f
sion_f77_omp.o : sion_f77_omp.f sion_f77.H $(SION_HDRS)
	$(OMPF77) $(F77FLAGS) -c sion_f77_omp.f


# F90 Interface (serial)
sion_f90.f90: sion_f90.F90 $(SION_HDRS)
	$(CPP) $(F90FLAGS) $(F90FLAGS_CPP) -P -E -I ../lib sion_f90.F90 > sion_f90.f90
sion_f90.mod : sion_f90.o
sion_f90.o : sion_f90.f90 sion_f77.h $(SION_HDRS)
	$(F90) -c sion_f90.f90

sion_f90_omp.o: sion_f90.o
sion_f90_mpi.o: sion_f90.o
sion_f90_ompi.o: sion_f90.o


# F90 Interface (MPI)
sion_f90_mpi.f90: sion_f90_mpi.F90
	$(CPP) $(F90FLAGS) $(F90FLAGS_CPP) $(F90MPIFLAGS) -P -E -I ../lib sion_f90_mpi.F90 > sion_f90_mpi.f90
sion_f90_mpi.mod : sion_f90_mpi.o
sion_f90_mpi.o : sion_f90_mpi.f90 sion_f90.mod $(SION_HDRS)
	$(MPIF90) -c sion_f90_mpi.f90

# F90 Interface (OMP)
sion_f90_omp.f90: sion_f90_omp.F90
	$(CPP) $(F90OMPFLAGS) $(F90FLAGS) $(F90FLAGS_CPP) -P -E -I ../lib sion_f90_omp.F90 > sion_f90_omp.f90
sion_f90_omp.mod : sion_f90_omp.o 
sion_f90_omp.o : sion_f90_omp.f90 sion_f90.mod $(SION_HDRS)
	$(OMPF90) -c sion_f90_omp.f90

# F90 Interface (OMPI)
sion_f90_ompi.f90: sion_f90_ompi.F90 $(SION_HDRS)
	$(CPP) $(F90MPIFLAGS) $(F90OMPFLAGS) $(F90FLAGS) $(F90FLAGS_CPP) -P -E -I ../lib sion_f90_ompi.F90 > sion_f90_ompi.f90
sion_f90_ompi.mod : sion_f90_ompi.o
sion_f90_ompi.o : sion_f90_ompi.f90 $(SION_HDRS)
	$(MPIF90) -c sion_f90_ompi.f90



sion_fortran.o: 
	$(CC) $(CFLAGS) $(FORTRANNAMES) -c sion_fortran.c

sion_fortran_writeread_f77.o: 
	$(CC) $(CFLAGS) $(FORTRANNAMES) -c sion_fortran_writeread_f77.c

sion_fortran_writeread_f90.o: 
	$(CC) $(CFLAGS) $(FORTRANNAMES) -c sion_fortran_writeread_f90.c

sion_fortran_mpi.o : sion_fortran_mpi.c 
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) $(FORTRANNAMES) -c sion_fortran_mpi.c

sion_fortran_writeread_mpi_f77.o : sion_fortran_writeread_mpi_f77.c 
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) $(FORTRANNAMES) -c sion_fortran_writeread_mpi_f77.c

sion_fortran_writeread_mpi_f90.o : sion_fortran_writeread_mpi_f90.c 
	$(MPICC) $(CMPIFLAGS) $(CFLAGS) $(FORTRANNAMES) -c sion_fortran_writeread_mpi_f90.c

sion_fortran_ompi.o : sion_fortran_ompi.c
	$(HYBCC) $(CHYBFLAGS) $(CFLAGS) $(FORTRANNAMES) -c sion_fortran_ompi.c

sion_fortran_omp.o : sion_fortran_omp.c 
	$(OMPCC) $(COMPFLAGS) $(CFLAGS) $(FORTRANNAMES) -c sion_fortran_omp.c


depend:
	@echo "Generating/updating dependencies"
	@echo "#------------------------------------------------------------------------------" >Makefile.dep
	@echo "# Dependencies" >>Makefile.dep
	@echo "# (automatically generated using 'make depend')" >>Makefile.dep
	@echo "#------------------------------------------------------------------------------" >>Makefile.dep
	@echo >>Makefile.dep
	@$(MPICC) $(CMPIFLAGS) $(CFLAGS) $(FORTRANNAMES) -MM \
		$(patsubst %.o, %.c, $(sort $(filter %.o, $(SION_DEPEND_OBJS)))) \
	   > depend.tmp
	@cat depend.tmp \
	 | $(TOPDIR)/mf/canonicalize_depends.pl >>Makefile.dep
	@rm -f depend.tmp

dependheader:
	@echo "Generating/updating dependencies to system header"
	@echo "#------------------------------------------------------------------------------" >Header.dep
	@$(MPICXX) $(CMPIFLAGS) $(CFLAGS) $(FORTRANNAMES) -M \
		$(patsubst %.o, %.c, $(sort $(filter %.o, $(SION_DEPEND_OBJS)))) \
	    > header.tmp
	@cat header.tmp \
	 | $(TOPDIR)/mf/canonicalize_dependsheaders.pl | grep '^\/'  >>Header.dep
	@rm -f header.tmp

install: all
	$(MKDIR) -m 755 $(INCDIR)
	$(MKDIR) -m 755 $(INCDIR)/$(SION_MODPATH)
	@list='$(USER_HDRS)'; for p in $$list; do \
		echo "$(INSTALL) -c -m 644 $$p $(INCDIR)" ; \
		eval "$(INSTALL) -c -m 644 $$p $(INCDIR)" ; \
	done
	@list='$(USER_MODS)'; for p in $$list; do \
		echo "$(INSTALL) -c -m 644 $$p $(INCDIR)/$(SION_MODPATH)" ; \
		eval "$(INSTALL) -c -m 644 $$p $(INCDIR)/$(SION_MODPATH)" ; \
	done
	$(MKDIR) -m 755 $(LIBDIR)
	@list='$(USER_LIBS)'; for p in $$list; do \
		echo "$(INSTALL) -c -m 644 $$p $(LIBDIR)" ; \
		eval "$(INSTALL) -c -m 644 $$p $(LIBDIR)" ; \
	done

clean:
	rm -f *.o lib*.a *.mod

#------------------------------------------------------------------------------
# Dependencies
#------------------------------------------------------------------------------

-include Makefile.dep


