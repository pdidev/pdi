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

CXXFLAGS += $(SION_DEBUG) $(SION_PTHREADS)
CXXFLAGS += -I. -I../lib -I../parlib
CXXFLAGS += $(SION_PLATFORM) 

CMPIFLAGS = $(MPIMFLAG)
COMPFLAGS = $(OMPMFLAG) $(OMPFLAG)
CHYBFLAGS = $(HYBMFLAG) $(HYBFLAG)

.SUFFIXES:  .cpp

SION_HDRS = ../parlib/sion_datatypes_mpi.h ../parlib/sion_mpi.h ../parlib/sion_ompi.h ../parlib/sion_omp.h ../lib/sion.h

ifeq ($(CXXENABLE),1)

USER_HDRS = sion_cxx_base.hpp sion_cxx_serial.hpp sion_cxx_common.hpp sion_cxx_mpi.hpp sion_cxx_ompi.hpp  sion_cxx_omp.hpp 
USER_LIBS = lib$(SION_LIBNAME_CXX_SER).a lib$(SION_LIBNAME_CXX_MPI).a
else
USER_HDRS = 
USER_LIBS = 
endif


SION_OBJS_SER = sion_cxx_base.o
SION_OBJS_SER += sion_cxx_serial.o
SION_OBJS_SER += sion_cxx_common.o

SION_OBJS_MPI =

#ifeq ($(OMPENABLE),1)
#  SION_OBJS += sion_cxx_omp.o
#endif

ifeq ($(MPIENABLE),1)
  SION_OBJS_MPI += sion_cxx_mpi.o
endif

#ifeq ($(HYBENABLE),1)
#  SION_OBJS += sion_cxx_ompi.o 
#endif   

all: $(USER_LIBS)


lib$(SION_LIBNAME_CXX_SER).a : $(SION_OBJS_SER)
	$(C_AR) $(AFLAG) -rcs $@ $(SION_OBJS_SER)
	@echo "*** $@ built"

lib$(SION_LIBNAME_CXX_MPI).a : $(SION_OBJS_MPI)
	$(C_AR) $(AFLAG) -rcs $@ $(SION_OBJS_MPI)
	@echo "*** $@ built"

sion_cxx_base.o :
	$(CXX) $(CXXFLAGS) -c sion_cxx_base.cpp

sion_cxx_serial.o :
	$(CXX) $(CXXFLAGS) -c sion_cxx_serial.cpp

sion_cxx_common.o :
	$(CXX) $(CXXFLAGS) -c sion_cxx_common.cpp

#sion_cxx_common.o : 
#	$(CXX) $(CXXFLAGS) -c sion_cxx_common.cpp

sion_cxx_mpi.o : 
	$(MPICXX) $(CMPIFLAGS) $(CXXFLAGS) -c sion_cxx_mpi.cpp

depend:
	@echo "Generating/updating dependencies"
	@echo "#------------------------------------------------------------------------------" >Makefile.dep
	@echo "# Dependencies" >>Makefile.dep
	@echo "# (automatically generated using 'make depend')" >>Makefile.dep
	@echo "#------------------------------------------------------------------------------" >>Makefile.dep
	@echo >>Makefile.dep
	@$(MPICXX) $(CXXFLAGS) -MM \
		$(patsubst %.o, %.cpp, $(sort $(filter %.o, $(SION_OBJS_SER) $(SION_OBJS_MPI)))) \
	   > depend.tmp
	@cat depend.tmp \
	 | $(TOPDIR)/mf/canonicalize_depends.pl >>Makefile.dep
	@rm -f depend.tmp

dependheader:
	@echo "Generating/updating dependencies to system header"
	@echo "#------------------------------------------------------------------------------" >Header.dep
	@$(MPICXX) $(CXXFLAGS) -M \
		$(patsubst %.o, %.cpp, $(sort $(filter %.o, $(SION_OBJS_SER) $(SION_OBJS_MPI)))) \
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
	rm -f *.o lib*.a

#------------------------------------------------------------------------------
# Dependencies
#------------------------------------------------------------------------------

-include Makefile.dep

