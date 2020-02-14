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
CFLAGS += -I. -I../lib -I../parlib -I../cppinterface 
CFLAGS += $(SION_PLATFORM) 
CFLAGS += $(SION_CUDA) $(CUDA_INCLUDE_PATHS)
LDFLAGS += -L../lib -l$(SION_LIBNAME_SER) -l$(SION_LIBNAME_COM) -l$(SION_LIBNAME_COM_LOCK_NONE) $(HINTSLIB) $(CACHELIB)
LDFLAGS += $(CUDA_LIBRARY_PATHS) $(CUDA_LIBRARIES)
LDFLAGS += $(SIONFWD_LIBS)

CFLAGS += -DGIT_REV='"$(GIT_REV)"'

.SUFFIXES:  .cpp .c .f

USER_HDRS = 
USER_LIBS = 
USER_UTILS = siondump siondefrag sionsplit sioncat sionversion

SION_OBJS = siondump.o siondefrag.o sionsplit.o sioncat.o sionversion.o

all: $(USER_UTILS)


siondump.o : siondump.c 
	$(CC) $(CFLAGS) -c siondump.c

siondefrag.o : siondefrag.c ../lib/lib$(SION_LIBNAME_SER).a 
	$(CC) $(CFLAGS) -c siondefrag.c

sionsplit.o : sionsplit.c ../lib/lib$(SION_LIBNAME_SER).a  
	$(CC) $(CFLAGS) -c sionsplit.c

sioncat.o : sioncat.c ../lib/lib$(SION_LIBNAME_SER).a  
	$(CC) $(CFLAGS) -c sioncat.c

sionversion.o : sionversion.c ../lib/lib$(SION_LIBNAME_SER).a  
	$(CC) $(CFLAGS) -c sionversion.c

siondump: siondump.o ../lib/lib$(SION_LIBNAME_SER).a  ../lib/lib$(SION_LIBNAME_COM).a  ../lib/lib$(SION_LIBNAME_COM_LOCK_NONE).a
	$(CC)  -o $@ siondump.o $(LDFLAGS)
	cp siondump $(TOPDIR)/..

siondefrag: siondefrag.o ../lib/lib$(SION_LIBNAME_SER).a  ../lib/lib$(SION_LIBNAME_COM).a  ../lib/lib$(SION_LIBNAME_COM_LOCK_NONE).a
	$(CC)  -o $@ siondefrag.o $(LDFLAGS)

sionsplit: sionsplit.o ../lib/lib$(SION_LIBNAME_SER).a  ../lib/lib$(SION_LIBNAME_COM).a  ../lib/lib$(SION_LIBNAME_COM_LOCK_NONE).a
	$(CC)  -o $@ sionsplit.o $(LDFLAGS)

sioncat: sioncat.o ../lib/lib$(SION_LIBNAME_SER).a  ../lib/lib$(SION_LIBNAME_COM).a  ../lib/lib$(SION_LIBNAME_COM_LOCK_NONE).a
	$(CC)  -o $@ sioncat.o $(LDFLAGS)

sionversion: sionversion.o ../lib/lib$(SION_LIBNAME_SER).a  ../lib/lib$(SION_LIBNAME_COM).a  ../lib/lib$(SION_LIBNAME_COM_LOCK_NONE).a
	$(CC)  -o $@ sionversion.o $(LDFLAGS)


###### sionconfig ######
install-config: sionconfig.in
	AINSTDIR=`(cd $(PREFIX) && pwd)`; \
	sed -e "s!@ARCH@!$(ARCH)!" \
		-e "s!@INSTDIR@!$$AINSTDIR!" \
		-e "s!@LIBNAME@!$(SION_LIBNAME_PAR)!" \
		-e "s!@LIBSERNAME@!$(SION_LIBNAME_SER)!" \
		-e "s!@HINTSLIB@!$(HINTSLIB)!" \
		-e "s!@CACHELIB@!$(CACHELIB)!" \
		-e "s!@PTHREADSUPPORT@!$(PTHREADSUPPORT)!" \
		-e "s!@CUDA_LIBRARIES@!$(CUDA_LIBRARIES)!" \
		-e "s!@CUDA_LIBRARY_PATHS@!$(CUDA_LIBRARY_PATHS)!" \
		-e "s!@SIONFWD_LIBS@!$(SIONFWD_LIBS)!" \
		sionconfig.in > $(PREFIX)/bin/sionconfig
	chmod 755 $(PREFIX)/bin/sionconfig; \


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
	@$(CXX) $(CFLAGS) $(FORTRANNAMES) -MM \
		$(patsubst %.o, %.c, $(sort $(filter %.o, $(SION_OBJS)))) \
	   > depend.tmp
	@cat depend.tmp \
	 | $(TOPDIR)/mf/canonicalize_depends.pl >>Makefile.dep
	@rm -f depend.tmp


dependheader:
	@echo "Generating/updating dependencies to system header"
	@echo "#------------------------------------------------------------------------------" >Header.dep
	@$(CXX) $(CFLAGS) $(FORTRANNAMES) -M \
		$(patsubst %.o, %.c, $(sort $(filter %.o, $(SION_OBJS)))) \
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
