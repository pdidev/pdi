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

CFLAGS += $(SION_PLATFORM) 
CFLAGS += $(SION_DEBUG) $(SION_PTHREADS) 
CFLAGS += $(SION_CUDA) $(CUDA_INCLUDE_PATHS)
CFLAGS += $(SION_SIONFWD) $(SIONFWD_CFLAGS)
CFLAGS += -I. 
CFLAGS += -I../parlib 
CFLAGS += -I../cppinterface

.SUFFIXES:  .cpp .c .f

SION_COMMON_OBJS = sion_common.o \
		   sion_fd.o \
		   sion_filedesc.o \
		   sion_file.o \
		   sion_metadata.o \
		   sion_tools.o \
		   sion_debug.o \
		   sion_error_handler.o \
		   sion_printts.o \
		   sion_internal.o \
		   sion_internal_seek.o \
		   sion_internal_collstat.o \
		   sion_internal_startptr.o \
		   sion_convert.o \
		   sion_buffer.o \
		   sion_hints.o \
		   sion_hints_linux.o \
		   sion_hints_gpfs.o \
	           sion_cache.o \
	           sion_dup.o \
		   sion_keyvalue.o \
		   sion_keyvalue_inline.o \
		   sion_keyvalue_table.o \
		   sion_keyvalue_keymngr.o \
	           sion_buddy_common.o \
		   sion_flags.o \

SION_COMMON_LOCK_PTHREADS_OBJS = sion_lock_pthreads.o 
SION_COMMON_LOCK_NONE_OBJS = sion_lock_none.o 
SION_COMMON_LOCK_OBJS = sion_lock.o 

SION_SER_OBJS = sion_serial.o

ifeq ($(CXXENABLE),1)
SION_CXX_HDR = sion_cxx.h
endif

USER_HDRS = sion.h sion_common.h sion_serial.h sion_const.h sion_datatypes.h sion_platform.h sion_debug.h $(SION_CXX_HDR)
USER_LIBS = lib$(SION_LIBNAME_SER).a lib$(SION_LIBNAME_COM).a lib$(SION_LIBNAME_COM_LOCK_PTHREADS).a lib$(SION_LIBNAME_COM_LOCK_NONE).a 


all: lib$(SION_LIBNAME_COM).a lib$(SION_LIBNAME_SER).a  lib$(SION_LIBNAME_COM_LOCK_PTHREADS).a lib$(SION_LIBNAME_COM_LOCK_NONE).a 

lib$(SION_LIBNAME_COM).a : $(SION_COMMON_OBJS)
	$(C_AR) $(AFLAG) -rcs $@ $(SION_COMMON_OBJS)
	@echo "*** $@ built"

lib$(SION_LIBNAME_COM_LOCK_PTHREADS).a : $(SION_COMMON_LOCK_PTHREADS_OBJS)
	$(C_AR) $(AFLAG) -rcs $@ $(SION_COMMON_LOCK_PTHREADS_OBJS)
	@echo "*** $@ built"

lib$(SION_LIBNAME_COM_LOCK_NONE).a : $(SION_COMMON_LOCK_NONE_OBJS)
	$(C_AR) $(AFLAG) -rcs $@ $(SION_COMMON_LOCK_NONE_OBJS)
	@echo "*** $@ built"

lib$(SION_LIBNAME_SER).a : $(SION_SER_OBJS)
	$(C_AR) $(AFLAG) -rcs $@ $(SION_SER_OBJS)
	@echo "*** $@ built"

sion_lock_pthreads.o: sion_lock.c
	$(CC) $(CFLAGS) -DSION_USE_PTHREADS -c sion_lock.c -o sion_lock_pthreads.o

sion_lock_none.o: sion_lock.c
	$(CC) $(CFLAGS) -c sion_lock.c -o sion_lock_none.o

sion_serial.o : sion_serial.c
	$(CC) $(CFLAGS) -c sion_serial.c
sion_fd.o: sion_fd.c
	$(CC) $(CFLAGS) -c sion_fd.c
sion_file.o: sion_file.c
	$(CC) $(CFLAGS) -c sion_file.c
sion_filedesc.o: sion_filedesc.c
	$(CC) $(CFLAGS) -c sion_filedesc.c
sion_metadata.o: sion_metadata.c
	$(CC) $(CFLAGS) -c sion_metadata.c
sion_tools.o: sion_tools.c
	$(CC) $(CFLAGS) -c sion_tools.c
sion_debug.o: sion_debug.c
	$(CC) $(CFLAGS) -c sion_debug.c
sion_internal_seek.o: sion_internal_seek.c
	$(CC) $(CFLAGS) -c sion_internal_seek.c
sion_internal_startptr.o: sion_internal_startptr.c
	$(CC) $(CFLAGS) -c sion_internal_startptr.c
sion_internal_collstat.o: sion_internal_collstat.c
	$(CC) $(CFLAGS) -c sion_internal_collstat.c
sion_dup.o: sion_dup.c
	$(CC) $(CFLAGS) -c sion_dup.c
sion_cache.o: sion_cache.c
	$(CC) $(CFLAGS) $(CACHEFLAGS) -c sion_cache.c
sion_printts.o: sion_printts.c 
	$(CC) $(CFLAGS) -c sion_printts.c
sion_internal.o: sion_internal.c
	$(CC) $(CFLAGS) -c sion_internal.c
sion_convert.o: sion_convert.c
	$(CC) $(CFLAGS) -c sion_convert.c
sion_keyvalue.o: sion_keyvalue.c
	$(CC) $(CFLAGS) -c sion_keyvalue.c
sion_keyvalue_inline.o: sion_keyvalue_inline.c
	$(CC) $(CFLAGS) -c sion_keyvalue_inline.c
sion_keyvalue_table.o: sion_keyvalue_table.c
	$(CC) $(CFLAGS) -c sion_keyvalue_table.c
sion_keyvalue_keymngr.o: sion_keyvalue_keymngr.c
	$(CC) $(CFLAGS) -c sion_keyvalue_keymngr.c
sion_hints.o: sion_hints.c 
	$(CC) $(CFLAGS) $(HINTSDEF) $(HINTSINC) -c sion_hints.c
sion_hints_linux.o: sion_hints_linux.c 
	$(CC) $(CFLAGS) $(HINTSDEF) $(HINTSINC) -c sion_hints_linux.c
sion_hints_gpfs.o: sion_hints_gpfs.c
	$(CC) $(CFLAGS) $(HINTSDEF) $(HINTSINC) -c sion_hints_gpfs.c


depend:
	@echo "Generating/updating dependencies"
	@echo "#------------------------------------------------------------------------------" >Makefile.dep
	@echo "# Dependencies" >>Makefile.dep
	@echo "# (automatically generated using 'make depend')" >>Makefile.dep
	@echo "#------------------------------------------------------------------------------" >>Makefile.dep
	@echo >>Makefile.dep
	@$(CXX) $(CFLAGS) $(FORTRANNAMES) -MM \
		$(patsubst %.o, %.c, $(sort $(filter %.o, $(SION_SER_OBJS) $(SION_COMMON_OBJS) $(SION_COMMON_LOCK_OBJS) ))) \
	   > depend.tmp
	@cat depend.tmp \
	 | $(TOPDIR)/mf/canonicalize_depends.pl >>Makefile.dep
	@rm -f depend.tmp


dependheader:
	@echo "Generating/updating dependencies to system header"
	@echo "#------------------------------------------------------------------------------" >Header.dep
	@$(CXX) $(CFLAGS) $(FORTRANNAMES) -M \
		$(patsubst %.o, %.c, $(sort $(filter %.o, $(SION_SER_OBJS) $(SION_COMMON_OBJS) $(SION_COMMON_LOCK_OBJS)  ))) \
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
