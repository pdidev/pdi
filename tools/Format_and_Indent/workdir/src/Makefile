
SRC_C=$(wildcard *.c)
SRC_CXX=$(wildcard *.cxx)
HDR=$(wildcard *.h)
OBJ_C=$(SRC_C:.c=.orig_c)
OBJ_CXX=$(SRC_CXX:.cxx=.orig_cxx)
OBJH=$(HDR:.h=.orig_h)

all: src_c.loc src_cxx.loc hdr.loc

src_c.loc: $(OBJ_C) 
	@touch "C_SRC_FILES_ARE_FORMATTED.loc"

src_cxx.loc: $(OBJ_CXX) 
	@touch "CXX_SRC_FILES_ARE_FORMATTED.loc"

hdr.loc: $(OBJH) 
	@touch "HEADER_FILES_ARE_FORMATTED.loc"


%.orig_c: %.c 
	@cp $< $@
	@astyle --options=.astylerc  < $@ > $<
	@touch $< && touch $@ # Hack, astyle change the last modify time of the file

%.orig_cxx: %.cxx 
	@cp $< $@
	@astyle --options=.astylerc  < $@ > $<
	@touch $< && touch $@ # Hack, astyle change the last modify time of the file

%.orig_h: %.h 
	@cp $< $@
	@astyle --options=.astylerc  < $@ > $<
	@touch $< && touch $@ # Hack (see before)

