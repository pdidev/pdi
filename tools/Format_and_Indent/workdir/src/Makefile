
SRC=$(wildcard *.c)
HDR=$(wildcard *.h)
OBJ=$(SRC:.c=.orig_c)
OBJH=$(HDR:.h=.orig_h)

all: src.loc hdr.loc

src.loc: $(OBJ) 
	@touch "SRC_FILES_ARE_FORMATTED.loc"

hdr.loc: $(OBJH) 
	@touch "HEADER_FILES_ARE_FORMATTED.loc"


%.orig_c: %.c 
	@cp $< $@
	@astyle --options=.astylerc  < $@ > $<
	@touch $< && touch $@ # Hack, astyle change the last modify time of the file


%.orig_h: %.h 
	@cp $< $@
	@astyle --options=.astylerc  < $@ > $<
	@touch $< && touch $@ # Hack (see before)

