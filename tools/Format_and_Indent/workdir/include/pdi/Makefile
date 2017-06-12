
HDR=$(wildcard *.h)
OBJH=$(HDR:.h=.orig_h)

HEADER_FILES_ARE_FORMATTED.loc: $(OBJH) 
	@touch "HEADER_FILES_ARE_FORMATTED.loc"

%.orig_h: %.h 
	@cp $< $@
	@astyle --options=.astylerc  < $@ > $<
	@touch $< && touch $@ # Hack (see before)

