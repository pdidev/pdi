

# This makefile is provided to check indent and coding style.
# It can reindent and reformat the whole PDI project.
#

# Origin
PDI_SRC:= $(wildcard ../../src/*.c)
PDI_HDR:= $(wildcard ../../src/*.h)
PDI_INC:= $(wildcard ../../include/pdi/*.h)
PDI_H  := $(wildcard ../../include/*.h)

# Local dir
LOC_SRC:= ".workdir/src/"
LOC_HDR:= ".workdir/src/"
LOC_INC:= ".workdir/include/pdi/"
LOC_H:=   ".workdir/include/"

# Local SRC
BACK_SRC:= $(shell find $(LOC_SRC) -iname '*.c')
BACK_HDR:= $(shell find $(LOC_SRC) -iname '*.h')
BACK_INC:= $(shell find $(LOC_INC) -iname '*.h')
BACK_H:=   $(shell find $(LOC_H)   -iname '*.h')


# Check if file does not exist copy 
ifeq ("$(BACK_SRC)","")
COPY_ALL:
	@cp PDI_Code_Formatting.astyle $(LOC_SRC).astylerc
	@cp PDI_Code_Formatting.astyle $(LOC_HDR).astylerc
	@cp PDI_Code_Formatting.astyle $(LOC_INC).astylerc
	@cp PDI_Code_Formatting.astyle $(LOC_H).astylerc
endif

all: loc_src.loc loc_inc.loc loc_h.loc

# Check Formatting in all sub directories
loc_src.loc: $(PDI_SRC)
	@cp $(PDI_SRC) $(LOC_SRC) 
	@(cd $(LOC_SRC) && $(MAKE))
	@touch loc_src.loc

# loc_hdr: $(PDI_HDR)
# 	@cp $(PDI_HDR) $(LOC_HDR)
# 	@(cd $(LOC_HDR) && $(MAKE))
# 	@touch loc_hdr

loc_inc.loc: $(PDI_INC) 
	@cp $(PDI_INC) $(LOC_INC)
	@(cd $(LOC_INC) && $(MAKE))
	@touch loc_inc.loc

loc_h.loc: $(PDI_H)
	@cp $(PDI_H)   $(LOC_H)
	@(cd $(LOC_H) && $(MAKE))
	@touch loc_h.loc



clean:
	rm *.loc
	rm $(LOC_SRC)/*.*_c 
	rm $(LOC_HDR)/*.*_h
	rm $(LOC_INC)/*.*_h
	rm $(LOC_H)/*.*_h
	rm $(LOC_SRC)/*.c 
	rm $(LOC_HDR)/*.h
	rm $(LOC_INC)/*.h
	rm $(LOC_H)/*.h

