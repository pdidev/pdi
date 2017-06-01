

# This makefile is provided to check indent and coding style.
# It can reindent and reformat the whole PDI project.
#

# Origin dir
DIR_SRC:= "../../src/"
DIR_HDR:= "../../src/"
DIR_INC:= "../../include/pdi/"
DIR_H  := "../../include/"

# Origin src
PDI_SRC:= $(shell find $(DIR_SRC) -iname '*.c')
PDI_HDR:= $(shell find $(DIR_HDR) -iname '*.h')
PDI_INC:= $(shell find $(DIR_INC) -iname '*.h')
PDI_H  := $(shell find $(DIR_H)   -iname '*.h')

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

all: indent_all replace_all


#############################################
#   Create new files that are reformatted   #
#############################################
# Check Formatting in all sub directories and create corrected files 
indent_all: loc_src.loc loc_inc.loc loc_h.loc

loc_src.loc: $(PDI_SRC)
	@cp $(PDI_SRC) $(LOC_SRC) 
	@(cd $(LOC_SRC) && $(MAKE))
	@touch loc_src.loc
	@rm replace_src.loc

#useless, done by upper make
# loc_hdr: $(PDI_HDR)
# 	@cp $(PDI_HDR) $(LOC_HDR)
# 	@(cd $(LOC_HDR) && $(MAKE))
# 	@touch loc_hdr

loc_inc.loc: $(PDI_INC) 
	@cp $(PDI_INC) $(LOC_INC)
	@(cd $(LOC_INC) && $(MAKE))
	@touch loc_inc.loc
	@rm replace_inc.loc

loc_h.loc: $(PDI_H)
	@cp $(PDI_H)   $(LOC_H)
	@(cd $(LOC_H) && $(MAKE))
	@touch loc_h.loc
	@rm replace_h.loc


#############################################
#             Replace old files             #
#############################################
# replace file in all directories
replace_all: replace_src.loc replace_inc.loc replace_h.loc 

replace_src.loc:
	@touch "replace_src.loc"
	./replace $(LOC_SRC) $(DIR_SRC)

replace_inc.loc:
	@touch "replace_inc.loc"
	./replace $(LOC_INC) $(DIR_INC)

replace_h.loc:
	@touch "replace_h.loc"
	./replace $(LOC_H) $(DIR_H)


#############################################
#         Clean all temporary files         #
#############################################

clean:
	@rm -fv *.loc 2> /dev/null
	@rm -fv $(LOC_SRC)/*.loc 2> /dev/null 
	@rm -fv $(LOC_INC)/*.loc 2> /dev/null
	@rm -fv $(LOC_H)/*.loc   2> /dev/null # remove lock
	@rm -fv $(LOC_SRC)/*.*_c 2> /dev/null 
	@rm -fv $(LOC_HDR)/*.*_h 2> /dev/null
	@rm -fv $(LOC_INC)/*.*_h 2> /dev/null
	@rm -fv $(LOC_H)/*.*_h   2> /dev/null # remove copy
	@rm -fv $(LOC_SRC)/*.c   2> /dev/null 
	@rm -fv $(LOC_HDR)/*.h   2> /dev/null
	@rm -fv $(LOC_INC)/*.h   2> /dev/null
	@rm -fv $(LOC_H)/*.h     2> /dev/null # remove src/header
