

# This makefile is provided to check indent and coding style.
# It can reindent and reformat the whole PDI project.
#

# Origin dir
DIR_SRC:= "../../src/"
DIR_HDR:= "../../src/"
DIR_INC:= "../../include/pdi/"
DIR_H  := "../../include/"

# Origin src
PDI_SRC:= $(shell find $(DIR_SRC) -maxdepth 1 -iname '*.c') $(shell find $(DIR_SRC) -maxdepth 1 -iname '*.cxx')
PDI_HDR:= $(shell find $(DIR_HDR) -maxdepth 1 -iname '*.h')
PDI_INC:= $(shell find $(DIR_INC) -maxdepth 1 -iname '*.h')
PDI_H  := $(shell find $(DIR_H)   -maxdepth 1 -iname '*.h')

# Local dir
LOC_SRC:= "workdir/src/"
LOC_HDR:= "workdir/src/"
LOC_INC:= "workdir/include/pdi/"
LOC_H:=   "workdir/include/"

# Local SRC
BACK_SRC:= $(shell find $(LOC_SRC) -maxdepth 1 -iname '*.c') $(shell find $(LOC_SRC) -maxdepth 1 -iname '*.cxx')
BACK_HDR:= $(shell find $(LOC_SRC) -maxdepth 1 -iname '*.h')
BACK_INC:= $(shell find $(LOC_INC) -maxdepth 1 -iname '*.h')
BACK_H:=   $(shell find $(LOC_H)   -maxdepth 1 -iname '*.h')

# Diff
DIFF_SRC:= $(shell find $(LOC_SRC) -maxdepth 1 -iname '*.diff')
DIFF_HDR:= $(shell find $(LOC_SRC) -maxdepth 1 -iname '*.diff')
DIFF_INC:= $(shell find $(LOC_INC) -maxdepth 1 -iname '*.diff')
DIFF_H:=   $(shell find $(LOC_H)   -maxdepth 1 -iname '*.diff')


all: test copy_style indent_all replace_all

test: 
	@which astyle 1> /dev/null || (echo "Program 'astyle' is not installed and is required. Abort." && exit 1)

copy_style:
	@cp PDI_Code_Formatting.astyle $(LOC_SRC).astylerc
	@cp PDI_Code_Formatting.astyle $(LOC_HDR).astylerc
	@cp PDI_Code_Formatting.astyle $(LOC_INC).astylerc
	@cp PDI_Code_Formatting.astyle $(LOC_H).astylerc


#############################################
#   Create new files that are reformatted   #
#############################################
# Check Formatting in all sub directories and create corrected files 
indent_all: loc_src.loc loc_hdr.loc loc_inc.loc loc_h.loc

loc_src.loc: $(PDI_SRC)
	@cp $(PDI_SRC) $(LOC_SRC) 
	@(cd $(LOC_SRC) && $(MAKE) src_c.loc)
	@(cd $(LOC_SRC) && $(MAKE) src_cxx.loc)
	@touch loc_src.loc

loc_hdr.loc: $(PDI_HDR)
	@cp $(PDI_HDR) $(LOC_HDR)
	@(cd $(LOC_HDR) && $(MAKE) hdr.loc)
	@touch loc_hdr.loc

loc_inc.loc: $(PDI_INC) 
	@cp $(PDI_INC) $(LOC_INC)
	@(cd $(LOC_INC) && $(MAKE))
	@touch loc_inc.loc

loc_h.loc: $(PDI_H)
	@cp $(PDI_H)   $(LOC_H)
	@(cd $(LOC_H) && $(MAKE))
	@touch loc_h.loc


#############################################
#             Replace old files             #
#############################################
# replace file in all directories
replace_all: replace_src.loc replace_inc.loc replace_h.loc 

replace_src.loc: $(BACK_SRC)
	@echo "--------------------------------"
	@echo "Checking repertory: ""$(DIR_SRC)"
	@./replace $(LOC_SRC) $(DIR_SRC)
	@touch "replace_src.loc"
	@echo "Done."
	@echo "--------------------------------"

replace_inc.loc: $(BACK_INC)
	@echo "Checking repertory: ""$(DIR_INC)"
	@./replace $(LOC_INC) $(DIR_INC)
	@touch "replace_inc.loc"
	@echo "Done."
	@echo "--------------------------------"

replace_h.loc: $(BACK_H)
	@echo "Checking repertory: ""$(DIR_H)"
	@./replace $(LOC_H) $(DIR_H)
	@touch "replace_h.loc"
	@echo "Done."
	@echo "--------------------------------"


#############################################
#         Clean all temporary files         #
#############################################

clean:
	@rm -fv *.loc 2> /dev/null
	@rm -fv $(LOC_SRC)/*.loc   2> /dev/null 
	@rm -fv $(LOC_INC)/*.loc   2> /dev/null
	@rm -fv $(LOC_H)/*.loc     2> /dev/null # remove lock
	@rm -fv $(LOC_SRC)/*.*_c   2> /dev/null 
	@rm -fv $(LOC_SRC)/*.*_cxx 2> /dev/null 
	@rm -fv $(LOC_HDR)/*.*_h   2> /dev/null
	@rm -fv $(LOC_INC)/*.*_h   2> /dev/null
	@rm -fv $(LOC_H)/*.*_h     2> /dev/null # remove copy
	@rm -fv $(LOC_SRC)/*.c     2> /dev/null 
	@rm -fv $(LOC_SRC)/*.cxx   2> /dev/null 
	@rm -fv $(LOC_HDR)/*.h     2> /dev/null
	@rm -fv $(LOC_INC)/*.h     2> /dev/null
	@rm -fv $(LOC_H)/*.h       2> /dev/null # remove src/header
