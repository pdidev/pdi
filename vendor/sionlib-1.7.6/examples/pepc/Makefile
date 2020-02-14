
ifeq ($(HOST), jugene1)
	MAKEFILE=Makefile_BE
else 
ifeq ($(HOST), jugene2)
	MAKEFILE=Makefile_BE
else 
ifeq ($(HOSTTYPE), i386)
	MAKEFILE=Makefile_LINUX
else
ifeq ($(HOST), zam219)
	MAKEFILE=Makefile_LINUX	
else
ifeq ($(HOST),)
	MAKEFILE=Makefile_PWR6
endif
endif
endif
endif
endif

all:
	@echo Building for $(HOST)
	$(MAKE) -f $(MAKEFILE) all install
ifeq ($(HOST), jugene1)
	$(MAKE) -f Makefile_FE clean all install
	$(MAKE) -f Makefile_FEgcc clean all install
else 
ifeq ($(HOST), jugene2)
	$(MAKE) -f Makefile_FE clean all install
	$(MAKE) -f Makefile_FEgcc clean all install
endif
endif
install:
	@echo Installing for $(HOST)
	$(MAKE) -f $(MAKEFILE) install
	
clean:
	@echo Cleaning
	$(MAKE) -f $(MAKEFILE) clean

fullbgp:
	@echo Full rebuild for BGP: $(HOST)
	$(MAKE) -f $(MAKEFILE) all
	$(MAKE) -f Makefile_FE all
#	$(MAKE) -f Makefile_FEgcc all
