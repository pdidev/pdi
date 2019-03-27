SHELL=/bin/sh

TARGETS=all install installtests checkinst test comptest packtest relink \
	depend clean

.PHONY: $(TARGETS)

$(TARGETS):
	@case `ls -d build-* 2> /dev/null | wc -l | sed -e 's/[^0-9]//g'` in \
	  0) echo "ERROR: No build configuration found. Run configure first." \
	     ;; \
	  1) cd build-*; ${MAKE} $@ \
	     ;; \
	  *) if [ -n "${CFG}" -a -d build-${CFG} ]; then \
	        cd build-${CFG}; ${MAKE} $@; \
	     else \
	        echo "ERROR: Select build configuration via CFG= out of"; \
	        ls -d1 build-* | sed -e 's/build-/* /' ; \
	        exit 1 ; \
	     fi ;; \
	esac
