#Get the hostname of current machine :
	find_program(HOSTNAME_CMD NAMES hostname)
	exec_program(${HOSTNAME_CMD} OUTPUT_VARIABLE HOSTNAME)
	set(SITE "${HOSTNAME}")
	MARK_AS_ADVANCED(HOSTNAME_CMD)
	
#Get the system information of current machine
	find_program(UNAME NAMES uname)
	macro(getuname name flag)
	  exec_program("${UNAME}" ARGS "${flag}" OUTPUT_VARIABLE "${name}")
	endmacro(getuname)
	MARK_AS_ADVANCED(UNAME)


STRING(TOLOWER ${CMAKE_SYSTEM_NAME} system-NAME)
if(${system-NAME} STREQUAL "darwin")
	SET(DISTRIB2 "OSX")
else(${system-NAME} STREQUAL "darwin")
	#Try to get the distrib
	find_program(CAT NAMES cat)
	exec_program("${CAT}" ARGS " /etc/issue" OUTPUT_VARIABLE DISTRIB) 
	MARK_AS_ADVANCED(CAT)
STRING(REPLACE "\\n \\l" "" DISTRIB2 ${DISTRIB})
endif(${system-NAME} STREQUAL "darwin")
	 
getuname(osname -s)
getuname(osrel  -r)
getuname(cpu    -m)

SET(BUILDNAME "${osname}-${DISTRIB2}-${cpu}-EXAMPLE_FLUID")
 
set(CTEST_PROJECT_NAME "FlowVR")
set(CTEST_NIGHTLY_START_TIME "01:00:00 EST")

set(CTEST_DROP_METHOD "http")
set(CTEST_DROP_SITE "cdash.inria.fr")
set(CTEST_DROP_LOCATION "/CDash/submit.php?project=FlowVR")
set(CTEST_DROP_SITE_CDASH TRUE)

