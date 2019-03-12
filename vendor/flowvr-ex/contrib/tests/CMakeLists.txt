#################################################
## AFFINITY TEST
#################################################

OPTION(BUILD_FLOWVR_CONTRIB_AFFINITY_TEST "Build test for affinity mapping" FALSE)
MARK_AS_ADVANCED(BUILD_FLOWVR_CONTRIB_AFFINITY_TEST)

IF (BUILD_FLOWVR_CONTRIB_AFFINITY_TEST) 
  ADD_SUBDIRECTORY(affinity-tests)
ENDIF (BUILD_FLOWVR_CONTRIB_AFFINITY_TEST)

MACRO_LOG_FEATURE(BUILD_FLOWVR_CONTRIB_AFFINITY_TEST "FlowVR Contrib: affinity tests" "Build test for affinity mapping" "http://flowvr.sf.net" FALSE "Optional")


#################################################
## FINITEQUEUE TEST
#################################################

OPTION(BUILD_FLOWVR_CONTRIB_FINITEQUEUE_TEST "Build test for finite queue filters" FALSE)

MARK_AS_ADVANCED(BUILD_FLOWVR_CONTRIB_FINITEQUEUE_TEST)

IF(BUILD_FLOWVR_CONTRIB_FINITEQUEUE_TEST)
  ADD_SUBDIRECTORY(finitequeuefilter-tests)
ENDIF(BUILD_FLOWVR_CONTRIB_FINITEQUEUE_TEST)

MACRO_LOG_FEATURE(BUILD_FLOWVR_CONTRIB_FINITEQUEUE_TEST "FlowVR Contrib: finitequeue tests" "Build test/example for FiniteQueueFilters" "http://flowvr.sf.net" FALSE "Optional")


#################################################
## SHAREDMEMORY TEST
#################################################

OPTION(BUILD_FLOWVR_CONTRIB_SHAREDMEMORY_TEST "Build test shared memory management" FALSE)

MARK_AS_ADVANCED(BUILD_FLOWVR_CONTRIB_SHAREDMEMORY_TEST)

IF(BUILD_FLOWVR_CONTRIB_SHAREDMEMORY_TEST)
  ADD_SUBDIRECTORY(sharedmemory-tests)
ENDIF(BUILD_FLOWVR_CONTRIB_SHAREDMEMORY_TEST)

MACRO_LOG_FEATURE(BUILD_FLOWVR_CONTRIB_SHAREDMEMORY_TEST "FlowVR Contrib: sharedmemory tests" "Build test/example for sharedmemory operations" "http://flowvr.sf.net" FALSE "Optional")

#################################################
## BUFFER TEST
#################################################
OPTION(BUILD_FLOWVR_CONTRIB_BUFFER_TEST "Build test for buffer management" FALSE)

MARK_AS_ADVANCED(BUILD_FLOWVR_CONTRIB_BUFFER_TEST)

IF(BUILD_FLOWVR_CONTRIB_BUFFER_TEST)
  ADD_SUBDIRECTORY(buffer-tests)
ENDIF(BUILD_FLOWVR_CONTRIB_BUFFER_TEST)

MACRO_LOG_FEATURE(BUILD_FLOWVR_CONTRIB_BUFFER_TEST "FlowVR Contrib: buffer tests" "Build test/example buffer management" "http://flowvr.sf.net" FALSE "Optional")
#################################################
## AUTOCONNECTION TEST
#################################################

## Commented to avoid conflicts. Tests need to be compiled and executed directly
## Need more work to integrate theses tests in the compilation of the suite

# OPTION(BUILD_FLOWVR_CONTRIB_AUTOCONNECTION_TEST "Build test autoconnection" FALSE)

# MARK_AS_ADVANCED(BUILD_FLOWVR_CONTRIB_AUTOCONNECTION_TEST)

# IF(BUILD_FLOWVR_CONTRIB_AUTOCONNECTION_TEST)
#   ADD_SUBDIRECTORY(autoconnection-tests)
# ENDIF(BUILD_FLOWVR_CONTRIB_AUTOCONNECTION_TEST)

# MACRO_LOG_FEATURE(BUILD_FLOWVR_CONTRIB_AUTOCONNECTION_TEST "FlowVR Contrib: autoconnection tests" "Build test/example for autoconnection operations" "http://flowvr.sf.net" FALSE "Optional")

#################################################
## HOST MAPPING TEST
#################################################

## Commented to avoid conflicts. Tests need to be compiled and executed directly
## Need more work to integrate theses tests in the compilation of the suite


# OPTION(BUILD_FLOWVR_CONTRIB_MAPPING_TEST "Build test mapping" FALSE)

# MARK_AS_ADVANCED(BUILD_FLOWVR_CONTRIB_MAPPING_TEST)

# IF(BUILD_FLOWVR_CONTRIB_MAPPING_TEST)
#   ADD_SUBDIRECTORY(mapping-tests)
# ENDIF(BUILD_FLOWVR_CONTRIB_MAPPING_TEST)

# MACRO_LOG_FEATURE(BUILD_FLOWVR_CONTRIB_MAPPING_TEST "FlowVR Contrib: mapping tests" "Build test/example for host mapping  operations" "http://flowvr.sf.net" FALSE "Optional")


#################################################
## SEGMENTS TEST
#################################################
OPTION(BUILD_FLOWVR_CONTRIB_SEGMENTS_TEST "Build test for segmented buffers" FALSE)

MARK_AS_ADVANCED(BUILD_FLOWVR_CONTRIB_SEGMENTS_TEST)

IF(BUILD_FLOWVR_CONTRIB_SEGMENTS_TEST)
  ADD_SUBDIRECTORY(segments-tests)
ENDIF(BUILD_FLOWVR_CONTRIB_SEGMENTS_TEST)

MACRO_LOG_FEATURE(BUILD_FLOWVR_CONTRIB_SEGMENTS_TEST "FlowVR Contrib: segmented buffer tests" "Build test/example segmented buffers" "http://flowvr.sf.net" FALSE "Optional")

#################################################
## tcp-test TEST
#################################################

OPTION(BUILD_FLOWVR_CONTRIB_TCPTEST_TEST "Build tcp-test" FALSE)
MARK_AS_ADVANCED(BUILD_FLOWVR_CONTRIB_TCPTEST_TEST)

IF(BUILD_FLOWVR_CONTRIB_TCPTEST_TEST)
  ADD_SUBDIRECTORY(tcp-tests)
ENDIF(BUILD_FLOWVR_CONTRIB_TCPTEST_TEST)

MACRO_LOG_FEATURE(BUILD_FLOWVR_CONTRIB_TCPTEST_TEST "FlowVR Contrib: tcp tests" "Build test/example for tcp operations" "http://flowvr.sf.net" FALSE "Optional")
