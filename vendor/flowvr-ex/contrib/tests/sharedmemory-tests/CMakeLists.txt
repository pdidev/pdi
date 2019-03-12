# THREAD-LEVEL AFFINITY EXAMPLE 

INCLUDE_DIRECTORIES(${FLOWVR_INCLUDE_DIR})

ADD_EXECUTABLE(flowvr-test-sharedmemory sharedmemory-test.cpp)
TARGET_LINK_LIBRARIES(flowvr-test-sharedmemory
                      ${FLOWVR_base_LIBRARY}
                      ${FLOWVR_ftl_LIBRARY} 
                      ${CMAKE_THREAD_LIBS_INIT})
                      
INSTALL(TARGETS flowvr-test-sharedmemory
        DESTINATION bin${BINDIR_SUFFIX}
       )

