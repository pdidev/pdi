# THREAD-LEVEL AFFINITY EXAMPLE 

INCLUDE_DIRECTORIES(${FLOWVR_INCLUDE_DIR})

ADD_EXECUTABLE(flowvr-test-thread-level-affinity thread-level-affinity.cpp)
TARGET_LINK_LIBRARIES(flowvr-test-thread-level-affinity 
                      ${FLOWVR_base_LIBRARY} ${CMAKE_THREAD_LIBS_INIT})
                      
INSTALL(TARGETS flowvr-test-thread-level-affinity
  DESTINATION bin${BINDIR_SUFFIX}
)

