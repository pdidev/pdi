# This is just a simple library without dependencies except FLowVR. Does not make it a project to keep cmake output clearer.

ENABLE_TESTING()

SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${FlowVR_DIR})

# CHECK FLOWVR DEPENDENCY
FIND_PACKAGE(FlowVR)

IF(FLOWVR_FOUND)

INCLUDE_DIRECTORIES(include ${FLOWVR_INCLUDE_DIR})

ADD_LIBRARY(fca SHARED src/main.cpp)
TARGET_LINK_LIBRARIES(fca ${FLOWVR_base_LIBRARY} ${FLOWVR_mod_LIBRARY} ${CMAKE_THREAD_LIBS_INIT})
SET_TARGET_PROPERTIES(fca PROPERTIES CLEAN_DIRECT_OUTPUT 1)
INSTALL(TARGETS fca LIBRARY DESTINATION lib)

# Please do not delete the lines below, will become useful later
#ADD_LIBRARY(fvmoduleapi-static STATIC src/fvmoduleapi.cpp src/fvmoduleapi-ftl.cpp)
#TARGET_LINK_LIBRARIES(fvmoduleapi-static ${FLOWVR_base_LIBRARY} ${FLOWVR_mod_LIBRARY} ${FLOWVR_utils_LIBRARY}  ${CMAKE_THREAD_LIBS_INIT})
#SET_TARGET_PROPERTIES(fvmoduleapi-static PROPERTIES OUTPUT_NAME "fvmoduleapi")
#SET_TARGET_PROPERTIES(fvmoduleapi-static PROPERTIES CLEAN_DIRECT_OUTPUT 1)
#INSTALL(TARGETS fvmoduleapi-static DESTINATION lib)

FILE(GLOB include-files include/*.h)
INSTALL(FILES ${include-files} DESTINATION include/fca/ PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ )

ENDIF(FLOWVR_FOUND)

