
# FlowVR plugd
SET(plugd-SRC plugd/class.cpp plugd/object.cpp plugd/actionhandler.cpp
	      plugd/routingtable.cpp plugd/outputmessagequeue.cpp
	      plugins/sys/flowvr.plugins.Commander.cpp
	      plugins/sys/flowvr.plugins.Regulator.cpp
	      plugins/sys/flowvr.plugins.BaseObject.cpp
	      plugins/sys/flowvr.plugins.Synchronizor.cpp
	      plugins/sys/flowvr.plugins.Filter.cpp
	      plugins/sys/flowvr.plugins.ThreadedSynchronizor.cpp
	      plugins/sys/flowvr.plugins.ThreadedFilter.cpp
	      daemon.cpp)
ADD_LIBRARY(flowvr-plugd SHARED ${plugd-SRC})
TARGET_LINK_LIBRARIES(flowvr-plugd flowvr-base ftlm ${CMAKE_THREAD_LIBS_INIT} dl )
if ( HWLOC_FOUND )
    INCLUDE_DIRECTORIES(${HWLOC_INCLUDE_DIR})
    TARGET_LINK_LIBRARIES(flowvr-plugd ${HWLOC_LIBRARIES} )
endif()
SET_TARGET_PROPERTIES(flowvr-plugd PROPERTIES SOVERSION ${PROJECT_VERSION_MAJOR} VERSION ${PROJECT_VERSION})
SET_SOURCE_FILES_PROPERTIES(plugd/class.cpp PROPERTIES COMPILE_FLAGS "-DFLOWVR_SO_SUFFIX=\"${CMAKE_SHARED_MODULE_SUFFIX}\"")

INSTALL(TARGETS flowvr-plugd LIBRARY DESTINATION lib)

# FlowVR daemon
FILE(GLOB_RECURSE daemon-HEADERS ../include/*.h) # express dependency to include files (optional)
ADD_EXECUTABLE(flowvrd daemon/main.cpp ${daemon-HEADERS})
#SET_TARGET_PROPERTIES(flowvrd PROPERTIES COMPILE_FLAGS "-DFLOWVR_DAEMON_VERSION=\"${PROJECT_VERSION_REVISION}\"")
TARGET_LINK_LIBRARIES(flowvrd ftlm flowvr-plugd ${CMAKE_DL_LIBS} ${CMAKE_THREAD_LIBS_INIT})
INSTALL(TARGETS flowvrd DESTINATION bin)

# flowvr-commands library
ADD_LIBRARY(flowvr-commands SHARED utils/telnet.cpp)
TARGET_LINK_LIBRARIES(flowvr-commands flowvr-base flowvr-mod ftlm ${CMAKE_THREAD_LIBS_INIT})
SET_TARGET_PROPERTIES(flowvr-commands PROPERTIES SOVERSION ${PROJECT_VERSION_MAJOR} VERSION ${PROJECT_VERSION})
INSTALL(TARGETS flowvr-commands LIBRARY DESTINATION lib)

# tools
ADD_EXECUTABLE(flowvr-shmdump utils/shmdump.cpp)
TARGET_LINK_LIBRARIES(flowvr-shmdump flowvr-base ftlm ${CMAKE_THREAD_LIBS_INIT})

ADD_EXECUTABLE(flowvr-kill utils/flowvr-kill.cpp)
TARGET_LINK_LIBRARIES(flowvr-kill flowvr-base flowvr-mod ftlm ${CMAKE_THREAD_LIBS_INIT})
INSTALL(TARGETS flowvr-kill DESTINATION bin)

ADD_EXECUTABLE(flowvr-fdump utils/fdump.cpp)
TARGET_LINK_LIBRARIES(flowvr-fdump flowvr-base ftlm ${CMAKE_THREAD_LIBS_INIT})
INSTALL(TARGETS flowvr-shmdump flowvr-fdump DESTINATION bin)

ADD_SUBDIRECTORY(plugins)
