# FLOWVR BASE LIBRARY

INCLUDE_DIRECTORIES("${flowvr_base_SOURCE_DIR}/include" 
		    "${flowvr_base_BINARY_DIR}/include" 
                    "${CMAKE_BINARY_DIR}/include" 
                    "${CMAKE_SOURCE_DIR}/flowvr/flowvr-ftl/include" #flowvr-ftl_SOURCE_DIR hasn't been set yet.
                    "${HWLOC_INCLUDE_DIR}"
                   )

###############################
## CONFIGURE MULTIBUF HANDLING
###############################

# TCP
CHECK_FUNCTION_EXISTS(writev  _FLOWVR_OS_HAVE_WRITEV)

CHECK_SYMBOL_EXISTS( HOST_NAME_MAX "unistd.h;sys/limits.h" _FLOWVR_OS_HAVE_HOST_NAME_MAX)

IF( NOT ${_FLOWVR_OS_HAVE_HOST_NAME_MAX} )
 CHECK_SYMBOL_EXISTS( _POSIX_HOST_NAME_MAX "sys/limits.h;unistd.h" _FLOWVR_OS_HAVE_HOST_NAME_MAX )
ENDIF( NOT ${_FLOWVR_OS_HAVE_HOST_NAME_MAX} )

IF( ${_FLOWVR_OS_HAVE_HOST_NAME_MAX} )
 SET( _OS_HOST_NAME_MAX ${_FLOWVR_OS_HAVE_HOST_NAME_MAX} )
ELSE( ${_FLOWVR_OS_HAVE_HOST_NAME_MAX} )
 SET( _OS_HOST_NAME_MAX 256 )
ENDIF( ${_FLOWVR_OS_HAVE_HOST_NAME_MAX} )


CONFIGURE_FILE(../include/flowvr/utils/multibuf.h.in ${flowvr_base_BINARY_DIR}/include/flowvr/utils/multibuf.h)


SET(base-SRC allocator.cpp 
             stamp.cpp 
             trace.cpp 
             message.cpp 
             buffer.cpp 
             bufferpool.cpp 
             bufferimp.cpp

             daemondata.cpp
             thread.cpp 
             portreader.cpp 
             portwriter.cpp
              
             xml.cpp 

             mem/sharedmemorybuffer.cpp 
             mem/sharedmemoryarea.cpp
             mem/sharedmemorymanager.cpp 
             mem/daemonsharedmemorymanager.cpp 
             mem/mpdata.cpp
             mem/memorybuffer.cpp
             mem/memorymanager.cpp
             
             utils/tcptools.cpp
             utils/backtrace.cpp
             utils/timing.cpp 
             utils/cmdline.cpp
             utils/filepath.cpp             
             utils/size.cpp
	     utils/fkill.cpp
                          
             topo.cpp
             )

FILE(GLOB_RECURSE base-HEADERS ../include/*.h)


ADD_LIBRARY(flowvr-base SHARED ${base-SRC} ${base-HEADERS})

LINK_DIRECTORIES( ${CMAKE_BUILD_PATH}/lib )


# check for special functions used in timing.cpp
# we have to take care: the -lrt is somehow a standard,
# but at the same time not available on all systems
# (for example mac)

SET(TARGET_LIBRARIES flowvr-base ${CMAKE_THREAD_LIBS_INIT} ${HWLOC_LIBRARIES})

FIND_LIBRARY(RT_LIB rt)

# have to make that an OR clause to check for RT_LIB_FOUND as well
# as checking the RT_LIB value itself. Might be a problem of my cmake (2.6)?

IF(RT_LIB_FOUND OR RT_LIB)
  SET(TARGET_LIBRARIES ${TARGET_LIBRARIES} ${RT_LIB})
ENDIF(RT_LIB_FOUND OR RT_LIB)

TARGET_LINK_LIBRARIES( ${TARGET_LIBRARIES}  )

SET_TARGET_PROPERTIES(flowvr-base PROPERTIES SOVERSION ${PROJECT_VERSION_MAJOR} VERSION ${PROJECT_VERSION}) 

ADD_LIBRARY(flowvr-utils SHARED utils/filepath.cpp)
TARGET_LINK_LIBRARIES(flowvr-utils flowvr-base)
SET_TARGET_PROPERTIES(flowvr-utils PROPERTIES SOVERSION ${PROJECT_VERSION_MAJOR} VERSION ${PROJECT_VERSION} COMPILE_FLAGS -DFLOWVR_PREFIX=\"${CMAKE_INSTALL_PREFIX}\")
INSTALL(TARGETS flowvr-utils LIBRARY DESTINATION lib)

ADD_EXECUTABLE(flowvr-kill utils/flowvr-kill.cpp)
TARGET_LINK_LIBRARIES(flowvr-kill flowvr-base  ftlm ${CMAKE_THREAD_LIBS_INIT})
INSTALL(TARGETS flowvr-kill DESTINATION bin)




# FLOWVR MODULE LIBRARY
SET(mod-SRC moduleapi.cpp moduleapifactory.cpp moduleapimultiprocessimpl.cpp
            moduleapifileimpl.cpp module.cpp parallel.cpp)
ADD_LIBRARY(flowvr-mod SHARED ${mod-SRC})
TARGET_LINK_LIBRARIES(flowvr-mod flowvr-base ${CMAKE_THREAD_LIBS_INIT} )


SET_TARGET_PROPERTIES(flowvr-mod PROPERTIES SOVERSION ${PROJECT_VERSION_MAJOR} VERSION ${PROJECT_VERSION})

INSTALL(TARGETS flowvr-base flowvr-mod LIBRARY DESTINATION lib)
