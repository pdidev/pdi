PROJECT(flowvr_base VERSION "${FLOWVR_MAIN_VERSION}")

set(CMAKE_CXX_STANDARD 11)


#INCLUDE(CheckLibraryExists)
INCLUDE(CheckFunctionExists)
INCLUDE(CheckSymbolExists)
INCLUDE(CheckProcessSharedThreads)

CHECK_FOR_PROCESS_SHARED_THREADS(FLOWVR_USE_MPTHREAD)
MACRO_LOG_FEATURE(FLOWVR_USE_MPTHREAD "MPThreads" "Multiprocess Shared Threads"  "" FALSE "Optional")

SET(CMAKE_REQUIRED_INCLUDES execinfo.h)
CHECK_FUNCTION_EXISTS(backtrace FLOWVR_HAVE_EXECINFO_H)
CHECK_SYMBOL_EXISTS( MSG_NOSIGNAL "signal.h;sys/types.h;sys/socket.h;bits/socket.h" FLOWVR_OS_HAVE_MSGNOSIGNAL)

CONFIGURE_FILE(include/flowvr/config.h.in ${flowvr_base_BINARY_DIR}/include/flowvr/config.h)


# Enable -fPIC compiler flag (position-independent code)
SET(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS}\ -fPIC)


INCLUDE_DIRECTORIES(include)
ADD_SUBDIRECTORY(src)

FILE(GLOB include-files include/flowvr/*.h)
INSTALL(FILES ${include-files} DESTINATION include/flowvr)
INSTALL(FILES include/flowvr/utils/filepath.h DESTINATION include/flowvr/utils)

FOREACH(include-subdir ipc mem sharedmem utils)
 FILE(GLOB include-files include/flowvr/${include-subdir}/*.h)
 INSTALL(FILES ${include-files} DESTINATION include/flowvr/${include-subdir})
ENDFOREACH(include-subdir)

INSTALL(FILES ${flowvr_base_BINARY_DIR}/include/flowvr/config.h DESTINATION include/flowvr)
