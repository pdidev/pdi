PROJECT(flowvrd)


set(CMAKE_CXX_STANDARD 11)

OPTION(BUILD_FLOWVRD_MPI_PLUGIN "FlowVR-Daemon MPI plugin." TRUE)
IF(NOT MPI_CXX_FOUND)
  SET(BUILD_FLOWVRD_MPI_PLUGIN FALSE CACHE BOOL "FlowVR-Daemon MPI plugin." FORCE)
  MESSAGE( "FlowVR-Daemon MPI plugin : MPI missing. BUILD_FLOWVRD_MPI_PLUGIN option turned OFF")
ENDIF(NOT MPI_CXX_FOUND)



MACRO_LOG_FEATURE(BUILD_FLOWVRD_MPI_PLUGIN "FlowVR MPI Plugin"
  "MPI based network plugin for FlowVR"
  ""
  FALSE
  "Used by FlowVR to provide MPI communications between Daemons")




INCLUDE_DIRECTORIES("${flowvr_base_SOURCE_DIR}/include"
                    "${flowvr_base_BINARY_DIR}/include"
		    "${flowvr_ftl_SOURCE_DIR}/include"
		    "${flowvrd_SOURCE_DIR}/include"
		    "${CMAKE_BINARY_DIR}/include"
		    )


ADD_SUBDIRECTORY(src)

INSTALL(FILES include/flowvr/daemon.h include/flowvr/telnet.h DESTINATION include/flowvr)

FOREACH(include-subdir plugd plugins)
 FILE(GLOB include-files include/flowvr/${include-subdir}/*.h)
 INSTALL(FILES ${include-files} DESTINATION include/flowvr/${include-subdir})
ENDFOREACH(include-subdir)
