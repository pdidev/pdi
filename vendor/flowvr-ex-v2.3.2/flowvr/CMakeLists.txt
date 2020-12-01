PROJECT(FlowVR VERSION "${FLOWVR_MAIN_VERSION}")




SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
SET(FLOWVR_PREFIX ${CMAKE_BINARY_DIR})
SET(FLOWVR_PID_LOG_DIR "$ENV{HOME}/.flowvr/" CACHE PATH "Dir where FlowVR log module pids")

###############################################################
## DEPENDENCIES
###############################################################
INCLUDE(MacroLogFeature)


#FIND_PACKAGE(Python3 COMPONENTS Interpreter Development) # not compatible with cmake < 3.12
FIND_PACKAGE(PythonInterp 3)
MACRO_LOG_FEATURE(PYTHONINTERP_FOUND  "PythonInterp"
  "Python 3 Interpreter"
  "www.python.org/"
  TRUE
  "Required to process FlowVR applications")


FIND_PACKAGE(Threads)
MACRO_LOG_FEATURE(CMAKE_USE_PTHREADS_INIT "Threads"
                                          "Threading support"
                                          ""
                                          TRUE
                                          "Mandatory")
FIND_PACKAGE(OpenGL)
MACRO_LOG_FEATURE(OPENGL_FOUND "OpenGL"
                               "OpenGL support"
                               "http://www.opengl.org"
                               FALSE
                               "Required by FlowVR GLTrace, FlowVR GLGraph and some examples")
FIND_PACKAGE(GLUT)
MACRO_LOG_FEATURE(GLUT_FOUND "Glut"
                             "The OpenGL Utility Toolkit"
                             "http://freeglut.sourceforge.net"
                             FALSE
                             "Required by FlowVR GLTrace, FlowVR GLGraph and some examples")
                           
FIND_PACKAGE(Doxygen)
MACRO_LOG_FEATURE(DOXYGEN "Doxygen"
                          "Documentation generation"
                          "http://www.doxygen.org"
                          FALSE
                          "Optional")
                        
FIND_PACKAGE(hwloc)
MACRO_LOG_FEATURE(HWLOC_FOUND   "hwloc"
                          "Portable Hardware Locality (development libraries)"
                          "http://http://www.open-mpi.org/projects/hwloc/"
                          TRUE
                          "Required by FlowVR Modules and daemon")

#MESSAGE("HWLOC_VERSION=${HWLOC_VERSION}\n")
IF( HWLOC_VERSION  VERSION_LESS 2.0.0)
  # 1.x version
  add_definitions(-DHWLOC_VERSION_1)
  #MESSAGE("VERSION LESS THAN 2\n")
ELSE()
  #MESSAGE("VERSION MORE THAN 2\n")
ENDIF()

#add_definitions(-fsanitize=thread)

# Set to later define preporcessor variable  FLOWVR_HAVE_HWLOC
if( HWLOC_FOUND )
    set( FLOWVR_HAVE_HWLOC ${HWLOC_FOUND} )
endif()

FIND_PACKAGE(MPI COMPONENTS CXX)

IF(MPI_FOUND AND NOT MPI_CXX_FOUND)

  # Probably a old cmake version supporting older MPI related variable names (before 2.8.9 ??)
  SET(MPI_CXX_FOUND TRUE)
  SET(MPI_CXX_INCLUDE_PATH ${MPI_INCLUDE_PATH})
  SET(MPI_CXX_LIBRARIES  ${MPI_LIBRARY} ${MPI_LIBRARIES} ${MPI_EXTRA_LIBRARY})
ENDIF( MPI_FOUND AND NOT MPI_CXX_FOUND)

MACRO_LOG_FEATURE(MPI_CXX_FOUND   "MPI"
                          "MESSAGE PASSING INTERFACE"
                          "http://http://www.open-mpi.org/"
                          FALSE
                          "Required by FlowVR daemon for fully taking advantage of high performance networks and some examples")


###################################################################
## GENERAL SETTINGS NEEDED FOR ALL SUBCOMPONENTS
###################################################################
INCLUDE_DIRECTORIES(${FlowVR_BINARY_DIR}/flowvr-base/include)
INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE}/flowvr-ftl/include)

###################################################################
# install flowvr-config shell script
###################################################################
FOREACH(config flowvr-config.sh )
  SET(INSTALL_PREFIX ${CMAKE_BINARY_DIR})
  SET(FLOWVRCONFIG_CMAKE) # assign the empty variable for this script
  CONFIGURE_FILE(${config}.in ${CMAKE_BINARY_DIR}/bin/${config} @ONLY)

  SET(INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})
  SET(FLOWVRCONFIG_CMAKE "/share/flowvr/cmake")
  CONFIGURE_FILE(${config}.in ${FlowVR_BINARY_DIR}/${config}.dist @ONLY)

  INSTALL(PROGRAMS ${FlowVR_BINARY_DIR}/${config}.dist
          DESTINATION bin
          RENAME ${config})
ENDFOREACH(config)

ADD_SUBDIRECTORY(cmake.in)

# install cmake macros
INSTALL(DIRECTORY  cmake  DESTINATION  share/flowvr/
  PATTERN ".svn" EXCLUDE
  PATTERN "FlowVRConfig.cmake" EXCLUDE )

##################################################################
## COMPONENTS
##################################################################

set(components flowvr-base
               flowvr-ftl
               flowvr-daemon
               flowvr-interface
               flowvr-scripts
               flowvr-modules
               flowvr-launcher
               flowvr-appy
               flowvr-moduleapi
               flowvr-examples )



FOREACH(component ${components})
    ADD_SUBDIRECTORY(${component})
ENDFOREACH(component ${components})


###################################################################
# packaging
###################################################################
SET(CPACK_PACKAGE_VERSION_MAJOR "${PROJECT_VERSION_MAJOR}")
SET(CPACK_PACKAGE_VERSION_MINOR "${PROJECT_VERSION_MINOR}")
SET(CPACK_PACKAGE_VERSION_PATCH "${PROJECT_VERSION_PATCH}")
SET(CPACK_PACKAGE_VENDOR "INRIA - Grenoble Universities")
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "FlowVR: A Middleware for High Performance Interactive Computing")

SET(CPACK_PACKAGE_DESCRIPTION_FILE "${FlowVR_SOURCE_DIR}/../README.md")

SET(CPACK_RESOURCE_FILE_LICENSE "${FlowVR_SOURCE_DIR}/flowvr-codedoc/flowvr_copyright.txt")
SET(CPACK_SOURCE_IGNORE_FILES "/CVS/;/_darcs/;/\\\\\\\\.svn/;\\\\\\\\.swp$;\\\\\\\\.#;/#")

# Pick a generator
IF(UNIX)
  IF(APPLE)
    SET(CPACK_GENERATOR "PackageMaker;STGZ")
  ELSE(APPLE)
    SET(CPACK_GENERATOR "STGZ;TGZ")
  ENDIF(APPLE)
  SET(CPACK_SOURCE_GENERATOR "TGZ")
ELSE(UNIX)
  SET(CPACK_GENERATOR "NSIS;ZIP")
  SET(CPACK_SOURCE_GENERATOR "ZIP")
ENDIF(UNIX)

INCLUDE(CPack)


#######################################################################
## DOXYGEN
#######################################################################

IF(BUILD_FLOWVR_DOXYGEN AND DOXYGEN)
    IF(DOT)
      SET(DOX_HAVE_DOT "YES")
    ELSE(DOT)
      SET(DOX_HAVE_DOT "NO")
    ENDIF(DOT)
    
    # we have to set the excluded items all in one row (or use the += syntax, which we do not do here)
    SET(DOX_ITEMS_EXCLUDED "*/tinyxml* */${ContribName}/* */flowvr-render/* */flowvr-vrpn/* */vtk-flowvr/* */flowvr/cmake/* */flowvr-examples/* */flowvr-interface/*  */flowvr-utils/* */flowvr-app/* */flowvr-parse/* */.svn/* */topology/*")
    SET(DOX_PROJECT_NAME "FlowVR -- base library")
    SET(DOX_PROJECT_PREFIX "FlowVR")
    SET(DOX_PROJECT_VERSION ${PROJECT_VERSION})
    
    SET(DOX_PREDEFINED_SYMBOLS "DEBUG FLOWVR_SCHED_HAVE_GETCPU FLOWVR_IPC_DEBUG")
    
    # use 'global' doxyfile.in from top-level
    CONFIGURE_FILE(../flowvr-suite-codedoc/Doxyfile.in ${FlowVR_BINARY_DIR}/Doxyfile)
    CONFIGURE_FILE(../flowvr-suite-codedoc/footer.html.in ${FlowVR_BINARY_DIR}/footer.html)
    CONFIGURE_FILE(../flowvr-suite-codedoc/header.html.in ${FlowVR_BINARY_DIR}/header.html)

    ADD_CUSTOM_TARGET(doc-flowvr  COMMAND ${DOXYGEN} COMMENT "generate flowvr doxygen")
    ADD_DEPENDENCIES(doc doc-flowvr)
    
    INSTALL(DIRECTORY ${FlowVR_BINARY_DIR}/doc/ DESTINATION share/flowvr/doc/${DOX_PROJECT_PREFIX})
    INSTALL( FILES ../flowvr-suite-codedoc/flowvr-logo-small.png ../flowvr-suite-codedoc/stylesheet.css DESTINATION share/flowvr/doc)
      
ENDIF(BUILD_FLOWVR_DOXYGEN AND DOXYGEN)


MACRO_DISPLAY_FEATURE_LOG()
