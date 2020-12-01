PROJECT(FlowVRContrib VERSION "${FLOWVR_MAIN_VERSION}")

INCLUDE(MacroLogFeature)

# You will find in this directory various contributions to FLowVR

# CHECK FLOWVR DEPENDENCY (other required dependencies checked locally where needed)
FIND_PACKAGE(FlowVR REQUIRED)

SET(FlowVRContrib_DIR ${CMAKE_SOURCE_DIR}/flowvr-contrib/cmake )

SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} 
                     "${CMAKE_CURRENT_SOURCE_DIR}/cmake"
                     "${FlowVRContrib_BINARY_DIR}/cmake")
                     

# install flowvr-contrib-config
FOREACH(config ${ContribName}-config.sh )
  SET(INSTALL_PREFIX ${CMAKE_BINARY_DIR})
  SET(FLOWVRCONFIG_CMAKE "/share/flowvr-contrib/cmake")
  SET(FLOWVRCONTRIBCONFIG_CMAKE "/cmake")

  CONFIGURE_FILE(${config}.in ${CMAKE_BINARY_DIR}/bin/${config} @ONLY)
#  SET(INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})
  CONFIGURE_FILE(${config}.in ${FlowVRContrib_BINARY_DIR}/${config}.dist @ONLY)
  INSTALL(PROGRAMS ${FlowVRContrib_BINARY_DIR}/${config}.dist 
          DESTINATION bin
          RENAME ${config})
ENDFOREACH(config)



# Add subdirectories
ADD_SUBDIRECTORY(components)


##########################################
# Major contrib components
##########################################

OPTION(BUILD_CONTRIB_FLOWVR_TESTS "FlowVR Contrib: Tests" FALSE)


OPTION(BUILD_CONTRIB_FLOWVR_VRPN "FlowVR Contrib: Library to use VRPN devices in FlowVR applications." FALSE)

OPTION(BUILD_CONTRIB_FLOWVR_IMMERSIVE "FlowVR Contrib: Library to use Immersive system in FlowVR applications." FALSE)



MACRO_LOG_FEATURE(BUILD_CONTRIB_FLOWVR_TESTS "FlowVR Contrib: tests"
                                   "Tests for flowvr." 
                                   "http://flowvr.sf.net" 
                                   FALSE 
                                   "Optional")

MACRO_LOG_FEATURE(BUILD_CONTRIB_FLOWVR_VRPN "FlowVR Contrib: FlowVR VRPN" 
                                    "Library to use VRPN devices in FlowVR applications." 
                                    "http://flowvr.sf.net" 
                                    FALSE 
                                    "Optional")

MACRO_LOG_FEATURE(BUILD_CONTRIB_FLOWVR_IMMERSIVE "FlowVR Contrib: FlowVR IMMERSIVE" 
                                    "Library to use immersive systems (CAVE, screen walls, etc.) in FlowVR applications." 
                                    "http://flowvr.sf.net" 
                                    FALSE 
                                    "Optional")

MACRO_DISPLAY_FEATURE_LOG()


# Add sub directories  and set variable to find cmake directory 

SET(FlowVR-VRPN_DIR ${CMAKE_CURRENT_SOURCE_DIR}/flowvr-vrpn/cmake)
IF(BUILD_CONTRIB_FLOWVR_VRPN)
  ADD_SUBDIRECTORY(flowvr-vrpn)
ENDIF(BUILD_CONTRIB_FLOWVR_VRPN)

SET(FlowVR-IMMERSIVE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/flowvr-immersive/cmake)
IF(BUILD_CONTRIB_FLOWVR_IMMERSIVE)
  ADD_SUBDIRECTORY(flowvr-immersive)
ENDIF(BUILD_CONTRIB_FLOWVR_IMMERSIVE)

IF(BUILD_CONTRIB_FLOWVR_TESTS)
  ADD_SUBDIRECTORY(tests)
ENDIF(BUILD_CONTRIB_FLOWVR_TESTS)

ADD_SUBDIRECTORY(utils)


IF(BUILD_FLOWVR_DOXYGEN)

  FIND_PACKAGE(Doxygen)
  
    
  IF(DOXYGEN)
    IF(DOT)
      SET(HAVE_DOT "YES")
      SET(CLASS_DIAGRAMS "NO")
    ELSE(DOT)
      SET(HAVE_DOT "NO")
      SET(CLASS_DIAGRAMS "YES")
    ENDIF(DOT)
    
    # we have to set the excluded items all in one row (or use the += syntax, which we do not do here)
    SET(DOX_ITEMS_EXCLUDED "*/cmake/* */tests/* */flowvr-vim/*")
    SET(DOX_PROJECT_NAME "FlowVR -- contribution directory")
    SET(DOX_PROJECT_PREFIX "FlowVR-Contrib")
    SET(DOX_PROJECT_VERSION ${PROJECT_VERSION})
    
    # use 'global' doxyfile.in from top-level
    CONFIGURE_FILE(../flowvr-suite-codedoc/Doxyfile.in ${FlowVRContrib_BINARY_DIR}/Doxyfile)
    CONFIGURE_FILE(../flowvr-suite-codedoc/footer.html.in ${FlowVRContrib_BINARY_DIR}/footer.html)
    CONFIGURE_FILE(../flowvr-suite-codedoc/header.html.in ${FlowVRContrib_BINARY_DIR}/header.html)

    ADD_CUSTOM_TARGET(doc-flowvr-contrib COMMAND ${DOXYGEN} COMMENT "generate flowvr-contrib doxygen")
      ADD_DEPENDENCIES(doc doc-flowvr-contrib)
    
    INSTALL(DIRECTORY ${FlowVRContrib_BINARY_DIR}/doc/ DESTINATION share/flowvr/doc/${DOX_PROJECT_PREFIX} )
    INSTALL( FILES ../flowvr-suite-codedoc/flowvr-logo-small.png ../flowvr-suite-codedoc/stylesheet.css DESTINATION  share/flowvr/doc)

ENDIF(DOXYGEN)
ENDIF(BUILD_FLOWVR_DOXYGEN)
  
  # Useless (duplicated in contrib sub directories). 
# install cmake macros 
#INSTALL(DIRECTORY  cmake  DESTINATION  share/flowvr-contrib/   
#  PATTERN ".svn" EXCLUDE 
#  PATTERN "FlowVRConfig.cmake" EXCLUDE )


