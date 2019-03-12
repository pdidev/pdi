PROJECT(FlowVR-VRPN)

INCLUDE(MacroLogFeature)

ENABLE_TESTING()

SET(INCLUDE_DIR include)
SET(LIB_DIR lib)
SET(BIN_DIR bin)
SET(MODULE_DIR share/flowvr-vrpn/modules/bin)
SET(CMAKE_DIR share/flowvr-vrpn/cmake)
SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${FlowVR_DIR})


# CHECK FLOWVR DEPENDENCY
FIND_PACKAGE(FlowVR)
MACRO_LOG_FEATURE(FLOWVR_FOUND "FlowVR" "FlowVR Middelware" "http://flowvr.sourceforge.net" TRUE "Required By FlowVR VRPN")


#CHECK OPENGL  DEPENDENCY
FIND_PACKAGE(OpenGL)
MACRO_LOG_FEATURE(OPENGL_FOUND "OpenGL" "" "http://www.opengl.org"  TRUE "Required by FlowVR VRPN  (required to compile modules)" )

#CHECK GLUT  DEPENDENCY
FIND_PACKAGE(GLUT)
MACRO_LOG_FEATURE(GLUT_FOUND "Glut" "The OpenGL Utility Toolkit" "http://freeglut.sourceforge.net" TRUE "Required by FlowVR VRPN (for the modules)")


#CHECK VRPN DEPENDENCY
FIND_PACKAGE(VRPN)
MACRO_LOG_FEATURE(VRPN_FOUND "VRPN" "The Virtual Reality Peripheral Network" "http://www.cs.unc.edu/Research/vrpn/" TRUE "Required by FlowVR VRPN")

#CHECK  GLUI DEPENDENCY
FIND_PACKAGE(GLUI)
MACRO_LOG_FEATURE(GLUI_FOUND "GLUI" "OpenGL User Interface" "http://glui.sourceforge.net" TRUE "Required By SettingFreq component")


IF(OPENGL_FOUND AND GLUT_FOUND AND GLUI_FOUND)
 SET(BUILD_FLOWVR_VRPN_SIMULATED_INTERFACE TRUE)
ELSE(OPENGL_FOUND AND GLUT_FOUND AND GLUI_FOUND)
 SET(BUILD_FLOWVR_VRPN_SIMULATED_INTERFACE FALSE) 
ENDIF(OPENGL_FOUND AND GLUT_FOUND AND GLUI_FOUND)



IF(FLOWVR_FOUND AND VRPN_FOUND AND OPENGL_FOUND AND GLUT_FOUND)

  FOREACH(config flowvr-vrpn-config.sh )
   SET(INSTALL_PREFIX ${CMAKE_BINARY_DIR})
   SET(FLOWVRVRPNCONFIG_CMAKE)	
   CONFIGURE_FILE(${config}.in ${CMAKE_BINARY_DIR}/bin/${config} @ONLY)
   SET(INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})
   SET(FLOWVRVRPNCONFIG_CMAKE "/share/flowvr-vrpn/cmake")
   CONFIGURE_FILE(${config}.in ${FlowVR-VRPN_BINARY_DIR}/${config}.dist @ONLY)
   INSTALL(PROGRAMS ${FlowVR-VRPN_BINARY_DIR}/${config}.dist 
           DESTINATION bin
           RENAME ${config})
  ENDFOREACH(config)




  # Install cmake macros and modules
  INSTALL(DIRECTORY  cmake  DESTINATION  share/flowvr-vrpn/   
    PATTERN "FlowVRVRPNConfig.cmake" EXCLUDE
    PATTERN ".in" EXCLUDE
    PATTERN ".svn" EXCLUDE )




#  SET(INSTALL_PREFIX ${CMAKE_BINARY_DIR})
#  CONFIGURE_FILE(FlowVRVRPNConfig.cmake.in FlowVRVRPNConfig.cmake @ONLY)
  
  SET(INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})  
  CONFIGURE_FILE(FlowVRVRPNConfig.cmake.dist.in  ${FlowVR-VRPN_BINARY_DIR}/FlowVRVRPNConfig.cmake.dist @ONLY)
  
  INSTALL(FILES  ${FlowVR-VRPN_BINARY_DIR}/FlowVRVRPNConfig.cmake.dist
    DESTINATION share/flowvr-vrpn/cmake
    RENAME FlowVRVRPNConfig.cmake)

 

  
  SET(components src examples)
  
  FOREACH(component ${components})
    IF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${component})
      ADD_SUBDIRECTORY(${component})
    ENDIF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${component})
  ENDFOREACH(component)
  
#   FOREACH(cmake_config_file VRPNConfig.cmake GLUIConfig.cmake FlowVRVRPNConfig.cmake)
#     CONFIGURE_FILE(cmake/${cmake_config_file}.in ${cmake_config_file} @ONLY)
#     INSTALL(FILES ${CMAKE_BINARY_DIR}/${cmake_config_file} DESTINATION ${CMAKE_DIR} PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
#   ENDFOREACH(cmake_config_file)
  
  FOREACH(include-subdir components)
    FILE(GLOB include-files include/flowvr-vrpn/${include-subdir}/*.h)
    INSTALL(FILES ${include-files} DESTINATION include/flowvr-vrpn/${include-subdir} PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ )
  ENDFOREACH(include-subdir)
  
  FOREACH(target xml)
    INSTALL(DIRECTORY  ${target}
      DESTINATION  share/flowvr-vrpn/ USE_SOURCE_PERMISSIONS PATTERN ".svn" EXCLUDE)
  ENDFOREACH(target)

# Install cmake macros and modules
INSTALL(DIRECTORY  cmake  DESTINATION  share/flowvr-vrpn/   
  PATTERN "FlowVRVRPNConfig.cmake" EXCLUDE 
  PATTERN ".svn" EXCLUDE 
  PATTERN "*.in" EXCLUDE)

ENDIF(FLOWVR_FOUND AND VRPN_FOUND AND OPENGL_FOUND AND GLUT_FOUND)

MACRO_DISPLAY_FEATURE_LOG()
