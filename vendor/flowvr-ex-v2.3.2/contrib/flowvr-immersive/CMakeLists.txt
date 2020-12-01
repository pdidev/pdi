PROJECT(FlowVR-IMMERSIVE VERSION "${FLOWVR_MAIN_VERSION}")

INCLUDE(MacroLogFeature)

ENABLE_TESTING()

SET(INCLUDE_DIR include)
SET(LIB_DIR lib)
SET(BIN_DIR bin)
SET(MODULE_DIR share/flowvr-immersive/modules/bin)
SET(CMAKE_DIR share/flowvr-immersive/cmake)
SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${FlowVR_DIR})


# CHECK FLOWVR DEPENDENCY
FIND_PACKAGE(FlowVR)
MACRO_LOG_FEATURE(FLOWVR_FOUND "FlowVR" "FlowVR Middelware" "http://flowvr.sourceforge.net" TRUE "Required By FlowVR Immersive")


#CHECK VRPN DEPENDENCY
FIND_PACKAGE(VRPN)
MACRO_LOG_FEATURE(VRPN_FOUND "VRPN" "The Virtual Reality Peripheral Network" "http://www.cs.unc.edu/Research/vrpn/" TRUE "Required by FlowVR VRPN")


IF(FLOWVR_FOUND AND VRPN_FOUND)

  FOREACH(config flowvr-immersive-config.sh )
   SET(INSTALL_PREFIX ${CMAKE_BINARY_DIR})
   SET(FLOWVRIMMERSIVECONFIG_CMAKE)	
   CONFIGURE_FILE(${config}.in ${CMAKE_BINARY_DIR}/bin/${config} @ONLY)
   SET(INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})
   SET(FLOWVRIMMERSIVECONFIG_CMAKE "/share/flowvr-immersive/cmake")
   CONFIGURE_FILE(${config}.in ${FlowVR-IMMERSIVE_BINARY_DIR}/${config}.dist @ONLY)
   INSTALL(PROGRAMS ${FlowVR-IMMERSIVE_BINARY_DIR}/${config}.dist 
           DESTINATION bin
           RENAME ${config})
  ENDFOREACH(config)




  # Install cmake macros and modules
  INSTALL(DIRECTORY  cmake  DESTINATION  share/flowvr-immersive/   
    PATTERN "FlowVRImmersiveConfig.cmake" EXCLUDE
    PATTERN ".in" EXCLUDE
    PATTERN ".svn" EXCLUDE )


  
  SET(INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})  
  CONFIGURE_FILE(FlowVRImmersiveConfig.cmake.dist.in  ${FlowVR-IMMERSIVE_BINARY_DIR}/FlowVRImmersiveConfig.cmake.dist @ONLY)
  
  INSTALL(FILES  ${FlowVR-IMMERSIVE_BINARY_DIR}/FlowVRImmersiveConfig.cmake.dist
    DESTINATION share/flowvr-immersive/cmake
    RENAME FlowVRImmersiveConfig.cmake)

 

  
  SET(components src)
  
  FOREACH(component ${components})
    IF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${component})
      ADD_SUBDIRECTORY(${component})
    ENDIF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${component})
  ENDFOREACH(component)
  
  
  FOREACH(include-subdir IO)
    FILE(GLOB include-files include/${include-subdir}/*.h)
    INSTALL(FILES ${include-files} DESTINATION include/flowvr-immersive/${include-subdir} PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ )
  ENDFOREACH(include-subdir)
  
  FOREACH(target xml)
    INSTALL(DIRECTORY  ${target}
      DESTINATION  share/flowvr-immersive/ USE_SOURCE_PERMISSIONS PATTERN ".svn" EXCLUDE)
  ENDFOREACH(target)

# Install cmake macros and modules
INSTALL(DIRECTORY  cmake  DESTINATION  share/flowvr-immersive/   
  PATTERN "FlowVRImmersiveConfig.cmake" EXCLUDE 
  PATTERN ".svn" EXCLUDE 
  PATTERN "*.in" EXCLUDE)

ENDIF(FLOWVR_FOUND AND VRPN_FOUND)

MACRO_DISPLAY_FEATURE_LOG()
