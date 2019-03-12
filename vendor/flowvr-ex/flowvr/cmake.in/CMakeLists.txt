###########################################################
## CONFIGURE OF DEPLOYMENT SCRIPTS
###########################################################

# install FlowVRConfig.cmake
SET(INSTALL_PREFIX ${CMAKE_BINARY_DIR})
CONFIGURE_FILE(FlowVRConfig.cmake.in FlowVRConfig.cmake @ONLY)

SET(INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})
CONFIGURE_FILE(FlowVRConfig.cmake.dist.in ${FlowVR_BINARY_DIR}/FlowVRConfig.cmake.dist @ONLY)
INSTALL(FILES ${FlowVR_BINARY_DIR}/FlowVRConfig.cmake.dist
  DESTINATION share/flowvr/cmake # if this path is changed, the content of FlowVRConfig.cmake.dist.in must be changed accordingly
  RENAME FlowVRConfig.cmake)
  
