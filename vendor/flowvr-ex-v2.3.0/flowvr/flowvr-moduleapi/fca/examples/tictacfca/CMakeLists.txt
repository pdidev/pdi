PROJECT(tictacfca)

INCLUDE(CTest)
ENABLE_TESTING()

SET(PROJECT_VERSION_MAJOR "1")
SET(PROJECT_VERSION_MINOR "0")
SET(PROJECT_VERSION_PATCH "0")
SET(PROJECT_VERSION "${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}.${PROJECT_VERSION_PATCH}")


# CMAKE SETTINGS

#Require cmake 2.8 at least
IF(DEFINED CMAKE_PATCH_VERSION)
  CMAKE_MINIMUM_REQUIRED(VERSION "2.8" FATAL_ERROR)
ELSE(DEFINED CMAKE_PATCH_VERSION)
  MESSAGE(FATAL_ERROR "Your version of CMake is too old, please use at least cmake 2.6")
ENDIF(DEFINED CMAKE_PATCH_VERSION)


# Application  Name


SET(app tictacfca)

# INSTALLATION DIRECTORIES

SET(INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include)
SET(COMP_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/tictacfca/components)  # Use namespace to define directory: include/name/space/components
SET(LIB_DIR ${CMAKE_INSTALL_PREFIX}/lib)
SET(COMP_LIB_DIR ${CMAKE_INSTALL_PREFIX}/lib)
SET(BIN_DIR ${CMAKE_INSTALL_PREFIX}/bin)
SET(MODULE_DIR ${CMAKE_INSTALL_PREFIX}/bin)
SET(CMAKE_DIR ${CMAKE_INSTALL_PREFIX}/share/tictacfca/cmake) # Use namespace to define directory: /share/name/space/cmake




# CHECK FLOWVR DEPENDENCY

FIND_PACKAGE(FlowVR REQUIRED)
IF(FLOWVR_FOUND)
 MESSAGE(STATUS "FlowVR Found")
ELSE(FLOWVR_FOUND)
 MESSAGE(SEND_ERROR "FlowVR Not Found")
ENDIF(FLOWVR_FOUND)



OPTION(BUILD_MODULES "build application  module binaries" TRUE)

IF(BUILD_MODULES)
  # CHECK THREADS DEPENDENCY
  FIND_PACKAGE(Threads REQUIRED)
ENDIF(BUILD_MODULES)





# Configuration Files 

FOREACH(shell sh)
  CONFIGURE_FILE(config/app-config.${shell}.in ${CMAKE_CURRENT_BINARY_DIR}/${app}-config.${shell} @ONLY)
  INSTALL(PROGRAMS  ${CMAKE_CURRENT_BINARY_DIR}/${app}-config.${shell} DESTINATION ${BIN_DIR})
ENDFOREACH(shell sh)

CONFIGURE_FILE(config/appConfig.cmake.in  ${CMAKE_CURRENT_BINARY_DIR}/${app}Config.cmake @ONLY)
INSTALL(FILES  ${CMAKE_CURRENT_BINARY_DIR}/${app}Config.cmake   DESTINATION ${CMAKE_DIR})

INSTALL(FILES ${app}.csv DESTINATION ${CMAKE_INSTALL_PREFIX})


ADD_SUBDIRECTORY(src)


# INSTALL INCLUDE FILES
FILE(GLOB include-files ${COMP_INCLUDE_DIR}/*.h) 
INSTALL(FILES ${include-files} DESTINATION ${COMP_INCLUDE_DIR})

INSTALL(CODE "MESSAGE(STATUS \"!!!Rember to execute \n. ${BIN_DIR}/flowvr-${app}-config.sh\nto properly run the examples \n(include it in your .bashrc for a permanent installation)\") ")
