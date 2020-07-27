PROJECT(FlowVR-Suite)

########## CMAKE 
SET(flowvr_CMAKE_REQUIRED_VERSION "3.0")
CMAKE_MINIMUM_REQUIRED(VERSION "${flowvr_CMAKE_REQUIRED_VERSION}" FATAL_ERROR)


##### POLICIES #############


# Run "cmake   --help-policy CMP00XX" to know more about the relatex policy

# to supress some warnings related to change in the project() command for cmake 3.0
# (set version directly from  project()"
# to be  supressed and project() calls changed once 3.0 is set a the new minimum required version.
#IF(POLICY CMP0048)
#  CMAKE_POLICY(SET CMP0048 OLD)
#ENDIF(POLICY CMP0048)


# Don't enable MACOSX_RPATH
#IF(POLICY CMP0042)
#  CMAKE_POLICY(SET CMP0042 OLD)
#ENDIF(POLICY CMP0042)


# for Opengl and glx
IF(POLICY CMP0072)
  CMAKE_POLICY(SET CMP0072 OLD)
ENDIF(POLICY CMP0072)


# for swig
IF(POLICY CMP0078)
  CMAKE_POLICY(SET CMP0078 OLD)
ENDIF(POLICY CMP0078)
IF(POLICY CMP0086)
  CMAKE_POLICY(SET CMP0086 OLD)
ENDIF(POLICY CMP0086)




##########################################


SET(PROJECT_VERSION_MAJOR 2)
SET(PROJECT_VERSION_MINOR 3)
SET(PROJECT_VERSION_PATCH 0)
SET(PROJECT_VERSION "${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}.${PROJECT_VERSION_PATCH}")



# To get the revision hash from git
# git commit hash macro
execute_process(
  COMMAND git log -1 "--format=format:%H%n" HEAD
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_COMMIT_HASH
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )


SET(FLOWVR_VERSION_REVISION "${PROJECT_VERSION} (GIT Revision ${GIT_COMMIT_HASH})")


MESSAGE("Building FlowVR ${FLOWVR_VERSION_REVISION}.")


##########################################
## TESTING ON
##########################################
ENABLE_TESTING()
INCLUDE(CTest)

SET( BUILD_DASHBOARD_REPORTS 1 ) 
SET( CMAKE_INCLUDE_DIRECTORIES_PROJECT_BEFORE 1)


##########################################
## FLOWVR-PROJECT SETTINGS
##########################################
SET(FlowVR_DIR ${CMAKE_SOURCE_DIR}/flowvr/cmake)

SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${FlowVR_DIR})
INCLUDE(MacroLogFeature) # this is found in the FlowVR_cmake_DIR/cmake 

####################################################################
#General Debug options
OPTION(BUILD_FLOWVR_DEBUG_MESSAGES "Enable -DDEBUG flag" FALSE)

IF (BUILD_FLOWVR_DEBUG_MESSAGES)
  ADD_DEFINITIONS( -DDEBUG )
ENDIF(BUILD_FLOWVR_DEBUG_MESSAGES)
####################################################################

# Warning MESSAGE is CMAKE_BUILD_TYPE not set
IF(NOT CMAKE_BUILD_TYPE)
  MESSAGE("CMAKE_BUILD_TYPE not set. Performance may be affected. Possible values are  Debug, Release, RelWithDebInfo or MinSizeRel\n")
ELSE(NOT CMAKE_BUILD_TYPE)
  MESSAGE("CMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}   (Possible values are  Debug, Release, RelWithDebInfo or MinSizeRel)\n")
ENDIF(NOT CMAKE_BUILD_TYPE)


# Message for APPLE SYSTEM
IF(APPLE)
  MESSAGE("WARNING: FlowVR relies on DYLD_LIBRARY_PATH env var for finding libraires, but not allowed on recent  Mac OS X version. Disable SIP protection  to fix this issue (reboot with Command + R and exec 'csrutil disable; reboot') \n")
ENDIF(APPLE)

####################################################################
## OPTIONAL COMPONENTS TO BE BUILD
####################################################################

OPTION(BUILD_FLOWVR_CONTRIB "Flowvr Contribs contains various components, API extensions, tests, utility tools, etc." FALSE)
MACRO_LOG_FEATURE(BUILD_FLOWVR_CONTRIB "Flowvr Contribs" 
                                       "contrib contains various components, API extensions, tests, utility tools, etc."  
                                       "http://flowvr.sf.net" 
                                       FALSE 
                                       "Optional")


####################################
# Set the components to compile
####################################
SET(components flowvr)

SET(ContribName contrib)
IF(BUILD_FLOWVR_CONTRIB)
  SET(components ${components} ${ContribName})
  SET(FlowVRContrib_DIR ${CMAKE_SOURCE_DIR}/${ContribName}/cmake)
  IF(BUILD_CONTRIB_FLOWVR_VRPN)
    SET(components ${components} flowvr-vrpn)
  ENDIF(BUILD_CONTRIB_FLOWVR_VRPN)

  IF(BUILD_CONTRIB_FLOWVR_IMMERSIVE)
    SET(components ${components} flowvr-immersive)
  ENDIF(BUILD_CONTRIB_FLOWVR_IMMERSIVE)
  
ENDIF(BUILD_FLOWVR_CONTRIB)



#####################################
# Additional components, config files
#####################################


###############################################################################
# Doxygen documentation
OPTION(BUILD_FLOWVR_DOXYGEN "Enable Doxygen doc for FlowVR " FALSE)
MACRO_LOG_FEATURE(BUILD_FLOWVR_DOXYGEN "Flowvr Doxygen generated documentation" 
                                       "Turn on/off Doxygen documentation build"  
                                       "http://flowvr.sf.net" 
                                       FALSE 
                                       "Optional")

IF(BUILD_FLOWVR_DOXYGEN)
   #dependencies to the doc target are added in the CMakeLists.txt of the directories where the doc is actually built (mainly flowvr and contrib)                                    
  ADD_CUSTOM_TARGET(doc ALL)
  
ENDIF(BUILD_FLOWVR_DOXYGEN)




####################################################################
# generate a string enumerating components from ${components} list
# used for flowvr-suite-config.* generation (components_STR is a
# variable used in these scripts, it will be replaced by a bash-
# readable string to loop over enabled components.
####################################################################
STRING(REPLACE ";" " " components_STR "${components}")

# CONFIGURE these scripts
FOREACH(config flowvr-suite-config.sh )
 # create a script in the current binary directory
 SET(INSTALL_PREFIX ${CMAKE_BINARY_DIR}) 
 CONFIGURE_FILE(${config}.in bin/${config} @ONLY) 
  
  # now create one for distribution, here, we replace the used
  # prefix to match the final destination
  SET(INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX})
  
  # call it .dist in the binary-directory
  CONFIGURE_FILE(${config}.in ${config}.dist @ONLY)
  
  # create an installation rule to copy the .dist to the resulting
  # .sh  script in the installation directory
  INSTALL(PROGRAMS ${CMAKE_BINARY_DIR}/${config}.dist
          DESTINATION bin
          RENAME ${config})
ENDFOREACH(config)



##############################################################################
## ENABLE SUBDIRECTORIES / COMPONENTS OF FLOWVR
##############################################################################

FOREACH(component ${components})
  IF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${component})
    ADD_SUBDIRECTORY(${component})
  ENDIF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${component})
ENDFOREACH(component)



################################################################
# packaging
################################################################
SET(CPACK_PACKAGE_VERSION_MAJOR "${PROJECT_VERSION_MAJOR}")
SET(CPACK_PACKAGE_VERSION_MINOR "${PROJECT_VERSION_MINOR}")
SET(CPACK_PACKAGE_VERSION_PATCH "${PROJECT_VERSION_PATCH}")
SET(CPACK_PACKAGE_VENDOR "INRIA - Grenoble University")
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "FlowVR-Suite: the FlowVR middleware and its associated tools for high performance interactive computing")
SET(CPACK_SOURCE_IGNORE_FILES "/CVS/;/.svn/;/build/;/BUILD/;.swp;.#;\#" )
SET(CPACK_RESOURCE_FILE_LICENSE_PROVIDED "0")

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

INCLUDE(InstallRequiredSystemLibraries)
INCLUDE(CPack)

################################################################


#########################################################
# Display FLowVR - List  Enabled/Disabled components 
# (need to be called before to process components to 
# avoid messing-up with project names)
#########################################################
MACRO_DISPLAY_FEATURE_LOG()
