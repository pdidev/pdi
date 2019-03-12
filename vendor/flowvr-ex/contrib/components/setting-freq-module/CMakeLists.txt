# INSTALLATION DIRECTORIES
SET(COMP_INCLUDE_DIR include/flowvr/contrib/components)
SET(COMP_LIB_DIR lib/flowvr/components)
SET(MODULE_BIN_DIR share/flowvr/modules/bin)
SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${FlowVR_DIR})
#Dependencies 

#CHECK OPENGL  DEPENDENCY
FIND_PACKAGE(OpenGL)
MACRO_LOG_FEATURE(OPENGL_FOUND "OpenGL" "" "http://www.opengl.org"  TRUE "Required by SettingFreq component" )

#CHECK GLUT  DEPENDENCY
FIND_PACKAGE(GLUT)
MACRO_LOG_FEATURE(GLUT_FOUND "Glut" "The OpenGL Utility Toolkit" "http://freeglut.sourceforge.net" TRUE "Required by SettingFreq component")
#CHECK GLUI DEPENDENCY
FIND_PACKAGE(GLUI)
MACRO_LOG_FEATURE(GLUI_FOUND "GLUI" "OpenGL User Interface" "http://glui.sourceforge.net" TRUE "Required By SettingFreq component")


# Install .h component files
INSTALL(FILES include/flowvr/contrib/components/metamoduleSettingFreq.comp.h 
              include/flowvr/contrib/components/moduleSettingFreq.comp.h 
              DESTINATION ${COMP_INCLUDE_DIR})
  
 
# Module Binary 
ADD_SUBDIRECTORY(src)
