# - Try to find GLUI
# Once done this will define
#
#  GLUI_FOUND - system has glui
#  GLUI_INCLUDES - the glui include directory
#  GLUI_LIBRARY - Link these to use glui


FIND_LIBRARY (GLUI_LIBRARY_EXT NAMES glui
    PATHS 
    ENV LD_LIBRARY_PATH
    ENV LIBRARY_PATH
    /usr/lib64
    /usr/lib
    /usr/local/lib64
    /usr/local/lib
    /opt/local/lib
    )
FIND_PATH (GLUI_INCLUDES_EXT glui.h
    PATHS
    ENV CPATH
    /usr/include
    /usr/local/include
    /opt/local/include
    PATH_SUFFIXES GL
    )

IF(GLUI_INCLUDES_EXT AND GLUI_LIBRARY_EXT)
    SET(GLUI_EXT_FOUND TRUE)
ENDIF(GLUI_INCLUDES_EXT AND GLUI_LIBRARY_EXT)

IF(GLUI_EXT_FOUND)
  IF(NOT GLUI_EXT_FIND_QUIETLY)
    MESSAGE(STATUS "GLUI Found: ${GLUI_LIBRARY_EXT}  ${GLUI_LIBRARY_EXT}")
  ENDIF(NOT GLUI_EXT_FIND_QUIETLY)
ELSE(GLUI_EXT_FOUND)
  IF(GLUI_EXT_FIND_REQUIRED)
    MESSAGE(FATAL_ERROR "Could not find GLUI")
  ENDIF(GLUI_EXT_FIND_REQUIRED)
ENDIF(GLUI_EXT_FOUND)

