# - Try to find GLUI
# Once done this will define
#
#  GLUI_FOUND - system has glui
#  GLUI_INCLUDES - the glui include directory
#  GLUI_LIBRARY - Link these to use glui


FIND_LIBRARY (GLUI_LIBRARY NAMES glui
    PATHS 
    ENV LD_LIBRARY_PATH
    ENV LIBRARY_PATH
    /usr/lib64
    /usr/lib
    /usr/local/lib64
    /usr/local/lib
    /opt/local/lib
    )
FIND_PATH (GLUI_INCLUDES glui.h
    PATHS
    ENV CPATH
    /usr/include
    /usr/include/glui
    /usr/local/include
    /opt/local/include
    PATH_SUFFIXES GL
    )

IF(GLUI_INCLUDES AND GLUI_LIBRARY)
    SET(GLUI_FOUND TRUE)
ENDIF(GLUI_INCLUDES AND GLUI_LIBRARY)

IF(GLUI_FOUND)
  IF(NOT GLUI_FIND_QUIETLY)
    MESSAGE(STATUS "GLUI Found: ${GLUI_LIBRARY}  ${GLUI_LIBRARY}")
  ENDIF(NOT GLUI_FIND_QUIETLY)
ELSE(GLUI_FOUND)
  IF(GLUI_FIND_REQUIRED)
    MESSAGE(FATAL_ERROR "Could not find GLUI")
  ENDIF(GLUI_FIND_REQUIRED)
ENDIF(GLUI_FOUND)

