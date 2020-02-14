SET(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/../cmake ${CMAKE_MODULE_PATH} ) # Use namespace to define directory: /share/name/space/cmake



# For each contribution add subdirectory

# settinFreq Module
OPTION(BUILD_CONTRIB_SETTINGFREQ_MOD "Module with graphical interface to control frequency throught the flowvr.plugins.VariableFrequencySynchronizor.cpp synchronizor" FALSE)
MARK_AS_ADVANCED(BUILD_CONTRIB_SETTINGFREQ_MOD)


IF (BUILD_CONTRIB_SETTINGFREQ_MOD) 
  ADD_SUBDIRECTORY(setting-freq-module)
ENDIF (BUILD_CONTRIB_SETTINGFREQ_MOD) 
# Feature logged once all dependencies have been checked.
MACRO_LOG_FEATURE(BUILD_CONTRIB_SETTINGFREQ_MOD "FlowVR Contrib: settingFreq module" "GLUI based module frquency control" "http://flowvr.sf.net" FALSE "Optional")

# joypad Module
OPTION(BUILD_CONTRIB_JOYPAD_MOD "joypad module" TRUE)
MARK_AS_ADVANCED(BUILD_CONTRIB_JOYPAD_MOD)

# Only compile  and install if linux system:
STRING(COMPARE EQUAL ${CMAKE_SYSTEM_NAME} Linux LINUX)

# Missing dependency: turn option off
IF (NOT LINUX) 
  SET(BUILD_CONTRIB_JOYPAD_MOD OFF CACHE BOOL "joypad module turned off: missing dependency" FORCE)
  MESSAGE(STATUS "FlowVR Contrib: joypad module only supported on  Linux:  BUILD_CONTRIB_JOYPAD_MOD option turned OFF")
ENDIF(NOT LINUX) 
 
IF (BUILD_CONTRIB_JOYPAD_MOD  AND LINUX) 
  ADD_SUBDIRECTORY(joypad-module)
ENDIF (BUILD_CONTRIB_JOYPAD_MOD  AND LINUX)
MACRO_LOG_FEATURE(BUILD_CONTRIB_JOYPAD_MOD "FlowVR Contrib: joypad  modules" "Only linux support" "http://flowvr.sf.net" FALSE "Optional")

###############################################################################
## MENU SOURCE
###############################################################################

OPTION(BUILD_CONTRIB_CEGUI_MENU_MOD "CEGUI based Menusource" FALSE)
MARK_AS_ADVANCED(BUILD_CONTRIB_CEGUI_MENU_MOD)

IF(BUILD_CONTRIB_CEGUI_MENU_MOD)
    ADD_SUBDIRECTORY(menusource-module)
ENDIF(BUILD_CONTRIB_CEGUI_MENU_MOD)
