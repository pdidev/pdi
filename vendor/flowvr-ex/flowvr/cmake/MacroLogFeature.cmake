# This file defines the Feature Logging macros.
#
# MACRO_LOG_FEATURE(VAR FEATURE DESCRIPTION URL [REQUIRED [COMMENTS [MIN_VERSION]]])
#   Logs the information so that it can be displayed at the end
#   of the configure run
#   VAR : TRUE or FALSE, indicating whether the feature is supported
#   FEATURE: name of the feature, e.g. "libjpeg"
#   DESCRIPTION: description what this feature provides
#   URL: home page
#   REQUIRED: TRUE or FALSE, indicating whether the featue is required
#   COMMENTS: More info you may want to provide.  empty string if unnecessary
#   MIN_VERSION: minimum version number. empty string if unneeded
#
# MACRO_DISPLAY_FEATURE_LOG()
#   Call this to display the collected results.
#   Exits CMake with a FATAL error message if a required feature is missing
#
# Example:
#
# INCLUDE(MacroLogFeature)
#
# FIND_PACKAGE(JPEG)
# MACRO_LOG_FEATURE(JPEG_FOUND "libjpeg" "Support JPEG images" "http://www.ijg.org" TRUE "3.2a" "")
# ...
# MACRO_DISPLAY_FEATURE_LOG()



SET( missing  ${CMAKE_BINARY_DIR}/FeatureMissing_${PROJECT_NAME}.txt )
SET( enabled  ${CMAKE_BINARY_DIR}/FeatureEnabled_${PROJECT_NAME}.txt )
SET( disabled ${CMAKE_BINARY_DIR}/FeatureDisabled_${PROJECT_NAME}.txt )


#IF (NOT _macroLogFeatureAlreadyIncluded)
  FOREACH(_file ${missing} ${enabled} ${disabled}) 
    IF (EXISTS ${_file})
      FILE(REMOVE ${_file})
    ENDIF (EXISTS ${_file})
  ENDFOREACH(_file ${missing} ${enabled} ${disabled}) 
#  SET(_macroLogFeatureAlreadyIncluded TRUE)
#ENDIF (NOT _macroLogFeatureAlreadyIncluded)


MACRO(MACRO_LOG_FEATURE _var _package _description _url  _required)
SET(_comments "${ARGV5}")
SET(_minvers "${ARGV6}")


   IF (${_var})
     SET(_LOGFILENAME ${enabled} )
   ELSE (${_var})
     IF (${_required} MATCHES "[Tt][Rr][Uu][Ee]")
       SET(_LOGFILENAME ${missing})
     ELSE (${_required} MATCHES "[Tt][Rr][Uu][Ee]")
       SET(_LOGFILENAME  ${disabled})
     ENDIF (${_required} MATCHES "[Tt][Rr][Uu][Ee]")
   ENDIF (${_var})

   IF (NOT EXISTS ${_LOGFILENAME})
     FILE(WRITE ${_LOGFILENAME} "\n")
   ENDIF (NOT EXISTS ${_LOGFILENAME})

   FILE(APPEND ${_LOGFILENAME} "   =======================================\n")
   FILE(APPEND ${_LOGFILENAME} "   PACKAGE:     ${_package}\n")
   FILE(APPEND ${_LOGFILENAME} "   DESCRIPTION: ${_description}\n")
#   IF (${_url} MATCHES ".*")
#     FILE(APPEND ${_LOGFILENAME} "   URL:         ${_url}\n")
#   ENDIF (${_url} MATCHES ".*")
#   IF (${_minvers} MATCHES ".*")
#     FILE(APPEND ${_LOGFILENAME} "   VERSION:     ${_minvers}\n")
#   ENDIF (${_minvers} MATCHES ".*")
   IF (${_comments} MATCHES ".*")
     FILE(APPEND ${_LOGFILENAME} "   COMMENTS:    ${_comments}\n")
   ENDIF (${_comments} MATCHES ".*")
 
ENDMACRO(MACRO_LOG_FEATURE)


MACRO(MACRO_DISPLAY_FEATURE_LOG)

  SET(_file ${missing})
  IF (EXISTS ${_file})
    SET( missingreq TRUE)
    FILE(APPEND ${_file} "   =======================================")
    FILE(READ ${_file} _requirements)
    MESSAGE("\n${PROJECT_NAME} Missing Requirements (Compilation will fail). Fix issue or turn off  BUILD_${PROJECT_NAME} to disable compilation):${_requirements}")
    FILE(REMOVE ${_file})
  ENDIF (EXISTS ${_file})
  
  SET(_file ${enabled}  )
  IF (EXISTS ${_file})
     FILE(APPEND ${_file} "   =======================================")
     FILE(READ ${_file} _enabled)
     MESSAGE("\n${PROJECT_NAME} Enabled  Features and  Resolved Dependencies:${_enabled}")
#     MESSAGE(STATUS "\n${PROJECT_NAME} Enabled  Features and  Resolved Dependencies:${_enabled}") # displayed in the  ccmake progress line (impossible to read)
     FILE(REMOVE ${_file})
  ENDIF (EXISTS ${_file})
  
  SET(_file ${disabled} )
  IF (EXISTS ${_file})
     FILE(APPEND ${_file} "   =======================================")
     FILE(READ ${_file} _disabled)
     MESSAGE("\n${PROJECT_NAME} Disabled Features:${_disabled}")#This version should be better (message clearly displayed but wirtten on stderrr instead of stdout with cmake. A cmake bug ?
#     MESSAGE(STATUS "\n${PROJECT_NAME} Disabled Features:${_disabled}") # displayed in the  ccmake progress line (impossible to read)
     FILE(REMOVE ${_file})
  ENDIF (EXISTS ${_file})

IF ( missingreq)
  MESSAGE(FATAL_ERROR "Exit: ${PROJECT_NAME} missing requirements")
ENDIF ( missingreq)

ENDMACRO(MACRO_DISPLAY_FEATURE_LOG)
