#
# Find fvmoduleapi  contrib (part of FLOWVR but not necessarily compiled and installed)
#
# fvmoduleapi_INCLUDE_DIRECTORY
# fvmoduleapi_LIBRARY        
# fvmoduleapi_FOUND         


FIND_PACKAGE(FlowVR)

find_path(fvmoduleapi_INCLUDE_DIRECTORY
   NAMES     fvmoduleapi.h
   PATHS  ${FLOWVR_base_INCLUDE_DIR}/fvmoduleapi
   NO_DEFAULT_PATH
)

find_library(fvmoduleapi_LIBRARY
  NAMES fvmoduleapi
  PATHS
  ENV LD_LIBRARY_PATH
  ENV LIBRARY_PATH
  NO_DEFAULT_PATH
)

if ( fvmoduleapi_INCLUDE_DIRECTORY AND fvmoduleapi_LIBRARY )
  set( fvmoduleapi_FOUND TRUE )
else ( fvmoduleapi_INCLUDE_DIRECTORY AND fvmoduleapi_LIBRARY )
  set( fvmoduleapi_FOUND FALSE )
endif ( fvmoduleapi_INCLUDE_DIRECTORY AND fvmoduleapi_LIBRARY )

