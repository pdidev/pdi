# INSTALLATION DIRECTORIES
SET(COMP_INCLUDE_DIR include/flowvr/contrib/components)
SET(COMP_LIB_DIR lib/flowvr/components)
SET(MODULE_BIN_DIR share/flowvr/modules/bin)



# Install .h component files
INSTALL(FILES include/flowvr/contrib/components/metamodulejoypad.comp.h 
  include/flowvr/contrib/components/modulejoypad.comp.h 
  DESTINATION ${COMP_INCLUDE_DIR})

# Module Binary 
ADD_SUBDIRECTORY(src) 






