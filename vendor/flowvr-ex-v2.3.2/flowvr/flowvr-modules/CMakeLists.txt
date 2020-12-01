PROJECT(flowvr_modules VERSION "${FLOWVR_MAIN_VERSION}")


# INSTALLATION DIRECTORIES FOR BINARIES
SET(MODULE_BIN_DIR share/flowvr/modules/bin)

# Module Binary 
ADD_SUBDIRECTORY(src/fread-fwrite)

# Python module
ADD_SUBDIRECTORY(src/spy_module)

