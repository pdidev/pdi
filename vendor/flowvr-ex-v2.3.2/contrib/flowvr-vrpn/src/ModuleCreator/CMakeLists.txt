INCLUDE_DIRECTORIES(../../include/flowvr-vrpn/core/ ${FLOWVR_INCLUDE_DIR} )

ADD_EXECUTABLE(modulecreator modulecreator.cpp deviceparser)

TARGET_LINK_LIBRARIES(modulecreator ${FLOWVR_base_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_mod_LIBRARY} ${FLOWVR_parse_LIBRARY} )

INSTALL(TARGETS modulecreator  DESTINATION ${MODULE_DIR})

