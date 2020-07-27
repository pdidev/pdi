INCLUDE_DIRECTORIES(../../include/ ${FLOWVR_INCLUDE_DIR} ${VRPN_INCLUDES} ${QUAT_INCLUDES})


ADD_EXECUTABLE(flowvr_forcefield flowvr_forcefield.cpp ../ForceUtils/forces.cpp ../ForceUtils/vrpnwrapper.cpp)
#ADD_EXECUTABLE(flowvr_forcefield flowvr_forcefield.cpp)

TARGET_LINK_LIBRARIES(flowvr_forcefield ${FLOWVR_base_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_mod_LIBRARY} ${VRPN_LIBRARY} ${QUAT_LIBRARY})

INSTALL(TARGETS flowvr_forcefield  DESTINATION ${MODULE_DIR})

