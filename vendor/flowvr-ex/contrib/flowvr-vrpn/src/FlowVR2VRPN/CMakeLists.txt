INCLUDE_DIRECTORIES(../../include/ ${FLOWVR_INCLUDE_DIR} ${VRPN_INCLUDES} ${QUAT_INCLUDES})      

SET(flowvr2vrpn flowvr2vrpn.cpp ../ForceUtils/forces.cpp ../ForceUtils/vrpnwrapper.cpp)

ADD_EXECUTABLE(flowvr2vrpn ${flowvr2vrpn})

TARGET_LINK_LIBRARIES(flowvr2vrpn ${FLOWVR_base_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_mod_LIBRARY} ${VRPN_LIBRARY} ${QUAT_LIBRARY})

INSTALL(TARGETS flowvr2vrpn  DESTINATION ${MODULE_DIR})

