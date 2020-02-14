INCLUDE_DIRECTORIES(../../../flowvr-vrpn/include/flowvr-vrpn/core/ ../../include/IO/ ${FLOWVR_INCLUDE_DIR} ${VRPN_INCLUDES} ${QUAT_INCLUDES})      

ADD_EXECUTABLE(tracking ComputeTracking.cpp ../IO/parseXML.cpp)

FOREACH(target tracking)
  TARGET_LINK_LIBRARIES(${target} ${FLOWVR_base_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_mod_LIBRARY} ${VRPN_LIBRARY} ${QUAT_LIBRARY})
  INSTALL(TARGETS ${target}  DESTINATION ${MODULE_DIR})
ENDFOREACH(target)

