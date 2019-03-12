INCLUDE_DIRECTORIES(../../include/flowvr-vrpn/core/ ${FLOWVR_INCLUDE_DIR} ${VRPN_INCLUDES} ${QUAT_INCLUDES})      

ADD_EXECUTABLE(flowvr_button flowvr_button.cpp)

ADD_EXECUTABLE(flowvr_analog flowvr_analog.cpp)

ADD_EXECUTABLE(flowvr_tracker flowvr_tracker.cpp)

FOREACH(target flowvr_button flowvr_analog flowvr_tracker)
  TARGET_LINK_LIBRARIES(${target} ${FLOWVR_base_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_mod_LIBRARY} ${VRPN_LIBRARY} ${QUAT_LIBRARY})
  INSTALL(TARGETS ${target}  DESTINATION ${MODULE_DIR})
ENDFOREACH(target)

