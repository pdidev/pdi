IF(BUILD_FLOWVR_VRPN_SIMULATED_INTERFACE)

  INCLUDE_DIRECTORIES(../../include/flowvr-vrpn/core/ ${FLOWVR_INCLUDE_DIR} ${VRPN_INCLUDES} ${QUAT_INCLUDES} ${GLUI_INCLUDES})
  
  SET(SimulatedDevice
    Axe.cpp  
    Button.cpp  
    Element.cpp  
    InputDevice.cpp  
    Stick.cpp  
    Tracker.cpp
    SimulatedDevice.cpp)

  ADD_EXECUTABLE(simulateddevice ${SimulatedDevice})
  

  
  TARGET_LINK_LIBRARIES(simulateddevice  ${GLUI_LIBRARY} ${FLOWVR_base_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_mod_LIBRARY} ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES} ${VRPN_LIBRARY} ${QUAT_LIBRARY})
  
  INSTALL(TARGETS simulateddevice  DESTINATION ${MODULE_DIR})
ENDIF(BUILD_FLOWVR_VRPN_SIMULATED_INTERFACE)
