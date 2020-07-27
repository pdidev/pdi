#Pour les exemples il va falloir lancer le make-target.sh pour compiler directement dans le répertoire d'installation.

FOREACH(target sphere_phantom sphere_joypad)
  INSTALL(DIRECTORY  ${target}
    DESTINATION  share/flowvr-vrpn/examples/
    PATTERN "*imulated*" EXCLUDE
    PATTERN "sphere_joypad.comp.cpp" EXCLUDE
    PATTERN "sphere_phantom.comp.cpp" EXCLUDE
    PATTERN ".svn" EXCLUDE)
   
  INSTALL(PROGRAMS ${target}/make-${target}.sh 
    DESTINATION share/flowvr-vrpn/examples/${target}/)
ENDFOREACH(target)



IF(BUILD_FLOWVR_VRPN_SIMULATED_INTERFACE)
  INSTALL(FILES sphere_joypad/src/sphere_simulatedjoypad.comp.cpp
    DESTINATION share/flowvr-vrpn/examples/sphere_joypad/src/
    RENAME sphere_joypad.comp.cpp)
  INSTALL(FILES sphere_joypad/src/SimulatedJoypad.comp.cpp
    DESTINATION share/flowvr-vrpn/examples/sphere_joypad/src/)
  INSTALL(FILES sphere_joypad/include/sphere_joypad/components/SimulatedJoypad.comp.h
    DESTINATION  share/flowvr-vrpn/examples/sphere_joypad/include/sphere_joypad/components/)

  INSTALL(FILES sphere_phantom/src/sphere_simulatedphantom.comp.cpp
    DESTINATION share/flowvr-vrpn/examples/sphere_phantom/src/
    RENAME sphere_phantom.comp.cpp)
  INSTALL(FILES sphere_phantom/src/SimulatedPhantom.comp.cpp
    DESTINATION share/flowvr-vrpn/examples/sphere_phantom/src/)
  INSTALL(FILES sphere_phantom/include/sphere_phantom/components/SimulatedPhantom.comp.h
    DESTINATION  share/flowvr-vrpn/examples/sphere_phantom/include/sphere_phantom/components/)

ELSE(BUILD_FLOWVR_VRPN_SIMULATED_INTERFACE)
  INSTALL(FILES sphere_joypad/src/sphere_joypad.comp.cpp
    DESTINATION share/flowvr-vrpn/examples/sphere_joypad/src/)
  
  INSTALL(FILES sphere_phantom/src/sphere_phantom.comp.cpp
    DESTINATION share/flowvr-vrpn/examples/sphere_phantom/src/)
  
ENDIF(BUILD_FLOWVR_VRPN_SIMULATED_INTERFACE)

