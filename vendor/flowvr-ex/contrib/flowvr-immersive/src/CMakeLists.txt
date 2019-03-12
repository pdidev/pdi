#FILE(GLOB include-files ../include/flowvr-immersive/IO/*.h)
INSTALL(FILES ${include-files} DESTINATION include/flowvr-immersive/IO/)

INCLUDE_DIRECTORIES(../include/  ${FLOWVR_INCLUDE_DIR} ${VRPN_INCLUDES} ${QUAT_INCLUDES})

#SET(forcelm-SRC ForceUtils/vrpnwrapper.cpp ForceUtils/forces.cpp)
#ADD_LIBRARY(forcelm SHARED ${forcelm-SRC})
#TARGET_LINK_LIBRARIES(forcelm ${VRPN_LIBRARY} ${QUAT_LIBRARY} ${FLOWVR_mod_LIBRARY})
#SET_TARGET_PROPERTIES(forcelm PROPERTIES SOVERSION ${PROJECT_VERSION_MAJOR} VERSION ${PROJECT_VERSION})
#INSTALL(TARGETS forcelm LIBRARY DESTINATION lib)


FOREACH(target Tracking Projection Navigation)

  ADD_SUBDIRECTORY(${target})

ENDFOREACH(target)

