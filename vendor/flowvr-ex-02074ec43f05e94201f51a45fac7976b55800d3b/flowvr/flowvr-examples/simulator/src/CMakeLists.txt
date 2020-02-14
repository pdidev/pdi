# SIMULATOR EXAMPLE 

INCLUDE_DIRECTORIES(../include ../vcg/ ../vcg/wrap/gui ${FLOWVR_INCLUDE_DIR})

# MODULE  BINARIES

IF(BUILD_MODULES)
  MESSAGE(STATUS "Starting discrete modules construction...")
  INCLUDE_DIRECTORIES(${OPENGL_INCLUDE_DIR} ${GLUT_INCLUDE_DIR})
  
  #FOREACH(target engine visualization)
    MESSAGE(STATUS "Constructing engine module...")
    # Engine construction
    ADD_EXECUTABLE(engine engine.cpp)
    # Should work but Linux complains about not finding dl lib used by flowvr-plugd
    #  TARGET_LINK_LIBRARIES(${target} ${FLOWVR_LIBRARY} ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES})
    TARGET_LINK_LIBRARIES(engine  ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES} ${FLOWVR_base_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_mod_LIBRARY} ${CMAKE_THREAD_LIBS_INIT} )
    INSTALL(TARGETS engine  DESTINATION ${MODULE_DIR})
    MESSAGE(STATUS "Done building engine.")	
  #ENDFOREACH(target)

    #Constructing visualization
    MESSAGE(STATUS "Constructing visualization module...")
    ADD_EXECUTABLE(visualization visualization.cpp trackmode.cpp trackball.cpp)
    TARGET_LINK_LIBRARIES(visualization  ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES} ${FLOWVR_base_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_mod_LIBRARY} ${CMAKE_THREAD_LIBS_INIT} )
    INSTALL(TARGETS visualization  DESTINATION ${MODULE_DIR})

  MESSAGE(STATUS "Done building visualization.")
ENDIF(BUILD_MODULES)

MESSAGE(STATUS "End of sources script.")
