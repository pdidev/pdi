
INCLUDE_DIRECTORIES(../include ${FLOWVR_INCLUDE_DIR})






# MODULE  BINARIES
IF(BUILD_MODULES)

  INCLUDE_DIRECTORIES(${OPENGL_INCLUDE_DIR} ${GLUT_INCLUDE_DIR})
 
   # density filtering
  ADD_EXECUTABLE(filter-density  filter-density.cpp )
  TARGET_LINK_LIBRARIES(filter-density  ${FLOWVR_base_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_mod_LIBRARY} ${CMAKE_THREAD_LIBS_INIT})
  INSTALL(TARGETS filter-density  DESTINATION ${MODULE_DIR})	

   # isoline computation
  ADD_EXECUTABLE(isolines  isolines.cpp )
  TARGET_LINK_LIBRARIES(isolines  ${FLOWVR_base_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_mod_LIBRARY} ${CMAKE_THREAD_LIBS_INIT})
  INSTALL(TARGETS isolines  DESTINATION ${MODULE_DIR})	

   # density/velocity visu
  ADD_EXECUTABLE(gldens  gldens.cpp)
  TARGET_LINK_LIBRARIES(gldens   ${FLOWVR_base_LIBRARY}  ${FLOWVR_ftl_LIBRARY} ${FLOWVR_mod_LIBRARY} ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} )
  INSTALL(TARGETS gldens  DESTINATION ${MODULE_DIR})	
 
   # parallel fluid (same source but set USE_MPI variable)
   INCLUDE_DIRECTORIES(${MPI_CXX_INCLUDE_PATH})
   ADD_EXECUTABLE(fluid_insitu fluid.cpp Vec3D.cpp Turbulent.cpp TurbulentBase.cpp )
   TARGET_LINK_LIBRARIES(fluid_insitu  ${FLOWVR_base_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_mod_LIBRARY} ${MPI_CXX_LIBRARIES} ${MPI_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT}) # issue on mac with openmpi package installed from port: MPI_CXX_LIBRARIES does not include /opt/local/lib/libmpi_cxx.dylib. Need to includ it manually.
   INSTALL(TARGETS fluid_insitu  DESTINATION ${MODULE_DIR})	


ENDIF(BUILD_MODULES)
