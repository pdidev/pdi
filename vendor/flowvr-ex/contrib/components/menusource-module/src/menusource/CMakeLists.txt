# cubesource 

SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/flowvr-contrib/cmake)

FIND_PACKAGE(CEGUI REQUIRED)
FIND_PACKAGE(FlowVRRender REQUIRED)


INCLUDE_DIRECTORIES(${CEGUI_INCLUDE_DIR} 
                    ${FLOWVR_INCLUDE_DIR}
                    ${FLOWVRRENDER_INCLUDE_DIR} )

ADD_SUBDIRECTORY( menusourceconfig )
ADD_SUBDIRECTORY( cegrenderer )
ADD_SUBDIRECTORY( service )
ADD_SUBDIRECTORY( plugs )

SET(menusource-SRC  menusource.cpp )
                    	 

# MODULE  BINARIES
FOREACH(target menusource )
  ADD_EXECUTABLE(${target} ${${target}-SRC})

  TARGET_LINK_LIBRARIES(${target}
  						cegrenderer
  						menusourceconfig
  						${FLOWVR_app_LIBRARY}
  						${FLOWVR_base_LIBRARY} 
                        ${FLOWVR_ftl_LIBRARY} 
                        ${FLOWVR_mod_LIBRARY}
                        ${FLOWVR_portutils_LIBRARY}                          
                        ${FLOWVRRENDER_LIBRARY}
                        ${FLOWVR_BALZAC_AUX_LIBRARY}
                        ${CMAKE_THREAD_LIBS_INIT} )
  INSTALL(TARGETS ${target}  DESTINATION ${MODULE_BIN_DIR})	
ENDFOREACH(target)

