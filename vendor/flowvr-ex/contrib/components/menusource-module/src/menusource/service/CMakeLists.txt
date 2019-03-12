FIND_PACKAGE( FlowVR       REQUIRED )
FIND_PACKAGE( FlowVRRender REQUIRED )
FIND_PACKAGE( CEGUI        REQUIRED)

SET( lib-name menuservice )

INCLUDE_DIRECTORIES( ${PORTUTILS_INCLUDE_DIR} ../ )
               
ADD_LIBRARY(${lib-name} SHARED menuservice.cpp )

TARGET_LINK_LIBRARIES(${lib-name}
                      cegrenderer
                      menusourceconfig
                      ${CEGUI_LIBRARIES}
                      ${FLOWVR_base_LIBRARY} 
                      ${FLOWVR_mod_LIBRARY} 
                      ${FLOWVR_ftl_LIBRARY}
                      ${FLOWVR_render_LIBRARY}
                      ${FLOWVR_portutils_LIBRARY})
                      
INSTALL(TARGETS ${lib-name} LIBRARY DESTINATION ${COMP_LIB_DIR} )
MESSAGE( STATUS "Will install menuservice to ${COMP_LIB_DIR}" )