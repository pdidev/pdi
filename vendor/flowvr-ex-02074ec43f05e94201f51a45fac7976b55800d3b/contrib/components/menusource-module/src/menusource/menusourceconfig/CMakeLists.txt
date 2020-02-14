FIND_PACKAGE( CEGUI  REQUIRED )

SET( lib-name menusourceconfig )

INCLUDE_DIRECTORIES( ${PORTUTILS_INCLUDE_DIR} ../ )
               
ADD_LIBRARY(${lib-name} SHARED menusourceconfig )

TARGET_LINK_LIBRARIES(${lib-name}
                      ${CEGUI_LIBRARIES} )
                      
INSTALL(TARGETS ${lib-name} LIBRARY DESTINATION ${COMP_LIB_DIR} )