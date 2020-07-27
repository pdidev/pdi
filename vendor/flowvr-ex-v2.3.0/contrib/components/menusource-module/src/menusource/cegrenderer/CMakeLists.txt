FIND_PACKAGE(CEGUI REQUIRED)

INCLUDE_DIRECTORIES(${CEGUI_INCLUDE_DIR}
                    ${FLOWVR_INCLUDE_DIR}
                    ${FLOWVRRENDER_INCLUDE_DIR} )
                    

# SOURCE FILES
 # 
SET(cegrenderer-SRC  cegrenderer.cpp  
                     cegtexture.cpp 
                     cegtexturetarget.cpp
                     ceggeometrybuffer.cpp
                     cegrendertarget.cpp )

# creating library cegrenderer
FOREACH(target cegrenderer)
      ADD_LIBRARY(${target} SHARED ${${target}-SRC})
      TARGET_LINK_LIBRARIES(${target}
                            ${CEGUI_LIBRARIES}
                      		${FLOWVRRENDER_LIBRARY} 
                            ${CMAKE_THREAD_LIBS_INIT} )
                            

	  SET_TARGET_PROPERTIES(${target} PROPERTIES
	                        SOVERSION ${PROJECT_VERSION_MAJOR} 
	                        VERSION ${PROJECT_VERSION})
	                                              
      INSTALL(TARGETS ${target}  LIBRARY DESTINATION ${COMP_LIB_DIR})	
ENDFOREACH(target)

