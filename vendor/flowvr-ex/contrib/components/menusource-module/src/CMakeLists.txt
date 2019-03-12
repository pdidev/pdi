ADD_SUBDIRECTORY(menusource)

INCLUDE_DIRECTORIES(../include/menurender/components ../include/menusource/components ${FLOWVR_INCLUDE_DIR})

SET(menurender.comp-SRC  menurender.comp.cpp)

FOREACH(target menurender.comp)
      ADD_LIBRARY(${target} SHARED ${${target}-SRC})
      TARGET_LINK_LIBRARIES(${target}
                            ${FLOWVR_base_LIBRARY}
                            ${FLOWVR_app_LIBRARY}
                            ${FLOWVR_app_comp_LIBRARY}
                      		${FLOWVR_mod_LIBRARY} 
                      		${FLOWVR_ftl_LIBRARY}
                            ${CMAKE_THREAD_LIBS_INIT})
                            

	  SET_TARGET_PROPERTIES(${target} PROPERTIES
	                        SOVERSION ${PROJECT_VERSION_MAJOR} 
	                        VERSION ${PROJECT_VERSION})
	                                              
      INSTALL(TARGETS ${target}  LIBRARY DESTINATION ${COMP_LIB_DIR})	
ENDFOREACH(target)

ADD_SUBDIRECTORY( components )