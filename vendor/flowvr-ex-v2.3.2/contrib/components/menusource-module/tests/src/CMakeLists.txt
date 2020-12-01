cmake_minimum_required(VERSION 2.8)
# each cpp is a component of its own
# glob all files ending with cpp in this directory
FILE(GLOB COMPS-SRC *.cpp )

INCLUDE_DIRECTORIES( ${FLOWVR_INCLUDE_DIR}                            
                     ${FLOWVRRENDER_INCLUDE_DIR} 
                     ../../include
                     ${BALZAC_DIR}/tests )

SET( simplemenu.comp-DEPS menusource.comp )


# ${comp} contains the full build path to the source code 
FOREACH(comp ${COMPS-SRC})
 MESSAGE( STATUS "file: " ${comp} )
 # strip comp-name from comp (full path)
 # find compin name:
 # search for:
 # 'any prefix' ( ".*" -> match any character any number of times ) until reached a 'slash'
 # that has a suffix '.cpp' at the end
 # now: capture the name (any character up to the point before 'cpp') in \1
 # store in comp-name, take from comp
 STRING( REGEX REPLACE ".*/(.*)[.]cpp$" "\\1" comp-name ${comp} )
 MESSAGE( STATUS "comp: " ${comp-name} )
 ADD_LIBRARY(${comp-name} SHARED ${comp})
 SET_TARGET_PROPERTIES(${comp-name} PROPERTIES
                      SOVERSION ${PROJECT_VERSION_MAJOR} 
                      VERSION ${PROJECT_VERSION})
                       
TARGET_LINK_LIBRARIES(${comp-name} 
                      ${FLOWVR_portutils_LIBRARY}
                      ${FLOWVR_BALZAC_COMP_LIBRARY}
                      ${FLOWVR_base_LIBRARY} 
                      ${FLOWVR_mod_LIBRARY}  
                      ${FLOWVR_ftl_LIBRARY}    
                      ${FLOWVR_app_LIBRARY}   
                      ${FLOWVR_app_comp_LIBRARY} 
                      ${${comp-name}-DEPS} )
                      
 INSTALL(TARGETS ${comp-name} DESTINATION share/flowvr-contrib/menusource/tests/components 
         PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ )
         
ENDFOREACH(comp)


INSTALL(DIRECTORY config  DESTINATION ${COMP_INSTALL_DIR}/menusource/config FILES_MATCHING PATTERN "*.xml" REGEX ".svn" EXCLUDE)
INSTALL(DIRECTORY config  DESTINATION ${COMP_INSTALL_DIR}/menusource/config FILES_MATCHING PATTERN "*.prm" REGEX ".svn" EXCLUDE)
INSTALL(DIRECTORY config  DESTINATION ${COMP_INSTALL_DIR}/menusource/config FILES_MATCHING PATTERN "*.mtx" REGEX ".svn" EXCLUDE)
