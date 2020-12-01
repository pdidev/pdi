FIND_PACKAGE(FlowVR REQUIRED)

# each cpp is a plugin of its own
# glob all files ending with cpp in this directory
FILE(GLOB PLUGS-SRC *.cpp )

INCLUDE_DIRECTORIES( ../../include ${FLOWVR_INCLUDE_DIR}  )


SET( segmentTest.comp-DEPS ${FLOWVR_base_LIBRARY} 
                      ${FLOWVR_mod_LIBRARY}  
                      ${FLOWVR_ftl_LIBRARY}    
                      ${FLOWVR_utils_LIBRARY} 
                      ${FLOWVR_portutils_comp_LIBRARY}
                      ${FLOWVR_app_LIBRARY}   
                      ${FLOWVR_app_comp_LIBRARY} )
                      
# ${plug} contains the full build path to the source code 
FOREACH(plug ${PLUGS-SRC})
 MESSAGE( STATUS "file: " ${plug} )
 # strip plug-name from plug (full path)
 # find plugin name:
 # search for:
 # 'any prefix' ( ".*" -> match any character any number of times ) until reached a 'slash'
 # that has a suffix '.cpp' at the end
 # now: capture the name (any character up to the point before 'cpp') in \1
 # store in plug-name, take from plug
 STRING( REGEX REPLACE ".*/(.*)[.]cpp$" "\\1" plug-name ${plug} )
 MESSAGE( STATUS "plug: " ${plug-name} )
 ADD_LIBRARY(${plug-name} SHARED ${plug})
 #SET_TARGET_PROPERTIES(${plug-name} PROPERTIES
 #                     SOVERSION ${PROJECT_VERSION_MAJOR} 
 #                     VERSION ${PROJECT_VERSION})

  # we assume that any plugin needs msg types defined for balzac now and then                      
  TARGET_LINK_LIBRARIES( ${plug-name} ${FLOWVR_portutils_LIBRARY} ${${plug-name}-DEPS} )
                      
 INSTALL(TARGETS ${plug-name} DESTINATION share/flowvr-contrib/lib 
         PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
ENDFOREACH(plug)


 INSTALL(FILES segment-pull.xml 
               segment-push.xml
               segment-test-stamps.xml
         DESTINATION share/flowvr-contrib/config
         PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
         
 INSTALL(DIRECTORY parameters
         DESTINATION share/flowvr-contrib/tests/segment-test
         REGEX ".svn" EXCLUDE
         PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ )
