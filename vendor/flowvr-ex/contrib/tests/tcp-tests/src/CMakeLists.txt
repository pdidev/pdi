FIND_PACKAGE(FlowVR REQUIRED)

# each cpp is a plugin of its own
# glob all files ending with cpp in this directory
FILE(GLOB PLUGS-SRC *.cpp )

INCLUDE_DIRECTORIES( ../include ${FLOWVR_INCLUDE_DIR})

SET( filterandtest.comp-DEPS 
      ${FLOWVR_app_LIBRARY}   
      ${FLOWVR_app_comp_LIBRARY} 
     ${FLOWVR_portutils_comp_LIBRARY}
     )


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

  # we assume that any plugin needs msg types defined for balzac now and then                      
  TARGET_LINK_LIBRARIES( ${plug-name} ${${plug-name}-DEPS} ${FLOWVR_portutils_LIBRARY}  )
                      
 INSTALL(TARGETS ${plug-name} DESTINATION share/flowvr-contrib/tests/tcp-tests/lib 
         PERMISSIONS OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ)
ENDFOREACH(plug)

