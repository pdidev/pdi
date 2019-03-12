SET(FLOWVR_FOUND TRUE)

SET(FLOWVR_PREFIX ${CMAKE_BINARY_DIR} CACHE INTERNAL "Path to FlowVR root")

SET(FLOWVR_base_INCLUDE_DIR  ${CMAKE_BINARY_DIR}/include 
                             ${flowvr_base_SOURCE_DIR}/include 
                             ${flowvr_base_BINARY_DIR}/include)
                             
SET(FLOWVR_base_LIBRARY flowvr-base)
SET(FLOWVR_base_FOUND TRUE)

SET(FLOWVR_mod_INCLUDE_DIR ${flowvr_base_SOURCE_DIR}/include)
SET(FLOWVR_mod_LIBRARY flowvr-mod)
SET(FLOWVR_mod_FOUND TRUE)

SET(FLOWVR_ftl_INCLUDE_DIR ${flowvr_ftl_SOURCE_DIR}/include)
SET(FLOWVR_ftl_LIBRARY ftlm)
SET(FLOWVR_ftl_FOUND TRUE)

SET(FLOWVR_plugd_INCLUDE_DIR ${flowvrd_SOURCE_DIR}/include)
SET(FLOWVR_plugd_LIBRARY flowvr-plugd)
SET(FLOWVR_plugd_FOUND TRUE)

SET(FLOWVR_commands_INCLUDE_DIR ${flowvrd_SOURCE_DIR}/include)
SET(FLOWVR_commands_LIBRARY flowvr-commands)
SET(FLOWVR_commands_FOUND TRUE)

SET(FLOWVR_INCLUDE_DIR ${FLOWVR_base_INCLUDE_DIR} 
                       ${FLOWVR_mod_INCLUDE_DIR} 
                       ${FLOWVR_ftl_INCLUDE_DIR} 
                       ${FLOWVR_plugd_INCLUDE_DIR} 
                       ${FLOWVR_commands_INCLUDE_DIR} )
                       
SET(FLOWVR_LIBRARY ${FLOWVR_base_LIBRARY} 
                   ${FLOWVR_mod_LIBRARY} 
                   ${FLOWVR_ftl_LIBRARY} 
                   ${FLOWVR_plugd_LIBRARY} 
                   ${FLOWVR_commands_LIBRARY} )
                   
