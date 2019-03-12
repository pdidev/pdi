INCLUDE_DIRECTORIES(${flowvr_base_SOURCE_DIR}/include 
                    ${flowvr_base_BINARY_DIR}/include 
                    ${flowvrd_SOURCE_DIR}/include  
                    ${flowvr_ftl_SOURCE_DIR}/include 
                    ${flowvr_modules_SOURCE_DIR}/include 
                    ${FLOWVR_commands_LIBRARY} 
		    ${CMAKE_SOURCE_DIR}/flowvr/flowvr-modules/include #flowvr-modules_SOURCE_DIR hasn't been set yet. 
                    ${HWLOC_INCLUDE_DIR}
                    )

# FLOWVR LAUNCHER BINARY 
ADD_EXECUTABLE(flowvr flowvr.cpp)
TARGET_LINK_LIBRARIES(flowvr
                      ftlm 
                      flowvr-base 
                      flowvr-commands 
                      ${CMAKE_DL_LIBS} 
                      ${CMAKE_THREAD_LIBS_INIT})
                      
                      
INSTALL(TARGETS flowvr RUNTIME DESTINATION bin)

