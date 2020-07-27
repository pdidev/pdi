# TICTACFCA EXAMPLE 


INCLUDE_DIRECTORIES(../include ${FLOWVR_INCLUDE_DIR})


SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH}  $ENV{FlowVR_DIR} $ENV{FlowVRVRPN_DIR} $ENV{FlowVRContrib_DIR} ${${CMAKE_PROJECT_NAME}_SOURCE_DIR}/cmake/)

find_package(fca)


# SOURCE FILES
#SET(components-SRC   ${app}.comp.cpp)

#ADD_TEST(${app}.comp_LIBRARY_TEST flowvr TicTacFca -L -c ${LIB_DIR}/lib${app}.comp${CMAKE_SHARED_LIBRARY_SUFFIX})

# COMPONENT LIBRARY
#ADD_LIBRARY(${app}.comp SHARED  ${components-SRC})
# Should work but Linux complain about not finding dl lib used by flowvr-plugd
#TARGET_LINK_LIBRARIES(${app}.comp ${FLOWVR_base_LIBRARY}
#                                  ${FLOWVR_mod_LIBRARY}  
#                                  ${FLOWVR_app_LIBRARY} 
#                                  ${FLOWVR_app_comp_LIBRARY})
#INSTALL(TARGETS ${app}.comp LIBRARY DESTINATION ${COMP_LIB_DIR} )

#set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} "${FLOWVR_PREFIX}/lib/libfca.so")

# MODULE  BINARIES

IF(BUILD_MODULES)

FOREACH(target putfca getfca)
  ADD_EXECUTABLE(${target} ${target}.cpp)
# Should work but Linux complain about not finding dl lib used  by flowvr-plugd
#  TARGET_LINK_LIBRARIES(${target} ${FLOWVR_LIBRARY})
  TARGET_LINK_LIBRARIES(${target} ${FLOWVR_base_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_mod_LIBRARY} ${CMAKE_THREAD_LIBS_INIT} ${fca_LIBRARY})
  INSTALL(TARGETS ${target}  DESTINATION ${MODULE_DIR})	
ENDFOREACH(target)

ENDIF(BUILD_MODULES)
