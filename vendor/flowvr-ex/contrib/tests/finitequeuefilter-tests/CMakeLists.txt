# FINITE-QUEUE-FILTER TEST

INCLUDE_DIRECTORIES(${FLOWVR_INCLUDE_DIR})

# COMPONENT LIBRARY

SET(components-SRC   finitequeuefiltertest.comp.cpp)


ADD_LIBRARY(finitequeuefiltertest.comp SHARED  ${components-SRC})

# Should work but Linux complain about not finding dl lib used by flowvr-plugd
TARGET_LINK_LIBRARIES(finitequeuefiltertest.comp 
                      ${FLOWVR_base_LIBRARY} 
                      ${FLOWVR_mod_LIBRARY}  
                      ${FLOWVR_ftl_LIBRARY}    
                      ${FLOWVR_app_LIBRARY}   
                      ${FLOWVR_app_comp_LIBRARY}  )
                      
                      
SET_TARGET_PROPERTIES(finitequeuefiltertest.comp PROPERTIES SOVERSION ${PROJECT_VERSION_MAJOR} VERSION ${PROJECT_VERSION})
INSTALL(TARGETS finitequeuefiltertest.comp LIBRARY DESTINATION share/flowvr-contrib/tests/components )
INSTALL(DIRECTORY parametersets DESTINATION share/flowvr-contrib/tests/finitequeuefiltertest USE_SOURCE_PERMISSIONS PATTERN ".svn" EXCLUDE)


SET(modules-SRC ModuleSender.cpp
                ModuleReceiver.cpp)

FOREACH(modulesrc ${modules-SRC})
  STRING(REPLACE ".cpp" "" module ${modulesrc})
  ADD_EXECUTABLE(${module} ${modulesrc})
   TARGET_LINK_LIBRARIES(${module}  ${FLOWVR_mod_LIBRARY} ${FLOWVR_ftl_LIBRARY} )
  INSTALL(TARGETS ${module} DESTINATION share/flowvr-contrib/tests/bin)
ENDFOREACH(modulesrc)

