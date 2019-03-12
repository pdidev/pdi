# PRIMES EXAMPLE 

INCLUDE_DIRECTORIES(../include ${FLOWVR_INCLUDE_DIR})




# COMPONENT LIBRARIES
  ADD_LIBRARY(modulecomposite.comp  SHARED  modulecomposite.comp )
  TARGET_LINK_LIBRARIES(modulecomposite.comp ${FLOWVR_base_LIBRARY} ${FLOWVR_mod_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_app_LIBRARY} ${FLOWVR_app_comp_LIBRARY}  )
  SET_TARGET_PROPERTIES(modulecomposite.comp PROPERTIES SOVERSION ${PROJECT_VERSION_MAJOR} VERSION ${PROJECT_VERSION})
  INSTALL(TARGETS modulecomposite.comp LIBRARY DESTINATION ${LIB_DIR} )


FOREACH(comp  ${app}.comp)

  # SOURCE FILES
  SET(components-SRC	${comp}.cpp)

  # Positive tests: probe different configurations of primes with different host mapping 
  ADD_TEST(NAME 1 WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh test1  primes_all1.csv 0)
  ADD_TEST(NAME 2   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test2 primes_all1.csv 1)
  ADD_TEST(NAME 3   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test3 primes_all1.csv 2)
  ADD_TEST(NAME 4   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test4 primes_all1.csv 3)
  ADD_TEST(NAME 5   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test5 primes_all1.csv 4)
  ADD_TEST(NAME 6   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test6 primes_all1.csv 5)
  ADD_TEST(NAME 7   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test7 primes_parallel.csv 4)
  ADD_TEST(NAME 8   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test8 primes_parallel.csv 5)

  # Positive tests: probe different configuraiton of primes with local host mapping

  ADD_TEST(NAME 9   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test9 -L 0)
  ADD_TEST(NAME 10   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test10 -L 1)
  ADD_TEST(NAME 11   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test11 -L 2)
  ADD_TEST(NAME 12   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test12 -L 3)
  ADD_TEST(NAME 13   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test13 -L 4)
  ADD_TEST(NAME 14   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test14 -L 5)

  # Negative test: mix localhost and real hosts 
  ADD_TEST(NAME 15   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test15 primes_localhost0.csv 4)

  # Positive test: mix 0 localhost and real hosts 
  ADD_TEST(NAME 16   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test16 primes_localhost1.csv 4)

 #Positive test: force mapping of filter  through csv
  ADD_TEST(NAME 17   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test17 primes_mapfilter0.csv 4)

 #Negative test: request mapping of module  through csv
  ADD_TEST(NAME 18   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test18 primes_mapmodule0.csv 4)

 #Negative  test: mapping primes composite
  ADD_TEST(NAME 19   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test19 primes_map0.csv 4)

 #Negative  test: multiple mapping of singelton metamodule
  ADD_TEST(NAME 20   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test20 primes_multiplesingleton0.csv 0)

 #Positive test: composite read hostlist from CSV and propagate to child (composite) that propagate to children (primitives)
  ADD_TEST(NAME 21   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test21 primes_propagecomposite0.csv 6)
  
 #Positive test: test propagation of hostlist from composite to primitive.
  ADD_TEST(NAME 22   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test22 primes_signaland.csv 8)

#Positive test: test metamoduleflowvr-run-ssh-parallelfromport
  ADD_TEST(NAME 23   WORKING_DIRECTORY  ${CMAKE_SOURCE_DIR}    COMMAND  test.sh   test23 primes_parallelfromport.csv 9)


  SET_TESTS_PROPERTIES(
    15
    18
    19
    20
    PROPERTIES WILL_FAIL TRUE) # Negative tests
    
    
  ADD_LIBRARY(${comp}  SHARED  ${components-SRC})
  TARGET_LINK_LIBRARIES(${comp} ${FLOWVR_base_LIBRARY} ${FLOWVR_mod_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_app_LIBRARY} ${FLOWVR_app_comp_LIBRARY} modulecomposite.comp )
  SET_TARGET_PROPERTIES(${comp} PROPERTIES SOVERSION ${PROJECT_VERSION_MAJOR} VERSION ${PROJECT_VERSION})
  INSTALL(TARGETS ${comp} LIBRARY DESTINATION ${LIB_DIR} )

ENDFOREACH(comp)

  SET( comp dynhierarchy.comp )
  SET( components-SRC ${comp}.cpp )
  
  ADD_LIBRARY(${comp}  SHARED  ${components-SRC})
  TARGET_LINK_LIBRARIES(${comp} ${FLOWVR_base_LIBRARY} 
                                ${FLOWVR_mod_LIBRARY} 
                                ${FLOWVR_ftl_LIBRARY} 
                                ${FLOWVR_app_LIBRARY} 
                                ${FLOWVR_app_comp_LIBRARY} )
                                
  SET_TARGET_PROPERTIES(${comp} PROPERTIES SOVERSION ${PROJECT_VERSION_MAJOR} VERSION ${PROJECT_VERSION})
  INSTALL(TARGETS ${comp} LIBRARY DESTINATION ${LIB_DIR} )


# MODULE  BINARIES

IF(BUILD_MODULES)
  
  INCLUDE_DIRECTORIES(${OPENGL_INCLUDE_DIR} ${GLUT_INCLUDE_DIR})
  
  FOREACH(target compute capture visu)
    ADD_EXECUTABLE(${target} ${target}.cpp)
    # Should work but Linux complain about not finding dl lib using by flowvr-plugd
    #  TARGET_LINK_LIBRARIES(${target} ${FLOWVR_LIBRARY} ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES})
    TARGET_LINK_LIBRARIES(${target}  ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES} ${FLOWVR_base_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_mod_LIBRARY} ${CMAKE_THREAD_LIBS_INIT} )
    INSTALL(TARGETS ${target}  DESTINATION ${MODULE_DIR})	
  ENDFOREACH(target)
  
ENDIF(BUILD_MODULES)
