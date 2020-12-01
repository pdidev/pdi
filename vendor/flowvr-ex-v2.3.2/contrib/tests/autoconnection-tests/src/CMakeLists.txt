# PRIMES EXAMPLE 

INCLUDE_DIRECTORIES(../include ${FLOWVR_INCLUDE_DIR})


# COMPONENT LIBRARIES
FOREACH(comp  ${app}.comp)

  # SOURCE FILES
  SET(components-SRC	${comp}.cpp)
  
  ADD_TEST(autoconnect:example=0 flowvr Primes -Pprimes:example=0  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=1 flowvr Primes -Pprimes:example=1  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=2 flowvr Primes -Pprimes:example=2  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=3 flowvr Primes -Pprimes:example=3  -L -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=4 flowvr Primes -Pprimes:example=4  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=5 flowvr Primes -Pprimes:example=5  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=6 flowvr Primes -Pprimes:example=6  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=7 flowvr Primes -Pprimes:example=7  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=8 flowvr Primes -Pprimes:example=8  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=9 flowvr Primes -Pprimes:example=9  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=10 flowvr Primes -Pprimes:example=10  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=11 flowvr Primes -Pprimes:example=11  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=12 flowvr Primes -Pprimes:example=12  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=13 flowvr Primes -Pprimes:example=13  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=14 flowvr Primes -Pprimes:example=14  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=15 flowvr Primes -Pprimes:example=15  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=16 flowvr Primes -Pprimes:example=16  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=17 flowvr Primes -Pprimes:example=17  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=18 flowvr Primes -Pprimes:example=18  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=19 flowvr Primes -Pprimes:example=19  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=20 flowvr Primes -Pprimes:example=20  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=21 flowvr Primes -Pprimes:example=21  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=22 flowvr Primes -Pprimes:example=22  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=23 flowvr Primes -Pprimes:example=23  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=24 flowvr Primes -Pprimes:example=24  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
  ADD_TEST(autoconnect:example=25 flowvr Primes -Pprimes:example=25  -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)

# Does not execute this test. Cmake does not seem to provide a way to escape the quote properly. The command to be executed is 
# If using the following line. It always succeed as the -P..... is simply ignired (execute the defautl example
#  ADD_TEST(autoconnect:example=26 flowvr Primes -P"primes:example=26 primes/paramComp2:testParam=toto" -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)
# Trying to escape the quotes like this deos not seem to work properly
#  ADD_TEST(autoconnect:example=26 flowvr Primes -P\"primes:example=26 primes/paramComp2:testParam=toto\" -c ${LIB_DIR}/lib${comp}${CMAKE_SHARED_LIBRARY_SUFFIX} -H ${CMAKE_SOURCE_DIR}/primes.csv)


  SET_TESTS_PROPERTIES(
    autoconnect:example=6
    autoconnect:example=8
    autoconnect:example=9
    autoconnect:example=11
    autoconnect:example=13
    autoconnect:example=14
    autoconnect:example=15
    autoconnect:example=20 
    PROPERTIES WILL_FAIL TRUE) # Negative tests


  ADD_LIBRARY(${comp}  SHARED  ${components-SRC})
  TARGET_LINK_LIBRARIES(${comp} ${FLOWVR_base_LIBRARY} ${FLOWVR_mod_LIBRARY} ${FLOWVR_ftl_LIBRARY}  ${FLOWVR_app_LIBRARY}   ${FLOWVR_app_comp_LIBRARY}  )
  SET_TARGET_PROPERTIES(${comp} PROPERTIES SOVERSION ${PROJECT_VERSION_MAJOR} VERSION ${PROJECT_VERSION})
  INSTALL(TARGETS ${comp} LIBRARY DESTINATION ${LIB_DIR} )
  
ENDFOREACH(comp)


# MODULE  BINARIES

IF(BUILD_MODULES)
  
  INCLUDE_DIRECTORIES(${OPENGL_INCLUDE_DIR} ${GLUT_INCLUDE_DIR})
  
  FOREACH(target compute capture visu)
    ADD_EXECUTABLE(${target} ${target}.cpp)
    # Should work but Linux complain about not finding dl lib using by flowvr-plugd
    #  TARGET_LINK_LIBRARIES(${target} ${FLOWVR_LIBRARY} ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES})
    TARGET_LINK_LIBRARIES(${target}  ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES} ${FLOWVR_base_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_mod_LIBRARY}  ${CMAKE_THREAD_LIBS_INIT} )
    INSTALL(TARGETS ${target}  DESTINATION ${MODULE_DIR})	
  ENDFOREACH(target)
  
ENDIF(BUILD_MODULES)
