# EXAMPLES
# Copy all  examples (no compilation) in /share directory

FOREACH(target tictac primes fluid fluid_insitu simulator bundle)
INSTALL(DIRECTORY  ${target}
  DESTINATION  share/flowvr/examples/ PATTERN ".svn" EXCLUDE )
INSTALL(PROGRAMS ${target}/make-app.sh 
  DESTINATION share/flowvr/examples/${target}/) # to make sure it has the correct execution rights
ENDFOREACH(target)

# Copy python filter examples in /share directory
INSTALL(DIRECTORY  filters
  DESTINATION  share/flowvr/examples/ PATTERN ".svn" EXCLUDE )


# Make sure we load FlowVRCOnfig.cmake to be able to use all variables it set up. 
FIND_PACKAGE(FlowVR REQUIRED)


# Compile and install the demo examples:  
# A demo:
#       - a flowvr application the user can easily try after installing flowvr
#       - use the flowvr-demo-*.sh script to launch the demo (launch the daemon as well)
#       - require to compile and install the lib*.comp.so and the module executable
# We prepare theses demos at this level not to pollute the CMAKELists of the examples. 

SET(DEMOS TicTac)
IF( OPENGL_FOUND AND GLUT_FOUND)
 IF( APPLE OR (GLUT_Xi_LIBRARY AND GLUT_Xmu_LIBRARY) )
  # decided that tictac and fluid is enough so far.
  # if you want to build primes along with this script... do so.
  SET(DEMOS ${DEMOS} Fluid Primes)
 ENDIF( APPLE OR (GLUT_Xi_LIBRARY AND GLUT_Xmu_LIBRARY) )
ENDIF( OPENGL_FOUND AND GLUT_FOUND)


# # Flowvr include dirs.
# INCLUDE_DIRECTORIES(${FLOWVR_INCLUDE_DIR})

# #########################################################################
# SET( fluid-modules fluid gldens )
# SET( fluid-src-dir fluid/src )
# SET( fluid-fluid-src ${fluid-src-dir}/fluid.cpp
#       ${fluid-src-dir}/Vec3D.cpp
#       ${fluid-src-dir}/Turbulent.cpp
#       ${fluid-src-dir}/TurbulentBase.cpp )
      
# SET( fluid-fluid-deps ${FLOWVR_base_LIBRARY} 
#                       ${FLOWVR_ftl_LIBRARY} 
#                       ${FLOWVR_mod_LIBRARY} 
#                       ${CMAKE_THREAD_LIBS_INIT} )
                      
# SET( fluid-gldens-src ${fluid-src-dir}/gldens.cpp )
# SET( fluid-gldens-deps ${FLOWVR_base_LIBRARY}  
#                        ${FLOWVR_ftl_LIBRARY} 
#                        ${FLOWVR_mod_LIBRARY} 
#                        ${OPENGL_LIBRARIES} 
#                        ${GLUT_LIBRARIES} 
#                        ${CMAKE_THREAD_LIBS_INIT} )
# SET( fluid-gldens-incs ${OPENGL_INCLUDE_DIR} ${GLUT_INCLUDE_DIR} )                       
# ##########################################################################

# ##########################################################################
# SET( tictac-modules put get )
# SET( tictac-src tictac/src )
# SET( tictac-put-src ${tictac-src}/put.cpp )
# SET( tictac-get-src ${tictac-src}/get.cpp )
# SET( tictac-put-deps ${FLOWVR_base_LIBRARY} ${FLOWVR_mod_LIBRARY}  ${CMAKE_THREAD_LIBS_INIT} )
# SET( tictac-get-deps ${FLOWVR_base_LIBRARY} ${FLOWVR_mod_LIBRARY}  ${CMAKE_THREAD_LIBS_INIT} )
# ##########################################################################

##########################################################################
#SET( primes-modules capture compute visu )
#SET( primes-src primes/src )
#ET( primes-capture-src ${primes-src}/capture.cpp )
#SET( primes-compute-src ${primes-src}/compute.cpp )
#SET( primes-visu-src    ${primes-src}/visu.cpp )
#SET( primes-capture-deps ${OPENGL_LIBRARIES} 
#                         ${GLUT_LIBRARIES} 
#                         ${FLOWVR_base_LIBRARY} 
#                         ${FLOWVR_ftl_LIBRARY}
#                         ${FLOWVR_mod_LIBRARY} 
#                         ${CMAKE_THREAD_LIBS_INIT} )
                         
#SET( primes-compute-deps ${FLOWVR_base_LIBRARY} 
#                         ${FLOWVR_ftl_LIBRARY}
#                         ${FLOWVR_mod_LIBRARY} 
#                         ${CMAKE_THREAD_LIBS_INIT} )
#
#SET( primes-visu-deps ${primes-capture-deps} )
                         
#########################################################################                         

FOREACH(DEMO ${DEMOS})

  # # Build lib name
  STRING(TOLOWER ${DEMO} DEMO_LOWER)

  # # Component Libraries
  # SET(DEMO_SRC_DIR    ${DEMO_COMP_LOWER}/src)
  # INCLUDE_DIRECTORIES(${DEMO_COMP_LOWER}/include)
  
  
  # ############################################################################
  # ## COMPILE MODULES
  # ############################################################################
  # SET(MODULE_BIN_DIR   share/flowvr/modules/bin) # module install dir
  
  # SET( MODULES ${${DEMO_COMP_LOWER}-modules} )
  # STRING( REPLACE ";" " " MODULES_STRING "${MODULES}" )
  
  # FOREACH( module ${MODULES} )
  #     INCLUDE_DIRECTORIES(${${DEMO_COMP_LOWER}-${module}-incs} ${DEMO_SRC_DIR} )     
  #     ADD_EXECUTABLE(${module} ${${DEMO_COMP_LOWER}-${module}-src} )    
  #     TARGET_LINK_LIBRARIES(${module}  ${${DEMO_COMP_LOWER}-${module}-deps} )
  #     INSTALL(TARGETS ${module}  DESTINATION ${MODULE_BIN_DIR})	      
  # ENDFOREACH(module ${MODULES} )

  # INSTALL flowvr-demo scripts 
  CONFIGURE_FILE(flowvr-demo-app.sh.in ${FlowVR_BINARY_DIR}/flowvr-demo-${DEMO_LOWER}.sh  @ONLY)
  INSTALL(PROGRAMS  ${FlowVR_BINARY_DIR}/flowvr-demo-${DEMO_LOWER}.sh   DESTINATION bin)
  
ENDFOREACH(DEMO ${DEMOS})



  




