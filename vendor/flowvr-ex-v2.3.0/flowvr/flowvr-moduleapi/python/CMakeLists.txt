OPTION(BUILD_FLOWVR_PYTHONMODULEAPI "Build FlowVR Python Module API" TRUE)

# cflowvr.py and cflowvrPYHTON_wrap.cxx are generated with swig through cmake 
# They can be directly generated with swig: 
# swig -v -python -c++  -I/home/matthijs/src/grimage/install/include/ cflowvr.i
# 

IF( BUILD_FLOWVR_PYTHONMODULEAPI ) 

  FIND_PACKAGE(PythonLibs)
  MACRO_LOG_FEATURE(PYTHONLIBS_FOUND "PythonLibs"
  		    "Python Libs" 
		    "www.python.org/"
		    FALSE
		   "Required to generate the Python Module API")

  FIND_PACKAGE(SWIG)
  MACRO_LOG_FEATURE(SWIG_FOUND  "SWIG" 
    "Simplified Wrapper and Interface Generator" 
    "http://www.swig.org/" 
    FALSE
    "Required to generate the Python Module API")
  
  
  IF( PYTHONLIBS_FOUND AND SWIG_FOUND  )



    INCLUDE(UseSWIG)
    
    INCLUDE_DIRECTORIES(${PYTHON_INCLUDE_PATH}
                        ${CMAKE_CURRENT_SOURCE_DIR}
                        ${flowvr_base_SOURCE_DIR}/include)

    
    SET(CMAKE_SWIG_FLAGS "")
    
#    SET(CMAKE_SWIG_OUTDIR ${CMAKE_CURRENT_SOURCE_DIR})

    SET_SOURCE_FILES_PROPERTIES(cflowvr.i PROPERTIES CPLUSPLUS ON)

    IF ( ${CMAKE_VERSION} VERSION_GREATER "3.8.0" )
       SWIG_ADD_LIBRARY(cflowvr
         LANGUAGE python
         SOURCES cflowvr.i)
     ELSE ( ${CMAKE_VERSION} VERSION_GREATER "3.8.0" )
       SWIG_ADD_MODULE(cflowvr python  cflowvr.i)
     ENDIF ( ${CMAKE_VERSION} VERSION_GREATER "3.8.0" )

    
    SWIG_LINK_LIBRARIES(cflowvr ${PYTHON_LIBRARIES} flowvr-base flowvr-mod)

    INSTALL(FILES flowvr.py ${CMAKE_CURRENT_BINARY_DIR}/cflowvr.py DESTINATION lib/flowvr/python)
    INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/_cflowvr.so DESTINATION lib/flowvr/python)
    

  ELSE( PYTHONLIBS_FOUND  AND SWIG_FOUND  )
    
    # Disable  Option if missing dependencies
    SET(BUILD_FLOWVR_PYTHONMODULEAPI FALSE CACHE BOOL "Build FlowVR Python Module API" FORCE)
    MESSAGE( "FlowVR Python Module API : SWIG or PythonLibs are missing. BUILD_FLOWVR_PYTHONMODULEAPI option turned OFF")

  ENDIF( PYTHONLIBS_FOUND   AND SWIG_FOUND  )

ENDIF( BUILD_FLOWVR_PYTHONMODULEAPI ) 



# Feature logged once all dependencies have been checked.
MACRO_LOG_FEATURE(BUILD_FLOWVR_PYTHONMODULEAPI "FlowVR Python Module API (flowvr-moduleapi/python)" "FlowVR Module API to support Python modules" "http://flowvr.sf.net" FALSE "Optional")
MACRO_DISPLAY_FEATURE_LOG()
