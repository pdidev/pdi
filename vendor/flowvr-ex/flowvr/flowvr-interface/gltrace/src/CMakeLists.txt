INCLUDE_DIRECTORIES(    ${flowvr_base_SOURCE_DIR}/include 
                        ${flowvr_base_BINARY_DIR}/include 
                        ${flowvr_interface_SOURCE_DIR}/gltrace/include
                        "${CMAKE_SOURCE_DIR}/flowvr/flowvr-ftl/include"
                        )

IF(OPENGL_FOUND AND GLUT_FOUND)
  SET(gltrace-SRC parser.cpp opengl.cpp host.cpp
	        display.cpp colors.cpp gltrace.cpp)
  FILE(GLOB_RECURSE gltrace-HEADERS ../include/*.h)
  
  ADD_EXECUTABLE(flowvr-gltrace ${gltrace-SRC} ${gltrace-HEADERS})
  INCLUDE_DIRECTORIES(${GLUT_INCLUDE_DIR} ${OPENGL_INCLUDE_DIR})
  TARGET_LINK_LIBRARIES(flowvr-gltrace flowvr-base ftlm  ${GLUT_LIBRARIES} ${OPENGL_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
  INSTALL(TARGETS flowvr-gltrace DESTINATION bin)
ENDIF(OPENGL_FOUND AND GLUT_FOUND)

ADD_EXECUTABLE(flowvr-stats stats.cpp)
TARGET_LINK_LIBRARIES(flowvr-stats flowvr-base ${CMAKE_THREAD_LIBS_INIT})
INSTALL(TARGETS flowvr-stats DESTINATION bin)
