SET(flowvr_graph-SRCS Application.cpp adltree.cpp bspline.cpp ConnectionNode.cpp ConnectionStampsNode.cpp
	FilterNode.cpp GCluster.cpp GEdge.cpp GNode.cpp Graph.cpp 
    interface.cpp main.cpp MiniViewer.cpp
    PortInNode.cpp PortOutNode.cpp PrimitiveNode.cpp RoutingNode.cpp utils.cpp
    GEdgeNet.cpp GraphNet.cpp GVertexNet.cpp xml_parser.cpp traces.cpp)

# Graphviz 2.30+ headers predeclare a couple of your system functions with conflicting signatures.
# 2.30 (debian) requires a fix. 2.26 (debian) doesn't.
# Versions inbetween haven't been tested.
IF(USING_CGRAPH)
	ADD_DEFINITIONS(-D_PACKAGE_ast=1) # ugly fix for graphviz headers
ENDIF(USING_CGRAPH)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR} ${QGLVIEWER_INCLUDE_DIR} ${QT_INCLUDES})
INCLUDE_DIRECTORIES(${LIBXML2_INCLUDE_DIR} ${LIBXSLT_INCLUDE_DIR} ${QGLVIEWER_INCLUDE_DIR}  ${LIBGVC_INCLUDE_DIR} ${OPENGL_INCLUDE_DIR} ${GLUT_INCLUDE_DIR} ${CMAKE_CURRENT_BINARY_DIR})

ADD_DEFINITIONS(${LIBXML2_DEFINITIONS} ${LIBXSLT_DEFINITIONS} ${LIBGVC_DEFINITIONS})

IF(USING_CGRAPH)
	INCLUDE_DIRECTORIES(${LIBCGRAPH_INCLUDE_DIR})
	ADD_DEFINITIONS(${LIBCGRAPH_DEFINITIONS})
ELSE(USING_CGRAPH)
	INCLUDE_DIRECTORIES(${LIBGRAPH_INCLUDE_DIR})
	ADD_DEFINITIONS(${LIBGRAPH_DEFINITIONS})
ENDIF(USING_CGRAPH)


ADD_DEFINITIONS(-g)


#ADD_SUBDIRECTORY(nodeClasses)

IF(SHADER_SUPPORT)
  INCLUDE_DIRECTORIES(${CG_INCLUDE_PATH} ${GLEW_INCLUDE_DIR})
ENDIF(SHADER_SUPPORT)
  

SET(flowvr_graph-FORMS myInterface.ui)
SET(flowvr_graph-MOCS interface.h MiniViewer.h Application.h)

QT4_WRAP_UI(flowvr_graph-SRCS ${flowvr_graph-FORMS} )
QT4_WRAP_CPP(flowvr_graph-SRCS ${flowvr_graph-MOCS})
#QT4_ADD_RESOURCES(${flowvr_graph-MOCS} ${flowvr_graph-FORMS})
#QT4_AUTOMOC(flowvr_graph-SRCS ${flowvr_graph-MOCS})

ADD_EXECUTABLE(flowvr-glgraph ${flowvr_graph-SRCS})

IF(USING_CGRAPH)
	TARGET_LINK_LIBRARIES(flowvr-glgraph  
		                  ${LIBXML2_LIBRARIES} 
		                  ${LIBXSLT_LIBRARIES} 
		                  ${QGLVIEWER_LIBRARY} 
		                  ${LIBCGRAPH_LIBRARIES} 
		                  ${LIBGVC_LIBRARIES} 
		                  ${OPENGL_LIBRARIES} 
		                  ${GLUT_LIBRARIES} 
		                  ${QT_LIBRARIES})
ELSE(USING_CGRAPH)
	TARGET_LINK_LIBRARIES(flowvr-glgraph  
		                  ${LIBXML2_LIBRARIES} 
		                  ${LIBXSLT_LIBRARIES} 
		                  ${QGLVIEWER_LIBRARY} 
		                  ${LIBGRAPH_LIBRARIES} 
		                  ${LIBGVC_LIBRARIES} 
		                  ${OPENGL_LIBRARIES} 
		                  ${GLUT_LIBRARIES} 
		                  ${QT_LIBRARIES})
ENDIF(USING_CGRAPH)
                      
#ADD_DEPENDENCIES(flowvr-glgraph QQGL) 

IF(SHADER_SUPPORT)
  TARGET_LINK_LIBRARIES(flowvr-glgraph ${CG_LIBRARY} ${CG_GL_LIBRARY}  ${GLEW_LIBRARIES})
ENDIF(SHADER_SUPPORT)

INSTALL(TARGETS flowvr-glgraph DESTINATION bin)
