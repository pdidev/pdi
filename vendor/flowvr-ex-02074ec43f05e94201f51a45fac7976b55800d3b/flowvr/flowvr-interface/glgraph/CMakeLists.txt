ADD_DEFINITIONS(-DHAVE_STRING_H)

FIND_PACKAGE(LibXml2)
MACRO_LOG_FEATURE(LIBXML2_FOUND "LibXML2" 
                                "The XML C parser and toolkit of Gnome" 
                                "http://xmlsoft.org" 
                                TRUE 
                                "Required by FlowVR GLGraph")

FIND_PACKAGE(LibXslt)
MACRO_LOG_FEATURE(LIBXSLT_FOUND "LibXslt" 
                                "Language for transforming XML documents" 
                                "http://xmlsoft.org/XSLT" 
                                TRUE 
                                "Required by FlowVR GLGraph")
                                

SET(QGLVIEWER_INCLUDE_DIR ../../)
SET(QGLVIEWER_LIBRARY QQGL)



# libgraph became deprecated and isn't distributed in recent graphviz packages
# However, libcgraph and gvc clash in graphviz 2.26.
# Therefore, we keep both versions around.
SET(REQUIREDGRAPHLIB_FOUND FALSE)

FIND_PACKAGE(LibGraph)
FIND_PACKAGE(LibCGraph)

IF(LIBGRAPH_FOUND)
  # libGraph is available, prioritize it
  SET(USING_CGRAPH FALSE)
  ADD_DEFINITIONS(-DUSING_CGRAPH=0)
  SET(REQUIREDGRAPHLIB_FOUND TRUE)
ELSE(LIBGRAPH_FOUND)
  IF(LIBCGRAPH_FOUND)
    # cgraph is available but might miss files on some systems
    SET(USING_CGRAPH TRUE)
    ADD_DEFINITIONS(-DUSING_CGRAPH=1)
    SET(REQUIREDGRAPHLIB_FOUND TRUE)
  ENDIF(LIBCGRAPH_FOUND)
ENDIF(LIBGRAPH_FOUND)

# IF GRAPH IS AVAILABLE, USE IT.
# CGRAPH CAN ASK FOR A MISSING ast_std.h FILE
#IF(LIBCGRAPH_FOUND)
  # We have a recent graphviz package, use cgraph instead
#  SET(USING_CGRAPH TRUE)
#  ADD_DEFINITIONS(-DUSING_CGRAPH=1)
#  SET(REQUIREDGRAPHLIB_FOUND TRUE)
#ELSE(LIBCGRAPH_FOUND)
#  IF(LIBGRAPH_FOUND)
    # We only have an old graphviz package, use deprecated libgraph
#    SET(USING_CGRAPH FALSE)
#    ADD_DEFINITIONS(-DUSING_CGRAPH=0)
#    SET(REQUIREDGRAPHLIB_FOUND TRUE)
#  ENDIF(LIBGRAPH_FOUND)
#ENDIF(LIBCGRAPH_FOUND)




FIND_PACKAGE(LibGVC)


IF(REQUIREDGRAPHLIB_FOUND AND LIBGVC_FOUND)
   SET(GRAPHVIZ_FOUND TRUE)
ELSE(REQUIREDGRAPHLIB_FOUND AND LIBGVC_FOUND)
  SET(GRAPHVIZ_FOUND FALSE)
ENDIF(REQUIREDGRAPHLIB_FOUND AND LIBGVC_FOUND)
MACRO_LOG_FEATURE(GRAPHVIZ_FOUND "GraphVIZ" "Graph layout library for graph visualization" "http://www.graphviz.org/" FALSE "Required by FlowVR GLGraph")


FIND_PACKAGE(GLEW)
MACRO_LOG_FEATURE(GLEW_FOUND "GLEW" "OpenGL Extension Wrangler Library" "http://glew.sourceforge.net" FALSE "Optional for FlowVR GLGraph. Used in conjunction with NVidia CG Toolkit to provide some advanced rendering effects")
FIND_PACKAGE(Cg)
SET(SHADER_SUPPORT FALSE)
IF(CG_FOUND AND CG_GL_LIBRARY AND GLEW_FOUND)
  SET(SHADER_SUPPORT TRUE)
ELSE(CG_FOUND AND CG_GL_LIBRARY AND GLEW_FOUND)
  ADD_DEFINITIONS(-DNOCG)
ENDIF(CG_FOUND AND CG_GL_LIBRARY AND GLEW_FOUND)

MACRO_LOG_FEATURE(SHADER_SUPPORT "Cg" "NVIDIA Cg Toolkit" "http://developer.nvidia.com"  FALSE "Optional for FlowVR GLGraph: Shader support for  fish-eye effect")


IF(LIBXML2_FOUND AND LIBXSLT_FOUND AND GRAPHVIZ_FOUND AND OPENGL_FOUND AND GLUT_FOUND)

  ADD_SUBDIRECTORY(src)
  INSTALL(DIRECTORY  share/   DESTINATION  share/flowvr/glgraph PATTERN ".svn" EXCLUDE)

ELSE(LIBXML2_FOUND AND LIBXSLT_FOUND AND GRAPHVIZ_FOUND AND OPENGL_FOUND AND GLUT_FOUND)

  SET(BUILD_FLOWVR_GLGRAPH FALSE CACHE BOOL "Build FlowVR-Graph" FORCE)
  MESSAGE(ERROR "FlowVR GLGraph: missing dependencies. BUILD_FLOWVR_GLGRAPH option turned OFF")

ENDIF(LIBXML2_FOUND AND LIBXSLT_FOUND AND GRAPHVIZ_FOUND AND OPENGL_FOUND AND GLUT_FOUND) 



