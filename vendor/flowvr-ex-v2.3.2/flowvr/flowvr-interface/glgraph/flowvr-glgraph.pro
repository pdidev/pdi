TEMPLATE	= app
LANGUAGE	= C++


CONFIG	+= qt opengl warn_on release thread console

HEADERS	+= src/Application.h \
	src/interface.h \
	src/GGraph.h \
	src/GVertex.h \
	src/GEdge.h \
	src/utils.h \
	src/bspline.h \
	src/MiniViewer.h \
    src/xml_parser.h \
    src/settings.h

SOURCES	+= src/Application.cpp \
	src/interface.cpp \
	src/main.cpp \
	src/GGraph.cpp \
	src/GVertex.cpp \
	src/GEdge.cpp \
	src/utils.cpp \
	src/bspline.cpp \
    src/xml_parser.cpp \
	src/MiniViewer.cpp

FORMS	= src/myInterface.ui

TARGET   = bin/flowvr-glgraph



QMAKE_CXXFLAGS *= -g

# Hide intermediate files
MOC_DIR = .moc
OBJECTS_DIR = .obj

# GLUT: for Text drawing
macx{
   LIBS +=  -framework GLUT
}else{
   LIBS *= -lglut
}


# OpenGL
macx{
   LIBS +=  -framework OpenGL 
} else {
   LIBS *= -lpthread -lGL -lGLU
}

# Shader (CG) and Glew
!contains(QMAKE_CXXFLAGS,-DNOCG) {
   macx{
      LIBS +=  -framework Cg  -lGLEW
   } else { 
      LIBS *=  -lCg -lCgGL -lGLEW
   }
}

# XSLT
macx{
   LIBS += -framework libxml -framework libxslt 
} else {
    XSLT=pkg-config --exists libxslt
    !system($$XSLT):error($$XSLT failled)
    LIBS *= $$system(pkg-config --libs libxslt)
    QMAKE_CXXFLAGS *=  $$system(pkg-config --cflags libxslt)
}

# GraphViz lib

GRAPH=pkg-config --exists libgvc libgraph
!system($$GRAPH):error($$GRAPH failled)
LIBS *= $$system(pkg-config --libs libgvc libgraph)
QMAKE_CXXFLAGS *=  $$system(pkg-config --cflags libgvc)

# QGLViewer

LIBS *= -lQGLViewer

# Use QGLVIEWER_LIB_DIR for lib dir  if set
QGL_LIB=$$(QGLVIEWER_LIB_DIR)
!isEmpty(QGL_LIB) {
  LIBS*=-L$$(QGLVIEWER_LIB_DIR)
}

# Use QGLVIEWER_INCLUDE_DIR for include dir  if set
QGL_INC=$$(QGLVIEWER_INCLUDE_DIR)
!isEmpty(QGL_INC) {
  QMAKE_CXXFLAGS *=  -I$$(QGLVIEWER_INCLUDE_DIR)
}


macx {
  # No thread support: look for -lqt and not -lqt-mt
  #  CONFIG -= thread
}


