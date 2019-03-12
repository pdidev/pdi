FIND_PACKAGE(FlowVR REQUIRED)
FIND_PACKAGE(FlowVRRender REQUIRED)

INCLUDE(${FLOWVR_APPLICATION})

CONFIGURE_FILE(net/opengl-wrapper.pl ${CMAKE_CURRENT_BINARY_DIR}/net/opengl-wrapper.pl COPYONLY)

ADD_FLOWVR_APPLICATION(opengl-wrapper net/opengl-wrapper.mml.xml
                       PERL net/opengl-wrapper.pl)
