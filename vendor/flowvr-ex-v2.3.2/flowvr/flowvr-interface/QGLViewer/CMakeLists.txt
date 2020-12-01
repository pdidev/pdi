
INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCES_DIR} /usr/X11R6/include) 

SET(MOC-SRCS frame.h qglviewer.h camera.h keyFrameInterpolator.h manipulatedCameraFrame.h manipulatedFrame.h)

SET(QQGL-SRCS camera.cpp 
              frame.cpp
              manipulatedFrame.cpp 
              qglviewer.cpp 
              saveSnapshot.cpp 
              constraint.cpp 
              keyFrameInterpolator.cpp 
              manipulatedCameraFrame.cpp 
              mouseGrabber.cpp 
              quaternion.cpp 
              vec.cpp)

QT4_WRAP_UI(QQGL-SRCS ImageInterface.Qt4.ui VRenderInterface.Qt4.ui)
QT4_WRAP_CPP(QQGL-SRCS ${MOC-SRCS})  
#QT4_ADD_RESOURCES(ImageInterface.Qt4.ui VRenderInterface.Qt4.ui ${MOC-SRCS})
#QT4_AUTOMOC(QQGL-SRCS ${MOC-SRCS})

SET(QQGL-SRCS ${QQGL-SRCS} VRender/BackFaceCullingOptimizer.cpp  
                           VRender/BSPSortMethod.cpp             
                           VRender/EPSExporter.cpp               
                           VRender/Exporter.cpp     
                           VRender/FIGExporter.cpp  
                           VRender/gpc.cpp          
                           VRender/NVector3.cpp   
                           VRender/ParserGL.cpp   
                           VRender/Primitive.cpp  
                           VRender/PrimitivePositioning.cpp   
                           VRender/TopologicalSortMethod.cpp  
                           VRender/Vector2.cpp                
                           VRender/Vector3.cpp 
                           VRender/VisibilityOptimizer.cpp 
                           VRender/VRender.cpp )

ADD_LIBRARY(QQGL STATIC ${QQGL-SRCS}) 
 
TARGET_LINK_LIBRARIES(QQGL ${QT_LIBRARIES}) 
