PROJECT(flowvr_interface)
OPTION(BUILD_FLOWVR_GLGRAPH "Build FlowVR GLGraph" TRUE)
OPTION(BUILD_FLOWVR_GLTRACE "Build FlowVR GLTrace" TRUE)


IF( BUILD_FLOWVR_GLTRACE )
  ADD_SUBDIRECTORY(gltrace)
ENDIF( BUILD_FLOWVR_GLTRACE )

IF( BUILD_FLOWVR_GLGRAPH ) 

  #Check QT4 dependency (common to QGLViewer and glgraph)

  # This find package version fails with cmake version < 2.6.4
  #  FIND_PACKAGE(Qt4 COMPONENTS QtCore QtGui QtXml QtOpenGL Qt3Support) 

  # More sturdy QT4 package search supported since cmake 2.6.0
  FIND_PACKAGE(Qt4)

  #Set  MYQT4_COMPONENTS_FOUND variable that is true if all required qt component were  found.
  IF( QT_QTCORE_FOUND AND QT_QTGUI_FOUND  AND QT4_FOUND AND QT_QTXML_FOUND AND QT_QT3SUPPORT_FOUND AND QT_QTOPENGL_FOUND )
    SET(MYQT4_COMPONENTS_FOUND TRUE) 
  ELSEIF( QT_QTCORE_FOUND AND QT_QTGUI_FOUND  AND QT4_FOUND AND QT_QTXML_FOUND AND QT_QT3SUPPORT_FOUND AND QT_QTOPENGL_FOUND )
    SET(MYQT4_COMPONENTS_FOUND FALSE) 
  ENDIF( QT_QTCORE_FOUND AND QT_QTGUI_FOUND  AND QT4_FOUND AND QT_QTXML_FOUND AND QT_QT3SUPPORT_FOUND AND QT_QTOPENGL_FOUND )


 
  MACRO_LOG_FEATURE(MYQT4_COMPONENTS_FOUND  "Qt4 (with XML, qt3 and opengl support)" "Qt crossplatform development framework" "http://qt.digia.comq" FALSE "Required by  flowvr-glgraph")

IF(QT_FOUND) 	 	 
	SET(QT_USE_QTXML 1) 	 	 
	SET(QT_USE_QTOPENGL 1) 	 	 
	 	 	 
	INCLUDE(${QT_USE_FILE}) 	 	 
	SET(QT_USE_QT3SUPPORT 1) 	 	 
ENDIF(QT_FOUND)

  IF( MYQT4_COMPONENTS_FOUND)

    # Set environment for QT4 compilation and linking
    SET(QT_USE_QTXML 1)
    SET(QT_USE_QTOPENGL 1)
    SET(QT_USE_QT3SUPPORT 1)
    INCLUDE(${QT_USE_FILE})

    ADD_SUBDIRECTORY(QGLViewer)
    ADD_SUBDIRECTORY(glgraph)

  ELSE( MYQT4_COMPONENTS_FOUND )

    SET(BUILD_FLOWVR_GLGRAPH FALSE CACHE BOOL "Build FlowVR-Graph" FORCE)
    MESSAGE(STATUS "FlowVR GLGraph: QT4 or QT4 support for xml, qt3 or opengl are  missing. BUILD_FLOWVR_GLGRAPH option turned OFF")
    
  ENDIF( MYQT4_COMPONENTS_FOUND )
   
ENDIF( BUILD_FLOWVR_GLGRAPH ) 

# Feature logged once all dependencies have been checked.
MACRO_LOG_FEATURE(BUILD_FLOWVR_GLGRAPH "FlowVR GLGraph  (flowvr-interface/glgraph)" "FlowVR  Network Visualization Tool" "http://flowvr.sf.net" FALSE "Optional")


