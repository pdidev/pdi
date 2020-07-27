FIND_PACKAGE( FlowVRRender REQUIRED )

ADD_SUBDIRECTORY(src)

INSTALL( DIRECTORY config DESTINATION share/flowvr-contrib/menusource/tests PATTERN "*.prm" REGEX ".svn" EXCLUDE )
INSTALL( DIRECTORY config DESTINATION share/flowvr-contrib/menusource/tests PATTERN "*.xml" REGEX ".svn" EXCLUDE )
INSTALL( DIRECTORY config DESTINATION share/flowvr-contrib/menusource/tests PATTERN "*.xsd" REGEX ".svn" EXCLUDE )
INSTALL( DIRECTORY resources/scripts DESTINATION share/flowvr-contrib/menusource/tests PATTERN "*.lua" REGEX ".svn" EXCLUDE )
INSTALL( DIRECTORY resources/layouts DESTINATION share/flowvr-contrib/menusource/tests PATTERN "*.wnd" REGEX ".svn" EXCLUDE )