ADD_SUBDIRECTORY( src )

INSTALL( DIRECTORY config DESTINATION share/flowvr-contrib/tests/tcp-tests PATTERN "*.xml" REGEX ".svn" EXCLUDE )
INSTALL( DIRECTORY config DESTINATION share/flowvr-contrib/tests/tcp-tests PATTERN "*.prm" REGEX ".svn" EXCLUDE )
INSTALL( DIRECTORY config DESTINATION share/flowvr-contrib/tests/tcp-tests PATTERN "*.csv" REGEX ".svn" EXCLUDE )

INSTALL( FILES filterandtest.sh DESTINATION share/flowvr-contrib/tests/tcp-tests PERMISSIONS OWNER_EXECUTE OWNER_READ OWNER_WRITE GROUP_READ WORLD_READ )
