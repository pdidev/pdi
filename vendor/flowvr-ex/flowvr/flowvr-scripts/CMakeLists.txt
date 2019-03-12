SET(scripts flowvr-run-ssh flowvr-run-env flowvr-term flowvr-setup-shmem )
FOREACH(script ${scripts})
  CONFIGURE_FILE(${script} ${CMAKE_BINARY_DIR}/bin/${script} @ONLY)
  INSTALL(PROGRAMS ${CMAKE_BINARY_DIR}/bin/${script} DESTINATION bin 
          PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ)
ENDFOREACH(script)

# install relocate by a seperate rule (it has a different naming scheme
CONFIGURE_FILE(flowvr-relocate.sh.in ${CMAKE_BINARY_DIR}/bin/flowvr-relocate.sh @ONLY)
INSTALL(PROGRAMS ${CMAKE_BINARY_DIR}/bin/flowvr-relocate.sh DESTINATION bin 
        PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ)


