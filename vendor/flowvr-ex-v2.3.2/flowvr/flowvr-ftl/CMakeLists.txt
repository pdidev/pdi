PROJECT(flowvr_ftl VERSION "${FLOWVR_MAIN_VERSION}")

ADD_SUBDIRECTORY(src)


FILE(GLOB include-files include/ftl/*.h)
INSTALL(FILES ${include-files} DESTINATION include/ftl)
