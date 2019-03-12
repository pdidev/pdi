PROJECT(flowvr_ftl)

ADD_SUBDIRECTORY(src)


FILE(GLOB include-files include/ftl/*.h)
INSTALL(FILES ${include-files} DESTINATION include/ftl)
