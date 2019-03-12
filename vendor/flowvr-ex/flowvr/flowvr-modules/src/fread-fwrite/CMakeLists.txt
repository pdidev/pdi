INCLUDE_DIRECTORIES(${flowvr_base_SOURCE_DIR}/include
                    ${flowvr_base_BINARY_DIR}/include
                    )

ADD_EXECUTABLE(fwrite fwrite.cpp)
TARGET_LINK_LIBRARIES(fwrite flowvr-base flowvr-mod ${CMAKE_THREAD_LIBS_INIT})

ADD_EXECUTABLE(fread fread.cpp)
TARGET_LINK_LIBRARIES(fread  flowvr-base flowvr-mod ${CMAKE_THREAD_LIBS_INIT})

INSTALL(TARGETS fread fwrite  DESTINATION ${MODULE_BIN_DIR})
