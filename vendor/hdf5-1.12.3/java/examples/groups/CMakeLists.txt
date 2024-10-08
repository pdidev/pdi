cmake_minimum_required (VERSION 3.12)
project (HDFJAVA_EXAMPLES_GROUPS Java)

set (CMAKE_VERBOSE_MAKEFILE 1)

set (HDF_JAVA_EXAMPLES
    H5Ex_G_Create
    H5Ex_G_Iterate
    H5Ex_G_Compact
    H5Ex_G_Corder
    H5Ex_G_Intermediate
    H5Ex_G_Phase
    H5Ex_G_Visit
)

if (WIN32)
  set (CMAKE_JAVA_INCLUDE_FLAG_SEP ";")
else ()
  set (CMAKE_JAVA_INCLUDE_FLAG_SEP ":")
endif ()

set (CMAKE_JAVA_INCLUDE_PATH "${HDF5_JAVA_JARS}")

set (CMAKE_JAVA_CLASSPATH ".")
foreach (CMAKE_INCLUDE_PATH ${CMAKE_JAVA_INCLUDE_PATH})
  set (CMAKE_JAVA_CLASSPATH "${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${CMAKE_INCLUDE_PATH}")
endforeach ()

foreach (example ${HDF_JAVA_EXAMPLES})
  file (WRITE ${PROJECT_BINARY_DIR}/${example}_Manifest.txt
  "Main-Class: examples.groups.${example}
"
  )
  add_jar (${example} MANIFEST ${PROJECT_BINARY_DIR}/${example}_Manifest.txt ${example}.java)
  get_target_property (${example}_JAR_FILE ${example} JAR_FILE)
#  install_jar (${example} ${HJAVA_INSTALL_DATA_DIR}/examples examples)
  get_target_property (${example}_CLASSPATH ${example} CLASSDIR)
  add_dependencies (${example} ${HDF5_JAVA_HDF5_LIB_TARGET})

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_JAVA_${example}_SRC_FORMAT ${example}.java)
  endif ()
endforeach ()

set (CMAKE_JAVA_INCLUDE_PATH "${HDF5_JAVA_JARS};${HDF5_JAVA_LOGGING_JAR};${HDF5_JAVA_LOGGING_NOP_JAR}")

set (CMAKE_JAVA_CLASSPATH ".")
foreach (HDFJAVA_JAR ${CMAKE_JAVA_INCLUDE_PATH})
  set (CMAKE_JAVA_CLASSPATH "${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${HDFJAVA_JAR}")
endforeach ()

set (HDF_JAVA_TEST_FILES
    h5ex_g_iterate.h5
    h5ex_g_visit.h5
)

foreach (h5_file ${HDF_JAVA_TEST_FILES})
  HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/${h5_file}" "${PROJECT_BINARY_DIR}/${h5_file}" "H5Ex_G_Visit_files")
endforeach ()
add_custom_target(H5Ex_G_Visit_files ALL COMMENT "Copying files needed by H5Ex_G_Visit tests" DEPENDS ${H5Ex_G_Visit_files_list})

if (BUILD_TESTING AND HDF5_TEST_EXAMPLES AND HDF5_TEST_SERIAL)
  get_property (target_name TARGET ${HDF5_JAVA_JNI_LIB_TARGET} PROPERTY OUTPUT_NAME)
  set (CMD_ARGS "-Dhdf.hdf5lib.H5.loadLibraryName=${target_name}$<$<OR:$<CONFIG:Debug>,$<CONFIG:Developer>>:${CMAKE_DEBUG_POSTFIX}>;")

  set (last_test "")
  foreach (example ${HDF_JAVA_EXAMPLES})
    if (NOT example STREQUAL "H5Ex_G_Iterate" AND NOT example STREQUAL "H5Ex_G_Visit")
      if (example STREQUAL "H5Ex_G_Compact")
        add_test (
            NAME JAVA_groups-${example}-clear-objects
            COMMAND ${CMAKE_COMMAND} -E remove
                ${HDFJAVA_EXAMPLES_BINARY_DIR}/${example}1.h5
                ${HDFJAVA_EXAMPLES_BINARY_DIR}/${example}2.h5
        )
      else ()
        add_test (
            NAME JAVA_groups-${example}-clear-objects
            COMMAND ${CMAKE_COMMAND} -E remove
                ${HDFJAVA_EXAMPLES_BINARY_DIR}/${example}.h5
        )
      endif ()
    else ()
      add_test (
          NAME JAVA_groups-${example}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E echo "${HDFJAVA_EXAMPLES_BINARY_DIR}/${example}.h5 exists"
      )
    endif ()
    if (last_test)
      set_tests_properties (JAVA_groups-${example}-clear-objects PROPERTIES DEPENDS ${last_test})
    endif ()

    add_test (
        NAME JAVA_groups-${example}-copy-objects
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${HDFJAVA_EXAMPLES_SOURCE_DIR}/testfiles/examples.groups.${example}.txt
            ${HDFJAVA_EXAMPLES_GROUPS_BINARY_DIR}/${example}.txt
    )
    set_tests_properties (JAVA_groups-${example}-copy-objects PROPERTIES DEPENDS JAVA_groups-${example}-clear-objects)
    add_test (
        NAME JAVA_groups-${example}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_TESTER=${CMAKE_Java_RUNTIME};${CMAKE_Java_RUNTIME_FLAGS}"
            -D "TEST_PROGRAM=examples.groups.${example}"
            -D "TEST_ARGS:STRING=${CMD_ARGS}"
            -D "TEST_CLASSPATH:STRING=${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${${example}_JAR_FILE}"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
            -D "TEST_FOLDER=${HDFJAVA_EXAMPLES_BINARY_DIR}"
            -D "TEST_OUTPUT=groups/${example}.out"
            -D "TEST_EXPECT=0"
            -D "TEST_REFERENCE=groups/${example}.txt"
            -P "${HDF_RESOURCES_DIR}/jrunTest.cmake"
    )
    set_tests_properties (JAVA_groups-${example} PROPERTIES
        DEPENDS JAVA_groups-${example}-copy-objects
    )

    if (NOT example STREQUAL "H5Ex_G_Iterate" AND NOT example STREQUAL "H5Ex_G_Visit")
      if (example STREQUAL "H5Ex_G_Compact")
        add_test (
            NAME JAVA_groups-${example}-clean-objects
            COMMAND ${CMAKE_COMMAND} -E remove
                ${HDFJAVA_EXAMPLES_BINARY_DIR}/${example}1.h5
                ${HDFJAVA_EXAMPLES_BINARY_DIR}/${example}2.h5
        )
      else ()
        add_test (
            NAME JAVA_groups-${example}-clean-objects
            COMMAND ${CMAKE_COMMAND} -E remove
                ${HDFJAVA_EXAMPLES_BINARY_DIR}/${example}.h5
        )
      endif ()
      set_tests_properties (JAVA_groups-${example}-clean-objects PROPERTIES
          DEPENDS JAVA_groups-${example}
      )
      set (last_test "JAVA_groups-${example}-clean-objects")
    else ()
      set (last_test "JAVA_groups-${example}")
    endif ()
  endforeach ()
endif ()
