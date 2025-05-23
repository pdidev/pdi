cmake_minimum_required (VERSION 3.12)
project (HDFJAVA_EXAMPLES_DATASETS Java)

set (CMAKE_VERBOSE_MAKEFILE 1)

set (HDF_JAVA_EXAMPLES
    H5Ex_D_Alloc
    H5Ex_D_Checksum
    H5Ex_D_Chunk
    H5Ex_D_Compact
    H5Ex_D_External
    H5Ex_D_FillValue
    H5Ex_D_Hyperslab
    H5Ex_D_ReadWrite
    H5Ex_D_UnlimitedAdd
    H5Ex_D_UnlimitedMod
    H5Ex_D_Nbit
    H5Ex_D_Transform
    H5Ex_D_Sofloat
    H5Ex_D_Soint
)

set (HDF_JAVA_ZLIB_EXAMPLES
    H5Ex_D_Gzip
    H5Ex_D_Shuffle
    H5Ex_D_UnlimitedGzip
)

set (HDF_JAVA_SZIP_EXAMPLES
    H5Ex_D_Szip
)

# detect whether the encoder is present.
  if (H5_HAVE_FILTER_DEFLATE)
    set (${HDF_JAVA_EXAMPLES} ${HDF_JAVA_EXAMPLES} ${HDF_JAVA_ZLIB_EXAMPLES})
  endif ()

  if (H5_HAVE_FILTER_SZIP)
    set (${HDF_JAVA_EXAMPLES} ${HDF_JAVA_EXAMPLES} ${HDF_JAVA_SZIP_EXAMPLES})
  endif (H5_HAVE_FILTER_SZIP)

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
  "Main-Class: examples.datasets.${example}
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

if (BUILD_TESTING AND HDF5_TEST_EXAMPLES AND HDF5_TEST_SERIAL)
  get_property (target_name TARGET ${HDF5_JAVA_JNI_LIB_TARGET} PROPERTY OUTPUT_NAME)
  set (CMD_ARGS "-Dhdf.hdf5lib.H5.loadLibraryName=${target_name}$<$<OR:$<CONFIG:Debug>,$<CONFIG:Developer>>:${CMAKE_DEBUG_POSTFIX}>;")

  set (last_test "")
  foreach (example ${HDF_JAVA_EXAMPLES})
    if (example STREQUAL "H5Ex_D_External")
      add_test (
          NAME JAVA_datasets-${example}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              ${HDFJAVA_EXAMPLES_BINARY_DIR}/${example}.h5
              ${HDFJAVA_EXAMPLES_BINARY_DIR}/${example}.data
      )
    else ()
      add_test (
          NAME JAVA_datasets-${example}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              ${HDFJAVA_EXAMPLES_BINARY_DIR}/${example}.h5
      )
    endif ()
    if (last_test)
      set_tests_properties (JAVA_datasets-${example}-clear-objects PROPERTIES DEPENDS ${last_test})
    endif ()

    add_test (
        NAME JAVA_datasets-${example}-copy-objects
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${HDFJAVA_EXAMPLES_SOURCE_DIR}/testfiles/examples.datasets.${example}.txt
            ${HDFJAVA_EXAMPLES_DATASETS_BINARY_DIR}/${example}.txt
    )
    set_tests_properties (JAVA_datasets-${example}-copy-objects PROPERTIES DEPENDS JAVA_datasets-${example}-clear-objects)

    add_test (
        NAME JAVA_datasets-${example}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_TESTER=${CMAKE_Java_RUNTIME};${CMAKE_Java_RUNTIME_FLAGS}"
            -D "TEST_CLASSPATH:STRING=${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${${example}_JAR_FILE}"
            -D "TEST_ARGS:STRING=${CMD_ARGS}"
            -D "TEST_PROGRAM=examples.datasets.${example}"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
            -D "TEST_FOLDER=${HDFJAVA_EXAMPLES_BINARY_DIR}"
            -D "TEST_OUTPUT=datasets/${example}.out"
            -D "TEST_EXPECT=0"
            -D "TEST_REFERENCE=datasets/${example}.txt"
            -P "${HDF_RESOURCES_DIR}/jrunTest.cmake"
    )
    set_tests_properties (JAVA_datasets-${example} PROPERTIES
        DEPENDS JAVA_datasets-${example}-copy-objects
    )

    if (example STREQUAL "H5Ex_D_External")
      add_test (
          NAME JAVA_datasets-${example}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              ${HDFJAVA_EXAMPLES_BINARY_DIR}/${example}.h5
              ${HDFJAVA_EXAMPLES_BINARY_DIR}/${example}.data
      )
    else ()
      add_test (
          NAME JAVA_datasets-${example}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              ${HDFJAVA_EXAMPLES_BINARY_DIR}/${example}.h5
      )
    endif ()
    set_tests_properties (JAVA_datasets-${example}-clean-objects PROPERTIES
        DEPENDS JAVA_datasets-${example}
    )
    set (last_test "JAVA_datasets-${example}-clean-objects")
  endforeach ()
endif ()
