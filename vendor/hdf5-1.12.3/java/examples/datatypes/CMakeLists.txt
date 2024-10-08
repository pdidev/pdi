cmake_minimum_required (VERSION 3.12)
project (HDFJAVA_EXAMPLES_DATATYPES Java)

set (CMAKE_VERBOSE_MAKEFILE 1)

set (HDF_JAVA_EXAMPLES
    H5Ex_T_Array
    H5Ex_T_ArrayAttribute
    H5Ex_T_Bit
    H5Ex_T_BitAttribute
    H5Ex_T_Commit
    H5Ex_T_Compound
    H5Ex_T_CompoundAttribute
    H5Ex_T_Float
    H5Ex_T_FloatAttribute
    H5Ex_T_Integer
    H5Ex_T_IntegerAttribute
    H5Ex_T_ObjectReference
    H5Ex_T_ObjectReferenceAttribute
    H5Ex_T_Opaque
    H5Ex_T_OpaqueAttribute
    H5Ex_T_String
    H5Ex_T_StringAttribute
    H5Ex_T_VLString
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
  "Main-Class: examples.datatypes.${example}
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
    add_test (
        NAME JAVA_datatypes-${example}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove
            ${HDFJAVA_EXAMPLES_BINARY_DIR}/${example}.h5
    )
    if (last_test)
      set_tests_properties (JAVA_datatypes-${example}-clear-objects PROPERTIES DEPENDS ${last_test})
    endif ()

    add_test (
        NAME JAVA_datatypes-${example}-copy-objects
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${HDFJAVA_EXAMPLES_SOURCE_DIR}/testfiles/examples.datatypes.${example}.txt
            ${HDFJAVA_EXAMPLES_DATATYPES_BINARY_DIR}/${example}.txt
    )
    set_tests_properties (JAVA_datatypes-${example}-copy-objects PROPERTIES DEPENDS JAVA_datatypes-${example}-clear-objects)
    add_test (
        NAME JAVA_datatypes-${example}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_TESTER=${CMAKE_Java_RUNTIME};${CMAKE_Java_RUNTIME_FLAGS}"
            -D "TEST_PROGRAM=examples.datatypes.${example}"
            -D "TEST_ARGS:STRING=${CMD_ARGS}"
            -D "TEST_CLASSPATH:STRING=${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${${example}_JAR_FILE}"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
            -D "TEST_FOLDER=${HDFJAVA_EXAMPLES_BINARY_DIR}"
            -D "TEST_OUTPUT=datatypes/${example}.out"
            -D "TEST_EXPECT=0"
            -D "TEST_REFERENCE=datatypes/${example}.txt"
            -P "${HDF_RESOURCES_DIR}/jrunTest.cmake"
    )
    set_tests_properties (JAVA_datatypes-${example} PROPERTIES
        DEPENDS JAVA_datatypes-${example}-copy-objects
    )

    add_test (
        NAME JAVA_datatypes-${example}-clean-objects
        COMMAND ${CMAKE_COMMAND} -E remove
            ${HDFJAVA_EXAMPLES_BINARY_DIR}/${example}.h5
    )
    set_tests_properties (JAVA_datatypes-${example}-clean-objects PROPERTIES
        DEPENDS JAVA_datatypes-${example}
    )
    set (last_test "JAVA_datatypes-${example}-clean-objects")
  endforeach ()
endif ()
