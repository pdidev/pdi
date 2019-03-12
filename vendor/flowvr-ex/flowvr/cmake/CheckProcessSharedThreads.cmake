# - Check if the current system supports setpshared on mutex attributes.
# CHECK_FOR_PROCESS_SHARED_THREADS(RESULT)
# RESULT - variable to store the result of this test.

GET_FILENAME_COMPONENT(CHECK_FOR_PROCESS_SHARED_THREADS_PREFIX ${CMAKE_CURRENT_LIST_FILE} PATH)

MACRO(CHECK_FOR_PROCESS_SHARED_THREADS result)
  IF(NOT PTHREAD_PSHARED_ATTR)
  MESSAGE(STATUS "Check if threads are multiprocess aware")
  TRY_RUN(${result} _compileResult
          ${CMAKE_BINARY_DIR}
          ${CHECK_FOR_PROCESS_SHARED_THREADS_PREFIX}/CheckProcessSharedThreads.c
          CMAKE_FLAGS -DLINK_LIBRARIES:STRING=${CMAKE_THREAD_LIBS_INIT}
          OUTPUT_VARIABLE OUTPUT)
  IF(${result} EQUAL 0)
    SET(${result} TRUE)
  ELSE(${result} EQUAL 0)
    SET(${result} FALSE)
  ENDIF(${result} EQUAL 0)
  IF(_compileResult AND ${result})
    SET(PTHREAD_PSHARED_ATTR TRUE CACHE INTERNAL "Pthreads support for shared mutex, attributes")
    MESSAGE(STATUS "Check if threads are multiprocess aware -- yes")
    FILE(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
      "Check if threads are multiprocess aware passed with the following output:\n"
      "${OUTPUT}\n\n")
  ELSE(_compileResult AND ${result})
    SET(PTHREAD_PSHARED_ATTR FALSE CACHE INTERNAL "Pthreads support for shared mutex, attributes")
    MESSAGE(STATUS "Check if threads are multiprocess aware -- no")
    FILE(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
      "Determining if threads are multiprocess aware failed with the following output:\n"
      "${OUTPUT}\n\n")
  ENDIF(_compileResult AND ${result})
  ENDIF(NOT PTHREAD_PSHARED_ATTR)
  SET(${result} ${PTHREAD_PSHARED_ATTR})
ENDMACRO(CHECK_FOR_PROCESS_SHARED_THREADS)
