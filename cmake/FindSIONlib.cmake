# include this to handle the QUIETLY and REQUIRED arguments
include(FindPackageHandleStandardArgs)
include(GetPrerequisites)

#   SIONlib_<api>_<lang>_COMPILE_FLAGS
#   SIONlib_<api>_<lang>_INCLUDE_DIRS
#   SIONlib_<api>_<lang>_LIBRARIES

function (interrogate_sionconfig api lang)
  string(TOLOWER ${api} SIONCONFIG_API_PARAM)
  string(TOLOWER ${lang} SIONCONFIG_LANG_PARAM)

  execute_process(
    COMMAND "${SIONCONFIG}" "--${SIONCONFIG_API_PARAM}" "--${SIONCONFIG_LANG_PARAM}" "--cflags" "--foobar"
    OUTPUT_VARIABLE SIONCONFIG_OUTPUT OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_VARIABLE SIONCONFIG_OUTPUT ERROR_STRIP_TRAILING_WHITESPACE
    RESULT_VARIABLE SIONCONFIG_RETURN)

  if (NOT "${SIONCONFIG_RETURN}" EQUAL 0)
    message(STATUS "Unable to interrogate sionconfig (${SIONCONFIG})")
    message(STATUS ${SIONCONFIG_OUTPUT})
    set(SIONCONFIG_OUTPUT)
  endif ()

  if (SIONCONFIG_OUTPUT)
    string(REGEX MATCHALL "(^| )-D([^\" ]+|\"[^\"]+\")" SIONlib_ALL_COMPILE_FLAGS "${SIONCONFIG_OUTPUT}")

    set(SIONlib_COMPILE_FLAGS_WORK)
    foreach (FLAG ${SIONlib_ALL_COMPILE_FLAGS})
      if (SIONlib_COMPILE_FLAGS_WORK)
        set(SIONlib_COMPILE_FLAGS_WORK "${SIONlib_COMPILE_FLAGS_WORK} ${FLAG}")
      else ()
        set(SIONlib_COMPILE_FLAGS_WORK ${FLAG})
      endif ()
    endforeach ()

    string(REGEX MATCHALL "(^| )-I([^\" ]+|\"[^\"]+\")" SIONlib_ALL_INCLUDE_DIRS "${SIONCONFIG_OUTPUT}")
    foreach (IDIR ${SIONlib_ALL_INCLUDE_DIRS})
      string(REGEX REPLACE "^ ?-I" "" IDIR ${IDIR})
      string(REGEX REPLACE "//" "/" IDIR ${IDIR})
      list(APPEND SIONlib_INCLUDE_DIRS_WORK ${IDIR})
    endforeach ()

    set(SIONlib_${api}_${lang}_COMPILE_FLAGS ${SIONlib_COMPILE_FLAGS_WORK} CACHE STRING "SIONlib ${api} API ${lang} compilation flags" FORCE)
    set(SIONlib_${api}_${lang}_INCLUDE_DIRS ${SIONlib_INCLUDE_DIRS_WORK} CACHE STRING "SIONlib ${api} API ${lang} include path" FORCE)
  endif ()

  execute_process(
    COMMAND "${SIONCONFIG}" "--${SIONCONFIG_API_PARAM}" "--${SIONCONFIG_LANG_PARAM}" "--libs"
    OUTPUT_VARIABLE SIONCONFIG_OUTPUT OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_VARIABLE SIONCONFIG_OUTPUT ERROR_STRIP_TRAILING_WHITESPACE
    RESULT_VARIABLE SIONCONFIG_RETURN)

  if (NOT "${SIONCONFIG_RETURN}" EQUAL 0)
    message(STATUS "Unable to interrogate sionconfig (${SIONCONFIG})")
    message(STATUS ${SIONCONFIG_OUTPUT})
    set(SIONCONFIG_OUTPUT)
  endif ()

  if (SIONCONFIG_OUTPUT)
    string(REGEX MATCHALL "(^| |-Wl,)-L([^\" ]+|\"[^\"]+\")" SIONlib_ALL_LINK_DIRS "${SIONCONFIG_OUTPUT}")

    set(SIONlib_LINK_DIR)
    foreach (LDIR ${SIONlib_ALL_LINK_DIRS})
      string(REGEX REPLACE "^(| |-Wl,)-L" "" LDIR ${LDIR})
      string(REGEX REPLACE "//" "/" LDIR ${LDIR})
      list(APPEND SIONlib_LINK_DIR ${LDIR})
    endforeach ()

    if (DEFINED CMAKE_${lang}_IMPLICIT_LINK_DIRECTORIES)
      list(APPEND SIONlib_LINK_DIR "${CMAKE_${lang}_IMPLICIT_LINK_DIRECTORIES}")
    endif ()

    string(REGEX MATCHALL "(^| )(-l([^\" ]+|\"[^\"]+\")|[^ ]*\\.(a|so))( |$)" SIONlib_LIBNAMES "${SIONCONFIG_OUTPUT}")

    foreach (LIB ${SIONlib_LIBNAMES})
      string(REGEX REPLACE "^ +" "" LIB ${LIB})
      string(REGEX REPLACE "^-l *" "" LIB ${LIB})
      string(REGEX REPLACE " $" "" LIB ${LIB})

      set(SIONlib_LIB "SIONlib_LIB-NOTFOUND" CACHE FILEPATH "Cleared" FORCE)
      if (EXISTS "${LIB}")
        set(SIONlib_LIB "${LIB}")
      else ()
        find_library(SIONlib_LIB NAMES ${LIB} HINTS ${SIONlib_LINK_DIR})
      endif ()

      if (SIONlib_LIB)
        list(APPEND SIONlib_LIBRARIES_WORK ${SIONlib_LIB})
      endif()
    endforeach ()

    set(SIONlib_${api}_${lang}_LIBRARIES ${SIONlib_LIBRARIES_WORK} CACHE STRING "SIONlib ${api} API ${lang} libraries to link against" FORCE)
    set(SIONlib_LIB "SIONlib_LIB-NOTFOUND" CACHE INTERNAL "Scratch variable for SIONlib lib detection" FORCE)
  endif ()
endfunction ()

find_program(SIONCONFIG NAMES sionconfig DOC "SIONlib configuration tool.")
mark_as_advanced(SIONCONFIG)

foreach (api SER OMP MPI OMPI)
  foreach (lang C CXX F90 F77)
    interrogate_sionconfig(${api} ${lang})
  endforeach ()
endforeach ()

find_package_handle_standard_args(SIONlib DEFAULT_MSG
  SIONCONFIG
  )
