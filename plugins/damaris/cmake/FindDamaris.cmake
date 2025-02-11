# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:
FindDamaris
--------

Find Damaris, a library for data managmement and visualisation that provides 
asynchronous, in situ processing capabilities to MPI based simulation codes.
#]=======================================================================]

#include(${CMAKE_CURRENT_LIST_DIR}/SelectLibraryConfigurations.cmake)
#include(${CMAKE_CURRENT_LIST_DIR}/FindPackageHandleStandardArgs.cmake)
include(FindPackageHandleStandardArgs)

set(Damaris_BASE_DIR /usr/lib/damaris /opt/damaris)
set(Damaris_VERSIONS 1.11.1 1.12.0)

#Find Damaris base install dir
set(Damaris_CANDIDATES ${Damaris_ROOT})
foreach(base_dir ${Damaris_BASE_DIR})  
  list(APPEND Damaris_CANDIDATES ${base_dir}) #In case no version dir exists
  foreach(version ${Damaris_VERSIONS})
    list(APPEND Damaris_CANDIDATES ${base_dir}/${version})
  endforeach()
endforeach()

set(Damaris_INC_SUFFIXES)
set(Damaris_LIB_SUFFIXES)
foreach(version ${Damaris_VERSIONS})
  list(APPEND Damaris_INC_SUFFIXES include)
  list(APPEND Damaris_LIB_SUFFIXES lib)
endforeach()

find_path(Damaris_INCLUDE_DIRS Damaris.h 
        HINTS ${Damaris_CANDIDATES} 
        PATH_SUFFIXES ${Damaris_INC_SUFFIXES} 
	ENV CPLUS_INCLUDE_PATH)
find_library(Damaris_LIBRARIES damaris
        HINTS ${Damaris_CANDIDATES}
        PATH_SUFFIXES ${Damaris_LIB_SUFFIXES} 
	ENV LIBRARY_PATH LD_LIBRARY_PATH)

find_package_handle_standard_args(Damaris DEFAULT_MSG Damaris_INCLUDE_DIRS 
		Damaris_LIBRARIES)
mark_as_advanced(Damaris_INCLUDE_DIRS Damaris_LIBRARIES)
if(Damaris_INCLUDE_DIRS)
 	set(Damaris_FOUND TRUE)
	get_filename_component(Damaris_LIBRARIES_PATH ${Damaris_LIBRARIES} DIRECTORY)
endif(Damaris_INCLUDE_DIRS)