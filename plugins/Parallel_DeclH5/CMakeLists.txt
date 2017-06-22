#=============================================================================
# Copyright 2015 CEA, Julien Bigot <julien.bigot@cea.fr>
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the names of CEA, nor the names of the contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written  permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================
#if(PARALLEL_DECLH5)
set(HDF5_PARALLEL "PAR")
	#endif()
find_package(HDF5 REQUIRED)

add_library(parallel_declh5 MODULE
	parallel_declh5.c
)

message("Include path: ${HDF5_SHARED_C_INCLUDE_PATH}")
message("flags: ${HDF5_SHARED_C_COMPILE_PATH}")
message("link: ${HDF5_SHARED_C_LINK_PATH}")

target_link_libraries(parallel_declh5 pdi)
include_directories(${HDF5_SHARED_C_INCLUDE_PATH})
set_property(SOURCE parallel_declh5.c APPEND_STRING PROPERTY COMPILE_FLAGS " ${HDF5_SHARED_C_COMPILE_FLAGS} ")
set_property(TARGET parallel_declh5   APPEND_STRING PROPERTY LINK_FLAGS    " ${HDF5_SHARED_C_LINK_FLAGS} ")
target_link_libraries(parallel_declh5 ${HDF5_SHARED_C_LIBRARIES})

set_target_properties(pdi PROPERTIES C_STANDARD 99 C_STANDARD_REQUIRED ON)
include(AddCCompilerFlags)
add_compiler_flags(parallel_declh5 PRIVATE -Wall -Wextra -std=c99)


# library installation

install(TARGETS parallel_declh5 EXPORT parallel_declh5Targets
	LIBRARY DESTINATION lib
	ARCHIVE DESTINATION lib
)

add_subdirectory(tests/)
