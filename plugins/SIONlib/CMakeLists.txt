#=============================================================================
# Copyright 2015 FZJ, Benedikt Steinbusch <b.steinbusch@fz-juelich.de>
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

find_package(SIONlib REQUIRED)

add_library(SIONlib MODULE
  SIONlib.c
)
target_link_libraries(SIONlib pdi)
include_directories(${SIONlib_MPI_C_INCLUDE_DIRS})
set_property(SOURCE SIONlib.c APPEND_STRING PROPERTY COMPILE_FLAGS " ${SIONlib_MPI_C_COMPILE_FLAGS} ")
target_link_libraries(SIONlib ${SIONlib_MPI_C_LIBRARIES})

set_target_properties(pdi PROPERTIES C_STANDARD 99 C_STANDARD_REQUIRED ON)
include(AddCCompilerFlags)
add_compiler_flags(SIONlib PRIVATE -Wall -Wextra -std=c99)


# library installation

install(TARGETS SIONlib EXPORT SIONlibTargets
	LIBRARY DESTINATION lib
	ARCHIVE DESTINATION lib
)

# add_subdirectory(tests/)
