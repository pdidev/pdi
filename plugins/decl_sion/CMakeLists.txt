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

add_library(decl_sion MODULE
  decl_sion.cxx 
)
target_link_libraries(decl_sion pdi)
include_directories(${SIONlib_MPI_C_INCLUDE_DIRS})
set_property(SOURCE decl_sion.cxx APPEND_STRING PROPERTY COMPILE_FLAGS " ${SIONlib_MPI_C_COMPILE_FLAGS} ")
target_link_libraries(decl_sion ${SIONlib_MPI_C_LIBRARIES})

include(AddCCompilerFlags)


# library installation

install(TARGETS decl_sion EXPORT decl_sionTargets
	LIBRARY DESTINATION lib
	ARCHIVE DESTINATION lib
)

add_subdirectory(tests/)
