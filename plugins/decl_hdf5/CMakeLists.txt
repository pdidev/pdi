#=============================================================================
# Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
# # SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

cmake_minimum_required(VERSION 3.1)
project(pdi_decl_hdf5_plugin LANGUAGES CXX)

# HDF5
set(HDF5_USE_STATIC_LIBRARIES OFF)
set(HDF5_PREFER_PARALLEL TRUE)
find_package(HDF5 REQUIRED)
if(NOT "${HDF5_IS_PARALLEL}")
	message(WARNING "Sequential HDF5 only found, decl'HDF5 will not work in parallel")
endif()

# The plugin
add_library(pdi_decl_hdf5_plugin MODULE decl_hdf5.cxx)
target_link_libraries(pdi_decl_hdf5_plugin PUBLIC pdi ${HDF5_C_LIBRARIES} ${HDF5_C_HL_LIBRARIES})
if("${HDF5_IS_PARALLEL}")
	target_link_libraries(pdi_decl_hdf5_plugin PUBLIC MPI::MPI_C)
endif()
target_include_directories(pdi_decl_hdf5_plugin PRIVATE ${HDF5_INCLUDE_DIRS})
target_compile_definitions(pdi_decl_hdf5_plugin PRIVATE ${HDF5_DEFINITIONS})
set_target_properties(pdi_decl_hdf5_plugin PROPERTIES
		CXX_STANDARD 11
		CXX_STANDARD_REQUIRED TRUE
		CXX_VISIBILITY_PRESET hidden)

# installation
install(TARGETS pdi_decl_hdf5_plugin EXPORT decl_hdf5Targets LIBRARY DESTINATION lib)

# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()
