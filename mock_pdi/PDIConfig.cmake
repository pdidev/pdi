#=============================================================================
# Copyright (C) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_policy(PUSH)
cmake_policy(VERSION 3.22...4.2)

include(CMakeFindDependencyMacro)

# Check for unsupported required components
if(DEFINED PDI_FIND_COMPONENTS)
	foreach(component ${PDI_FIND_COMPONENTS})
		if(NOT component STREQUAL "C")
			message(FATAL_ERROR "The component '${component}' is not supported by the mock PDI; only the C component is.")
		endif()
	endforeach()
endif()

# Create the PDI_C target only once
if(NOT TARGET PDI_C)
	add_library(PDI_C INTERFACE)
	target_include_directories(PDI_C INTERFACE "${CMAKE_CURRENT_LIST_DIR}")
	target_compile_definitions(PDI_C INTERFACE "WITHOUT_PDI=1")
	add_library(PDI::pdi   ALIAS PDI_C)
	add_library(PDI::PDI_C ALIAS PDI_C)
endif()

if(NOT TARGET paraconf::paraconf)
	if(DEFINED PDI_MOCK_PARACONF_TARGET AND "${PDI_MOCK_PARACONF_TARGET}")
		add_library(paraconf INTERFACE)
		target_compile_definitions(paraconf INTERFACE "WITHOUT_PARACONF=1")
		add_library(paraconf::paraconf ALIAS paraconf)
	else()
		find_dependency(paraconf COMPONENTS C)
	endif()
endif()
target_link_libraries(PDI_C INTERFACE paraconf::paraconf)

cmake_policy(POP)
