# SPDX-FileCopyrightText: 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

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
