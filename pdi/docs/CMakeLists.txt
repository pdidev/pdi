# SPDX-FileCopyrightText: 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)

find_package(Doxygen 1.9 REQUIRED)

set(DOXYGEN_INPUT
		"${CMAKE_CURRENT_SOURCE_DIR}/About.md"
		"${CMAKE_CURRENT_SOURCE_DIR}/Concepts.md"
		"${CMAKE_CURRENT_SOURCE_DIR}/FAQ.md"
		"${CMAKE_CURRENT_SOURCE_DIR}/First_steps.md"
		"${CMAKE_CURRENT_SOURCE_DIR}/How_to_create_plugin.md"
		"${CMAKE_CURRENT_SOURCE_DIR}/Installation.md"
		"${CMAKE_CURRENT_SOURCE_DIR}/Plugins.md"
		"${CMAKE_CURRENT_SOURCE_DIR}/Specification_tree_ref.md"
		"${CMAKE_CURRENT_SOURCE_DIR}/Source_installation.md"
		"${CMAKE_CURRENT_SOURCE_DIR}/Using_PDI.md"
		"${CMAKE_CURRENT_SOURCE_DIR}/Yaml.md"
		"${PDI_BINARY_DIR}/pdi/export.h"
		"${PDI_BINARY_DIR}/pdi/version.h"
		"${PDIF_H}"
		"${PDI_SOURCE_DIR}/include/pdi.h"
		"${PDI_SOURCE_DIR}/include/pdi/"
		"${PDI_SOURCE_DIR}/../example/README.md"
		"${PDI_SOURCE_DIR}/../tutorial/README.md"
		"${PDI_SOURCE_DIR}/../plugins/decl_hdf5/README.md"
		"${PDI_SOURCE_DIR}/../plugins/decl_netcdf/README.md"
		"${PDI_SOURCE_DIR}/../plugins/mpi/README.md"
		"${PDI_SOURCE_DIR}/../plugins/trace/README.md"
		"${PDI_SOURCE_DIR}/../plugins/user_code/README.md"
		"${PDI_SOURCE_DIR}/../plugins/pycall/README.md"
		"${PDI_SOURCE_DIR}/../plugins/serialize/README.md"
		"${PDI_SOURCE_DIR}/../plugins/set_value/README.md"
		"${PDI_SOURCE_DIR}/../plugins/json/README.md")

set(DOXY_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/../..")
cmake_path(NORMAL_PATH DOXY_ROOT)

set(DOXYGEN_REL_INPUT)
foreach(INPUT_ELEMENT IN LISTS DOXYGEN_INPUT)
	cmake_path(RELATIVE_PATH INPUT_ELEMENT BASE_DIRECTORY "${DOXY_ROOT}")
	list(APPEND DOXYGEN_REL_INPUT "${INPUT_ELEMENT}")
endforeach()
string(REPLACE ";" " " DOXYGEN_REL_INPUT "${DOXYGEN_REL_INPUT}")

file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/html")
configure_file(Doxyfile "${CMAKE_CURRENT_BINARY_DIR}/Doxyfile")
add_custom_target(doc ALL
	Doxygen::doxygen "${CMAKE_CURRENT_BINARY_DIR}/Doxyfile"
	WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../"
	DEPENDS ${DOXYGEN_INPUT}
)
add_dependencies(doc generated_files_for_doc)
install(DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/html/" DESTINATION "${CMAKE_INSTALL_DOCDIR}/html")

add_subdirectory(First_steps)
