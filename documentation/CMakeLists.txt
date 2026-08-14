#=============================================================================
# Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.22...4.2)

find_package(Doxygen 1.9 REQUIRED)

set(DOXYGEN_INPUT
		"documentation/About.md"
		"documentation/Concepts.md"
		"documentation/FAQ.md"
		"documentation/First_steps.md"
		"documentation/How_to_create_plugin.md"
		"documentation/Installation.md"
		"documentation/Plugins.md"
		"documentation/Specification_tree_ref.md"
		"documentation/Source_installation.md"
		"documentation/Using_PDI.md"
		"documentation/Yaml.md"
		"pdi/include/pdi.h"
		"pdi/include/pdi/"
		"example/README.md"
		"tutorial/README.md"
		"plugins/decl_hdf5/README.md"
		"plugins/decl_netcdf/README.md"
		"plugins/mpi/README.md"
		"plugins/timer/README.md"
		"plugins/trace/README.md"
		"plugins/user_code/README.md"
		"plugins/pycall/README.md"
		"plugins/serialize/README.md"
		"plugins/set_value/README.md"
		"plugins/json/README.md")
string(REPLACE ";" " " DOXYGEN_INPUT "${DOXYGEN_INPUT}")

file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/html")
configure_file(Doxyfile "${CMAKE_CURRENT_BINARY_DIR}/Doxyfile")
add_custom_target(doc ALL
	Doxygen::doxygen "${CMAKE_CURRENT_BINARY_DIR}/Doxyfile"
	WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../"
	DEPENDS ${DOXYGEN_INPUT}
)
install(DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/html/" DESTINATION "${CMAKE_INSTALL_DOCDIR}/html")

add_subdirectory(First_steps)
