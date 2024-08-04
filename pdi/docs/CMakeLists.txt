#=============================================================================
# Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.10...3.25)

find_package(Doxygen 1.8.12 REQUIRED OPTIONAL_COMPONENTS dot)

add_custom_target(doc ALL)
file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/html")

set(DOXYGEN_AUTOLINK_SUPPORT YES)
set(DOXYGEN_BUILTIN_STL_SUPPORT YES)
set(DOXYGEN_DISABLE_INDEX YES)
set(DOXYGEN_ENUM_VALUES_PER_LINE 1)
set(DOXYGEN_EXAMPLE_PATH "${PDI_SOURCE_DIR}/.." "${PDI_SOURCE_DIR}" "${CMAKE_CURRENT_SOURCE_DIR}")
set(DOXYGEN_EXPAND_ONLY_PREDEF YES)
set(DOXYGEN_EXTRACT_ALL YES)
set(DOXYGEN_EXTRACT_LOCAL_CLASSES NO)
set(DOXYGEN_FULL_PATH_NAMES NO)
set(DOXYGEN_GENERATE_TREEVIEW YES)
#set(DOXYGEN_GENERATE_XML YES) # Crashes on Gitlab CI
set(DOXYGEN_HTML_FOOTER "${CMAKE_CURRENT_SOURCE_DIR}/_template/footer.html")
set(DOXYGEN_HTML_HEADER "${CMAKE_CURRENT_SOURCE_DIR}/_template/header.html")
set(DOXYGEN_HTML_STYLESHEET "${CMAKE_CURRENT_SOURCE_DIR}/_template/style.css")
set(DOXYGEN_HTML_OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/html")
set(DOXYGEN_HTML_TIMESTAMP YES)
set(DOXYGEN_INCLUDE_GRAPH NO)
set(DOXYGEN_INCLUDE_PATH "${PDI_SOURCE_DIR}/include/" "${PDI_SOURCE_DIR}/src/" "${PDI_BINARY_DIR}")
set(DOXYGEN_INCLUDED_BY_GRAPH NO)
set(DOXYGEN_INLINE_SIMPLE_STRUCTS YES)
set(DOXYGEN_JAVADOC_AUTOBRIEF YES)
set(DOXYGEN_LAYOUT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/_template/layout.xml")
set(DOXYGEN_MACRO_EXPANSION YES)
set(DOXYGEN_PREDEFINED "PDI_EXPORT=" "PDI_NO_EXPORT=" "PDI_DEPRECATED_EXPORT=")
set(DOXYGEN_PROJECT_LOGO "${CMAKE_CURRENT_SOURCE_DIR}/_template/logo.png")
set(DOXYGEN_PROJECT_NUMBER "${PDI_VERSION}")
set(DOXYGEN_QT_AUTOBRIEF YES)
set(DOXYGEN_RECURSIVE NO)
set(DOXYGEN_SEARCHENGINE NO)
set(DOXYGEN_SHOW_FILES NO)
set(DOXYGEN_SHOW_INCLUDE_FILES NO)
set(DOXYGEN_SORT_MEMBER_DOCS NO)
set(DOXYGEN_SORT_MEMBERS_CTORS_1ST YES)
set(DOXYGEN_TOC_INCLUDE_HEADINGS 4)
set(DOXYGEN_TYPEDEF_HIDES_STRUCT YES)
set(DOXYGEN_UML_LIMIT_NUM_FIELDS 20)
set(DOXYGEN_UML_LOOK YES)
set(DOXYGEN_WARN_NO_PARAMDOC YES)
set(DOXYGEN_IMAGE_PATH
		"${CMAKE_CURRENT_SOURCE_DIR}/images"
		"${PDI_SOURCE_DIR}/../plugins/flowvr/images")
doxygen_add_docs(doxygen_doc
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
		"${PDI_BINARY_DIR}/fmoddir/pdif.h"
		"${PDI_SOURCE_DIR}/include/pdi.h"
		"${PDI_SOURCE_DIR}/include/pdi/"
		"${PDI_SOURCE_DIR}/../example/README.md"
		"${PDI_SOURCE_DIR}/../tutorial/README.md"
		"${PDI_SOURCE_DIR}/../plugins/decl_hdf5/README.md"
		"${PDI_SOURCE_DIR}/../plugins/flowvr/README.md"
		"${PDI_SOURCE_DIR}/../plugins/fti/README.md"
		"${PDI_SOURCE_DIR}/../plugins/decl_netcdf/README.md"
		"${PDI_SOURCE_DIR}/../plugins/mpi/README.md"
		"${PDI_SOURCE_DIR}/../plugins/trace/README.md"
		"${PDI_SOURCE_DIR}/../plugins/user_code/README.md"
		"${PDI_SOURCE_DIR}/../plugins/pycall/README.md"
		"${PDI_SOURCE_DIR}/../plugins/serialize/README.md"
		"${PDI_SOURCE_DIR}/../plugins/set_value/README.md"
		"${PDI_SOURCE_DIR}/../plugins/json/README.md")
add_subdirectory(First_steps)
add_dependencies(doc doxygen_doc)
install(DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/html/" DESTINATION "${CMAKE_INSTALL_DOCDIR}/html")
