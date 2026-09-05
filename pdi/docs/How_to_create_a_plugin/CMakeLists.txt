#=============================================================================
# Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
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

# These plugins illustrate the syntax of the plugin API, compiling them is what
# ensures the documentation keeps matching that API.

foreach(DOC_PLUGIN IN ITEMS minimal_plugin posix)
	add_library(doc_${DOC_PLUGIN} MODULE "${DOC_PLUGIN}.cxx")
	target_link_libraries(doc_${DOC_PLUGIN} PDI::PDI_plugins)
endforeach()

# `example_plugin' is the plugin the tutorial builds up step by step and the
# one whose output the documentation shows, so it is actually loaded and run.
# It has to be named after the `example' plugin the specification tree asks for.
add_library(doc_example_plugin MODULE example_plugin.cxx)
target_link_libraries(doc_example_plugin PDI::PDI_plugins)
set_target_properties(doc_example_plugin PROPERTIES
		OUTPUT_NAME pdi_example_plugin
		PREFIX lib)

add_executable(doc_example_use example_use.cxx)
target_link_libraries(doc_example_use PDI::PDI_C)

if("${BUILD_TESTING}")
	add_test(NAME How_to_create_a_plugin_example_use
			COMMAND "${CMAKE_COMMAND}"
				"-DEXAMPLE=$<TARGET_FILE:doc_example_use>"
				"-DEXPECTED=${CMAKE_CURRENT_SOURCE_DIR}/example_use.output"
				"-DPLUGIN_PATH=$<TARGET_FILE_DIR:doc_example_plugin>"
				"-DWORKDIR=${CMAKE_CURRENT_SOURCE_DIR}"
				-P "${CMAKE_CURRENT_SOURCE_DIR}/../run_and_compare.cmake")
endif()
