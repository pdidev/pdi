#=============================================================================
# Copyright (C) 2018-2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# * Neither the name of CEA nor the names of its contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#=============================================================================


cmake_minimum_required(VERSION 3.5)
project(pdi_flowvr_plugin_core LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

# FlowVR
find_package(FlowVR REQUIRED)

# The plugin
add_library(pdi_flowvr_plugin MODULE src/flowvr.cxx
		src/component.cxx
		src/module.cxx
		src/ports/input_port.cxx
		src/ports/output_port.cxx
		src/ports/port.cxx
		src/ports/payloads/input_payload.cxx
		src/ports/payloads/output_payload.cxx
		src/ports/payloads/button_event/input_payload_button_event.cxx
		src/ports/payloads/button_event/output_payload_button_event.cxx
		src/ports/payloads/button_event/payload_button_event.cxx
		src/ports/payloads/chunk/input_payload_chunk.cxx
		src/ports/payloads/chunk/output_payload_chunk.cxx
		src/ports/payloads/chunk/payload_chunk.cxx
		src/ports/payloads/data/input_payload_data.cxx
		src/ports/payloads/data/output_payload_data.cxx
		src/ports/payloads/data/payload_data.cxx
		src/ports/payloads/mouse_event/input_payload_mouse_event.cxx
		src/ports/payloads/mouse_event/output_payload_mouse_event.cxx
		src/ports/payloads/mouse_event/payload_mouse_event.cxx
		src/ports/stamps/stamp_base.cxx
		src/ports/stamps/stamp.cxx
		src/ports/stamps/desc/stamp_desc_float_array.cxx
		src/ports/stamps/desc/stamp_desc_float.cxx
		src/ports/stamps/desc/stamp_desc_int_array.cxx
		src/ports/stamps/desc/stamp_desc_int.cxx
		src/ports/stamps/desc/stamp_desc_string.cxx
		src/ports/stamps/expr/stamp_expr_float_array.cxx
		src/ports/stamps/expr/stamp_expr_float.cxx
		src/ports/stamps/expr/stamp_expr_int_array.cxx
		src/ports/stamps/expr/stamp_expr_int.cxx
		src/ports/stamps/expr/stamp_expr_string.cxx
		src/traces/trace.cxx
		src/traces/traces_impl/trace_base.cxx
		src/traces/traces_impl/trace_float.cxx
		src/traces/traces_impl/trace_int.cxx
		src/traces/traces_impl/trace_string.cxx)

target_link_libraries(pdi_flowvr_plugin PDI::PDI_plugins ${FLOWVR_base_LIBRARY} ${FLOWVR_ftl_LIBRARY} ${FLOWVR_mod_LIBRARY} ${CMAKE_THREAD_LIBS_INIT})
target_include_directories(pdi_flowvr_plugin PUBLIC 
	"${CMAKE_CURRENT_LIST_DIR}/include"
	${FLOWVR_INCLUDE_DIR})

set_target_properties(pdi_flowvr_plugin PROPERTIES
		CXX_STANDARD 14
		CXX_STANDARD_REQUIRED TRUE
		CXX_VISIBILITY_PRESET hidden)

# Indentation
if("${BUILD_INDENT}")
	Astyle_add_indent(indent TEST
		OPTIONS_FILE ../formating.astyle
		include/*.h
		include/ports/*.h
		include/ports/payloads/*.h
		include/ports/payloads/button_event/*.h
		include/ports/payloads/chunk/*.h
		include/ports/payloads/data/*.h
		include/ports/payloads/mouse_event/*.h
		include/ports/stamps/*.h
		include/ports/stamps/desc/*.h
		include/ports/stamps/expr/*.h
		src/*.cxx
		src/ports/*.cxx
		src/ports/payloads/*.cxx
		src/ports/payloads/button_event/*.cxx
		src/ports/payloads/chunk/*.cxx
		src/ports/payloads/data/*.cxx
		src/ports/payloads/mouse_event/*.cxx
		src/ports/stamps/*.cxx
		src/ports/stamps/desc/*.cxx
		src/ports/stamps/expr/*.cxx)
endif()

# Installation
# installation
install(TARGETS pdi_flowvr_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)
