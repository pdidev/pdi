#=============================================================================
# Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2020-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
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


# The plugin

add_library(pdi_set_value_plugin MODULE set_value.cxx
										trigger.cxx
										logger_operation.cxx
										operation.cxx
										set_operation.cxx
										share_operation.cxx
										expose_operation.cxx
										event_operation.cxx
										release_operation.cxx)
target_link_libraries(pdi_set_value_plugin PUBLIC PDI::PDI_plugins spdlog::spdlog)
set_target_properties(pdi_set_value_plugin PROPERTIES CXX_VISIBILITY_PRESET hidden)
set_target_properties(pdi_set_value_plugin PROPERTIES LIBRARY_OUTPUT_DIRECTORY "${PDI_PLUGIN_OUTPUT_DIRECTORY}")

# installation
install(TARGETS pdi_set_value_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()
