#=============================================================================
# Copyright 2015 CEA, Julien Bigot <julien.bigot@cea.fr>
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

cmake_minimum_required(VERSION 3.1)
# enable C++ to work-around cmake issue #15829 https://cmake.org/Bug/view.php?id=15829
project(FTI_pdi LANGUAGES C CXX Fortran)

option(USE_SYSTEM_FTI
		"Use an already installed fti instead of building the embedded one"
		OFF
)

if("${USE_SYSTEM_FTI}")
	find_package(fti)
else()
	add_subdirectory(../../vendor/fti fti EXCLUDE_FROM_ALL)
endif()

include_directories(${FTI_INCLUDE_PATH})

add_library(fti_plugin
		fti_plugin.c
)
set_target_properties(fti_plugin PROPERTIES C_VISIBILITY_PRESET hidden)
target_link_libraries(fti_plugin pdi fti.shared)

install(TARGETS fti_plugin EXPORT fti_pdi_pluginTargets
		LIBRARY DESTINATION lib INCLUDES DESTINATION include
		ARCHIVE DESTINATION lib INCLUDES DESTINATION include
)
